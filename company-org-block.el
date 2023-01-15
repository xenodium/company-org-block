;;; company-org-block.el --- Org blocks company backend -*- lexical-binding: t; -*-

;; Author: Alvaro Ramirez
;; Package-Requires: ((emacs "25.1") (company "0.8.0") (org "9.2.0"))
;; URL: https://github.com/xenodium/company-org-block
;; Version: 0.5

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; `company-complete' org blocks using "<" as a trigger.
;;
;; To enable, add `company-org-block' to `company-backends'.
;;
;; Configure edit style via `company-org-block-edit-style'.
;;
;; Completion candidates are drawn from `org-babel-load-languages'.

;;; Code:

(require 'company)
(require 'map)
(require 'org)
(require 'org-element)
(require 'seq)

(defgroup company-org-block nil
  "Completion back-end for org blocks."
  :group 'company)

(defcustom company-org-block-complete-at-bol t
  "If t, detect completion only at the beginning of lines."
  :type 'boolean)

(defcustom company-org-block-explicit-lang-defaults t
  "If t, insert org-babel-default-header-args:lang into block header."
  :type 'boolean)

(defcustom company-org-block-edit-style 'auto
  "Customize how to enter edit mode after block is inserted."
  :type '(choice
          (const :tag "inline: no edit mode invoked after insertion" inline)
          (const :tag "prompt: ask before entering edit mode" prompt)
          (const :tag "auto: automatically enter edit mode" auto)))

(defcustom company-org-block-auto-indent t
  "If t, automatically indent source block using `org-indent-line'.
Otherwise, insert block at cursor position."
  :type 'boolean)

(defvar company-org-block--regexp "<\\([^ ]*\\)")

;;;###autoload
(defun company-org-block (command &optional arg &rest _ignored)
  "A company completion backend for org blocks.

COMMAND and ARG are sent by company itself."
  (interactive (list #'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-org-block))
    (prefix (when (derived-mode-p 'org-mode)
              (company-org-block--grab-symbol-cons)))
    (candidates (company-org-block--candidates arg))
    (doc-buffer (company-org-block--doc-buffer arg))
    (post-completion
     (company-org-block--expand arg))))

(defun company-org-block--candidates (prefix)
  "Return a list of org babel languages matching PREFIX."
  (seq-filter (lambda (language)
                (string-prefix-p prefix language))
              ;; Flatten `org-babel-load-languages' and
              ;; `org-structure-template-alist', join, and sort.
              (seq-sort
               #'string-lessp
               (seq-uniq
                (append
                 (company-org-block--languages)
                 (company-org-block--templates)
                 (company-org-block--languages-from-extensions))))))

(defun company-org-block--languages ()
  "Get language names."
  (mapcar #'prin1-to-string
          ;; Filter out non-symbols.
          (seq-filter
           (lambda (item)
             (symbolp item))
           (map-keys org-babel-load-languages))))

(defun company-org-block--templates ()
  "Get template names."
  ;; Filter out non-strings (pre org 9.2 templates)
  ;; https://github.com/xenodium/company-org-block/issues/7
  (seq-filter
   #'stringp
   (map-values org-structure-template-alist)))

(defun company-org-block--languages-from-extensions ()
  "Get language names from extensions."
  (seq-filter
   #'stringp
   (map-keys org-babel-tangle-lang-exts)))

(defun company-org-block--template-p (template)
  "Check if there is a TEMPLATE available for completion."
  (seq-contains (map-values org-structure-template-alist)
                template))

(defun company-org-block--doc-buffer (candidate)
  "Return doc for CANDIDATE."
  (when (string-equal candidate "src")
    (setq candidate ""))
  (let ((company-org-block-edit-style 'inline))
    (company-doc-buffer
     (with-temp-buffer
       (insert "<" candidate)
       (company-org-block--expand candidate)
       (buffer-string)))))

(defun company-org-block--expand (insertion)
  "Replace INSERTION with generated source block."
  (delete-region (point) (- (point) (1+ ;; Include "<" in length.
                                     (length insertion))))
  ;; If < trigger generated a matching >, delete it.
  (when (looking-at ">")
    (delete-char 1))
  (cond ((string-equal insertion "src")
         ;; src templates have no associated language. Ask user for one.
         (company-org-block--wrap-point (format "src %s%s"
                                                (read-string "Language: ")
                                                (if company-org-block-explicit-lang-defaults
                                                    (company-org-block--lang-header-defaults insertion)
                                                  ""))
                                        "src"))
        ((company-org-block--template-p insertion)
         (company-org-block--wrap-point insertion
                                        ;; May be multiple words.
                                        ;; Take the first one.
                                        (nth 0 (split-string insertion))))
        (t
         (company-org-block--wrap-point (format "src %s%s"
                                                insertion
                                                (if company-org-block-explicit-lang-defaults
                                                    (company-org-block--lang-header-defaults insertion)
                                                  ""))
                                        "src"))))

(defun company-org-block--wrap-point (begin end)
  "Wrap point with block using BEGIN and END.  For example:
#+begin_BEGIN
  |
#+end_END"
  (when company-org-block-auto-indent
    (org-indent-line))
  (insert (format "#+begin_%s\n\n" begin))
  (insert (format "#+end_%s" end))
  (beginning-of-line)
  (org-indent-line)
  (line-move -1)
  (insert (make-string org-edit-src-content-indentation ?\s))
  (cond ((and (eq company-org-block-edit-style 'auto)
              (company-org-block--edit-src-code-p))
         ;; Only enter major mode if there's a language recognized for it.
         (when (org-element-property :language (org-element-at-point))
           (org-edit-src-code)))
        ((and (eq company-org-block-edit-style 'prompt)
              (company-org-block--edit-src-code-p)
              (yes-or-no-p "Edit now?"))
         (org-edit-src-code))))

(defun company-org-block--edit-src-code-p ()
  "Return t if `edit-src-code' can edit in a separate major mode."
  (memq (org-element-type (org-element-at-point))
        '(example-block src-block)))

(defun company-org-block--grab-symbol-cons ()
  "Return cons with symbol and t whenever prefix of < is found.
For example: \"<e\" -> (\"e\" . t)"
  (when (looking-back (if company-org-block-complete-at-bol
                          (concat "^[[:space:]]*" company-org-block--regexp)
                        company-org-block--regexp)
                      (line-beginning-position))
    (cons (match-string-no-properties 1) t)))

(defun company-org-block--lang-header-defaults (lang)
  "Resolve and concatenate all header defaults for LANG.

For example: \"python\" resolves to:

\((:exports . \"both\")
  (:results . \"output\"))

and returns:

\" :exports both :results output\""
  (let ((lang-headers-var (intern
                           (concat "org-babel-default-header-args:" lang))))
    (if (boundp lang-headers-var)
        (seq-reduce (lambda (value element)
                      (format "%s %s %s"
                              value
                              (car element)
                              (cdr element)))
                    (eval lang-headers-var t) "")
      "")))

(provide 'company-org-block)

;;; company-org-block.el ends here
