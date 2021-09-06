;;; idee-utils.el --- Emacs IDE Utilities.  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Ioannis Canellos

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;       http://www.apache.org/licenses/LICENSE-2.0

;;Unless required by applicable law or agreed to in writing, software
;;distributed under the License is distributed on an "AS IS" BASIS,
;;WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;See the License for the specific language governing permissions and
;;limitations under the License.

;;; Commentary:

;;; Code:
(require 'seq)
(require 'yasnippet)
(require 'editorconfig)
(require 'idee-vars)
(require 'projectile)

;;
;; File funtcionts
;;

;;;###autoload
(defun idee-read-file (f)
  "Read the content of file F."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

;;;###autoload
(defun idee-read-and-eval-template (f)
  "Read the template from F and evaluate quotes."
  (if (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (yas--restore-backquotes (yas--save-backquotes))
        (buffer-substring-no-properties
         (point-min)
         (point-max))) nil))

;;;###autoload
(defun idee-filesystem-root-p (f)
  "Check if file F is the filesystem root."
  (equal f "/"))

;;;###autoload
(defun idee-parent-dir (f)
  "Reuturn the parent dir of F."
  (file-name-directory (directory-file-name f)))

(defun idee-current-file-name ()
  "Return the filename of the current buffer"
  (file-name-nondirectory (buffer-file-name)))

;;
;; String Functions
;;

;;;###autoload
(defun idee-string-blank (string)
  "Return non-nil if STRING start is blank."
  (let* ((transformed
          (replace-in-string "\n" "" (replace-in-string "\r" "" (replace-in-string "\t" "" (string-trim string))))))
    (if (= 0 (length transformed)) t nil)))

(defun idee-string-up-to (string prefix)
  "Return t if STRING start with PREFIX."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))
(defun idee-starts-with (string prefix)
  "Return t if STRING start with PREFIX."
  (and string (string-match (rx-to-string `(: bos ,prefix) t) string)))

;;;###autoload
(defun idee-ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and string (string-match (rx-to-string `(: ,suffix eos) t) string)))

;;;###autoload
(defun idee-contains-string (item string)
  "Return t if STRING exists in the ITEM. ITEM may be a buffer a list or a string."
  (cond ((bufferp item) (with-current-buffer item (idee-buffer-contains-string string)))
        ((listp item) (member string item))
        ((stringp item) (string-match (regexp-quote string) item))
        (t nil)))

(defun idee-buffer-contains-string (string)
  "Return t if STRING exists in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward string nil t)))

;;
;; Original source: https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
;;
;;;###autoload
(defun idee-string-match-as-list (regexp string)
  "Get a list of all REGEXP matches in a STRING."
  (save-match-data
    (let ((matches)
          (index 0)
          (match " "))
      (string-match regexp string)
      (while match
        (setq match (match-string index string))
        (if match
            (progn
              (push match matches)
              (setq index (+ 1 index)))))
      matches)))


(defun idee-string-camelcase-split (s)
  "Splits a camel case S into a list of strings."
  (let ((case-fold-search nil))
    (seq-filter
     (lambda (x) (not (equal "" x))) ;;remove all empty strings (its usually the first)
     (split-string (replace-regexp-in-string "\\([A-Z]\\)" " \\1" s) " "))))


(defun idee-string-camelcase-to-kebabcase (s)
  "Converts S from camel case to kebab case."
  (if s
      (mapconcat #'downcase (idee-string-camelcase-split s) "-")
    nil))


;;
;; List functions
;;

;;;###autoload
(defun idee-strip-duplicates (list)
  "Strip duplicate items from LIST."
  (let ((new-list nil))
    (while list
      (when (and (car list) (not (member (car list) new-list)))
        (setq new-list (cons (car list) new-list)))
      (setq list (cdr list)))
    (nreverse new-list)))

;;
;; Buffers
;;

(defun idee-matching-buffer-names (regex)
  "Return a list of buffers names that matches the regexp"
    (seq-filter (lambda (b) (string-match regex b)) (mapcar 'buffer-name (buffer-list))))

(defun idee--item-to-kind (item)
  "Convert an ITEM to its kind."
  (cond
   ((listp item) "list")
   ((numberp item) "number")
   ((symbolp item) "symbol")
   ((stringp item) "string")
   ((arrayp item) "array")
   (t "other")))

;;;###autoload
(defun idee-http-post (url args callback)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (mapconcat (lambda (arg) (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg)))) args "&")))
    (url-retrieve url callback)))

;;;###autoload
(defun idee--point-beginning-of-line()
  "Return the point of the beginning of the current line."
  (save-excursion
    (beginning-of-line)
    (point)))

;;;###autoload
(defun idee--point-end-of-line()
  "Return the point of the end of the current line."
  (save-excursion
    (end-of-line)
    (point)))

;;;###autoload
(defun idee-indent-file (f)
  "Indent file F."
    (find-file f)
    (set-auto-mode t)
    (indent-region (point-min) (point-max))
    (write-file f))

;;;###autoload
(defun idee-indent-all-project-files()
  "Indend all files in the project."
  (interactive)
  (idee-visit-project-files 'idee-indent-file))

;;;###autoload
(defun idee-visit-project-files (visitor &optional dir)
  "Call VISITOR with all project files or DIR files."
  (let* ((current (or dir (projectile-project-root))))
    (dolist (extension (idee-source-file-extensions))
         (mapc (lambda (x) (funcall visitor x))
          (directory-files-recursively current (format "\\.%s$" extension))))))

;; Credits: https://emacs.stackexchange.com/questions/12613/convert-the-first-character-to-uppercase-capital-letter-using-yasnippet
;;;###autoload
(defun idee-capitalize-first(&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first) rest-str))))

(defun idee-as-code (item)
  "Display the item as elisp code."
  (cond ((stringp item) (format "\"%s\"" item))
        ((numberp item) (format "%s" item)) 
        ((sequencep item) (format "'%s" (mapcar 'idee-as-code item)))
        (t item)))

;;;###autoload
(defun idee-project-settings (settings-file)
  "Return the path of a local SETTINGS-FILE."
  (concat (file-name-as-directory (concat (projectile-project-root) ".idee")) settings-file))

(defun idee-project-settings-set (settings-file key value)
  "Add or replace a set statement inside the SETTINGS-FILE using the specified KEY and VALUE."
  (let* ((file (idee-project-settings settings-file))
         (settings-dir (file-name-as-directory (file-name-directory (directory-file-name file)))))
    (when (not (file-exists-p settings-dir)) (make-directory settings-dir t))
    (with-temp-buffer
      (when (file-exists-p file) (insert-file-contents file))
      (goto-char (point-min))
      (if (re-search-forward (regexp-quote key) nil t)
          (let* ((start (point))
                 (end start))
            (condition-case nil (forward-sexp) (error t))
            (setq end (point))
            (replace-region-contents (+ 1 start) end (lambda () (format "%s" value)))
            (write-file file nil))
        (progn
          (goto-char (point-max))
          (insert (format "(setq %s %s)" key value))
          (write-file file nil))))))
;;; Macros

;;;###autoload (autoload 'idee-with-project-settings "idee-utils")
(defmacro idee-with-project-settings (settings-file options &rest body)
  "Load a SETTINGS-FILE as local OPTIONS and evaluate BODY."
  (declare (indent 1) (debug t))
       `(let (,options)
          (let ((f (idee-project-settings ,settings-file)))
            (when (and f (file-exists-p f)) (load-file f))) ,@body))

;;;###autoload (autoload 'idee-toggle "idee-utils")
(defmacro idee-toggle (bool)
  "Toggle the specified BOOL variable."
  (list 'setq bool (list 'not bool)))

(defmacro idee-switch-on (bool)
  "Switch on the specified BOOL variable."
  (list 'setq bool nil))

(defmacro idee-switch-off (bool)
  "Switch off the specified BOOL variable."
  (list 'setq bool nil))


;;;###autoload (autoload 'idee-leader/set-key "idee-utils")
(defmacro idee-leader/set-key (key func &optional desc)
"Leader key function abstraction."
  (declare (indent 1) (debug t))
  `(cond
     ((fboundp 'evil-leader/set-key) (evil-leader/set-key ,key #',func))
     ((and (require 'core-keybinds nil t) (fboundp 'map!)) (map! :leader :prefix ,key :desc ,desc "" ',func))))

;;; Misc Functions
;;;###autoload
(defun idee-screenshot ()
  "Get a screenshot."
  (interactive)
  (shell-command "scrot -s '/home/iocanel/Photos/screenshots/%Y-%m-%d_%H:%M:%S_$wx$h.png'"))

;;;###autoload
(defun idee-git-checkout (repo target &optional dirs)
  "Checkout a git REPO into the TARGET dir.  Optionally only checkout one or more DIRS."
  (make-directory target t)
  (setq default-directory (file-name-as-directory target))
  (call-process-shell-command "git init")
  (call-process-shell-command (format "git remote add -f origin %s" repo))
  (if dirs
      (progn
      (call-process-shell-command "git config core.sparseCheckout true")
      (call-process-shell-command "echo > .git/info/sparse-checkout")
      (mapc (lambda (x) (call-process-shell-command (format "echo '%s' >> .git/info/sparse-checkout" x))) dirs)))
  (call-process-shell-command "git checkout master"))
 
  (provide 'idee-utils)
;;; idee-utils.el ends here
