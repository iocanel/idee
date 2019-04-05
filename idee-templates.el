;;; idee-templates.el --- Emacs IDE - File templates.

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

;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(require 'yasnippet)

(defconst idee-templates-dir
  (expand-file-name
   "templates"
   (file-name-directory
    ;; Copied from ‘yasnippet-snippets’ that copied from ‘f-this-file’ from f.el.
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name))))))

;;
;; State
;;
(defconst idee-emacs-templates-dir (concat (file-name-as-directory idee-resources-dir) "templates") "The directory where template files are stored.")

(defvar idee-type-modes-alist '() "Association list for extension to mode.")
(setq idee-type-modes-alist '(
                                ("el" . "emacs-lisp-mode")
                                ("org" . "org-mode")
                                ("md" . "markdown-mode")
                                ("java" . "java-mode")
                                ("js" . "js2-mode")
                                ("ts" . "typescript-mode")
                                ("py" . "python-mode")
                                ("go" . "go-mode")
                                ("cl" . "clojure-mode")
                                ("kt" . "kotlin-mode")
                                ("groovy" . "groovy-mode")
                                ("html" . "html-mode")
                                ))
;;
;; Functions
;;
(defun idee-new-file-function()
  "Create an empty buffer."
  (interactive)
  (let* ((path (ido-find-file))
         (extension (file-name-extension (buffer-file-name path)))
         (mode (cdr (assoc extension idee-type-modes-alist)))
         (filetypes (directory-files (concat (file-name-as-directory idee-emacs-templates-dir) mode)))
         (filetype (projectile-completing-read "Select type of file:" filetypes)))
    (with-silent-modifications (write-file (buffer-file-name path)))
    (switch-to-buffer path)
    (insert filetype)
    (funcall (intern mode))
    (yas-expand)))

(defun idee-select-project-header-function ()
  "Select a header for the project from the existing selection of headers."
  (interactive)
  (let ((headers (directory-files idee-emacs-headers-dir))
        (header (projectile-completing-read "Select header:" headers))
        (content (idee-read-and-eval-template (concat (file-name-as-directory idee-emacs-headers-dir) header))))
        (idee--current-header content)))

(provide 'idee-templates)
;;; idee-templates.el ends here
