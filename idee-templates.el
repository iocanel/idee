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
(require 'idee-headers)

(defconst idee-source-dir
   ;; If idee is installed using `straight-use-pacakge` we need to refer to the repo and not the build output.
   ;; TODO: have these get downloaded from github.
   (replace-regexp-in-string (regexp-quote "straight/build/idee") "straight/repos/idee"
                             (file-name-directory
                              ;; Copied from ‘yasnippet-snippets’ that copied from ‘f-this-file’ from f.el.
                              (cond
                               (load-in-progress load-file-name)
                               ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
                                byte-compile-current-file)
                               (:else (buffer-file-name))))))

(defconst idee-templates-source-dir (f-join idee-source-dir "templates") "The idee source template directory.")
(defconst idee-snippets-source-dir (f-join idee-source-dir "snippets") "The idee source snippet directory.")
(defconst idee-headers-source-dir (f-join idee-source-dir "headers") "The idee source header directory.")

(defconst idee-emacs-templates-dir (f-join idee-resources-dir "templates") "The directory where template files are stored.")
(defconst idee-emacs-snippets-dir (f-join idee-resources-dir "snippets") "The directory where snippet files are stored.")
(defconst idee-emacs-headers-dir (f-join idee-resources-dir "headers") "The directory where header files are stored.")

;;
;; Initialization
;;
(when (not (file-exists-p idee-resources-dir)) (mkdir idee-resources-dir t))

(when (and
       (file-exists-p idee-templates-source-dir)
       (not (file-exists-p idee-emacs-templates-dir))) (copy-directory idee-templates-source-dir idee-emacs-templates-dir))

(when (and
       (file-exists-p idee-snippets-source-dir)
       (not (file-exists-p idee-emacs-snippets-dir))) (copy-directory idee-snippets-source-dir idee-emacs-snippets-dir))

(when (not (file-exists-p idee-emacs-templates-dir)) (mkdir idee-emacs-templates-dir))
(when (not (file-exists-p idee-emacs-snippets-dir)) (mkdir idee-emacs-snippets-dir))

(add-to-list 'yas-snippet-dirs idee-emacs-templates-dir)
(add-to-list 'yas-snippet-dirs idee-emacs-snippets-dir)

(yas-compile-directory idee-emacs-templates-dir)
(yas-reload-all)

;;
;; State
;;
(defvar idee-type-modes-alist '() "Association list for extension to mode.")
(setq idee-type-modes-alist '(
                                ("el" . "emacs-lisp-mode")
                                ("org" . "org-mode")
                                ("md" . "markdown-mode")
                                ("java" . "java-mode")
                                ("json" . "json-mode")
                                ("js" . "js2-mode")
                                ("ts" . "typescript-mode")
                                ("py" . "python-mode")
                                ("go" . "go-mode")
                                ("cl" . "clojure-mode")
                                ("kt" . "kotlin-mode")
                                ("groovy" . "groovy-mode")
                                ("html" . "html-mode")
                                ("yml" . "yaml-mode")
                                ("yaml" . "yaml-mode")
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
         (mode-path (file-name-as-directory (concat (file-name-as-directory idee-emacs-templates-dir) mode)))
         (filetypes (remove-if (lambda (f) (string-prefix-p "." f)) (directory-files mode-path)))
         (definitions (mapcar (lambda (f) (idee-templates-parse-file (concat mode-path f))) filetypes))
         (names (mapcar (lambda (d) (idee-snippet-name d)) definitions))
         (name (projectile-completing-read "Select type:" names))
         (matches (remove-if-not (lambda (d) (equal name (idee-snippet-name d))) definitions))
         (key (car (mapcar 'idee-snippet-key matches))))

    (with-silent-modifications (write-file (buffer-file-name path)))
    (switch-to-buffer path)
    (insert key)
    (funcall (intern mode))
    (yas-expand)))

(defun idee-select-project-header-function ()
  "Select a header for the project from the existing selection of headers."
  (interactive)
  (let* ((headers (directory-files idee-emacs-headers-dir))
         (header (projectile-completing-read "Select header:" headers))
         (content (idee-read-and-eval-template (concat (file-name-as-directory idee-emacs-headers-dir) header))))
    (setq idee--current-header content)))


; (KEY TEMPLATE NAME CONDITION GROUP VARS LOAD-FILE KEYBINDING UUID)

(defun idee-templates-parse-file (f)
  "Parse template from file F."
  (with-temp-buffer
    (insert-file-contents f)
              (yas--parse-template f)))

(defun idee-snippet-name (definition)
  "Name of the snippet found in."
  (car (cdr (cdr definition))))

(defun idee-snippet-key (definition)
  "Key of the snippet found in."
  (car definition))

(provide 'idee-templates)
;;; idee-templates.el ends here
