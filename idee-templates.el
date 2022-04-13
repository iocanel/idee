;; idee-templates.el --- Emacs IDE - File templates.

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

(require 'yasnippet)
(require 'warnings)

(defconst idee/emacs-templates-dir (concat (file-name-as-directory idee/resources-dir) "templates") "The directory where template files are stored.")
(defconst idee/emacs-snippets-dir (concat (file-name-as-directory idee/resources-dir) "snippets") "The directory where snippet files are stored.")

;;
;; Template factories
;;
(cl-defstruct idee/template-factory
  mode
  description
  func)

(defvar idee/template-factory-list nil "A list of all the available template factories.")

;;;###autoload 
(defun idee/template-factory-register (template-factory)
  "Register a TEMPLATE_FACTORY."
  (setq idee/template-factory-list (delq template-factory idee/template-factory-list))
  (setq idee/template-factory-list (add-to-list  'idee/template-factory-list template-factory)))

;;;###autoload 
(defun idee/template-factory-get (mode)
  "Find the matching template factory for the specified MODE."
  (car (seq-filter (lambda (f) (equal mode (idee/template-factory-mode f))) idee/template-factory-list)))

(defun idee/template-create-from-buffer (&optional buffer-or-name name key)
  (let* ((buffer (if buffer-or-name (or (get-buffer buffer-or-name) (current-buffer)) (current-buffer)))
         (file-name (buffer-file-name buffer))
         (mode (buffer-local-value 'major-mode buffer))
         (template-factory (idee/template-factory-get mode))
         (func (if template-factory (idee/template-factory-func template-factory) nil)))

    (if (not func)
        (message "No template factory found for mode: %s of file: %s" mode file-name) 
      (funcall func file-name name key))))


(defun idee/template-create()
  "Create a template from the current buffer."
  (interactive)
  (let* ((name (read-string "Template name: "))
         (key (read-string "Template key: ")))
  (idee/template-create-from-buffer (current-buffer) name key)))

;;
;; Utils
;;

(defun idee/template-source-dir ()
  "Find the templates source directory."
  (concat (file-name-as-directory (idee/source-dir)) "templates"))

; (KEY TEMPLATE NAME CONDITION GROUP VARS LOAD-FILE KEYBINDING UUID)
(defun idee/template-parse-from-file (f)
  "Parse template from file F."
  (with-temp-buffer
    (insert-file-contents f)
              (yas--parse-template f)))

(defun idee/snippet-source-dir ()
  "Find the snippets source directory."
  (concat (file-name-as-directory (idee/source-dir)) "snippets"))

(defun idee/snippet-name (definition)
  "Name of the snippet found in DEFINITION."
  (car (cdr (cdr definition))))

(defun idee/snippet-key (definition)
  "Key of the snippet found in DEFINITION."
  (car definition))

;;
;; Commands
;;
(defun idee/file-new()
  "Create an empty buffer."
  (interactive)
  (let* ((path (ido-find-file))
         (extension (file-name-extension (buffer-file-name path)))
         (mode (cdr (assoc extension idee/type-modes-alist)))
         (mode-path (file-name-as-directory (concat (file-name-as-directory idee/emacs-templates-dir) mode)))
         (filetypes (seq-filter (lambda (f) (not (string-prefix-p "." f))) (directory-files mode-path)))
         (definitions (mapcar (lambda (f) (idee/template-parse-from-file (concat mode-path f))) filetypes))
         (names (mapcar (lambda (d) (idee/snippet-name d)) definitions))
         (name (projectile-completing-read "Select type:" names))
         (matches (seq-filter (lambda (d) (equal name (idee/snippet-name d))) definitions))
         (key (car (mapcar 'idee/snippet-key matches))))

    (with-silent-modifications (write-file (buffer-file-name path)))
    (switch-to-buffer path)
    (insert key)
    (funcall (intern mode))
    (yas-expand)))


;;
;; Initialization
;;

;;;###autoload
(defun idee/template-load-from-project ()
  "Load project templates."
  (interactive)
  (let* ((root-dir (idee/project-root-dir (buffer-file-name)))
         (conf-dir (concat (file-name-as-directory root-dir) idee/project-conf-dir))
         (template-dir (concat (file-name-as-directory conf-dir) "templates")))
    (when (file-exists-p template-dir)
      (yas-load-directory template-dir)
      (yas-compile-directory template-dir))))

;;;###autoload
(defun idee/template-init ()
  "Initialize ide templates."
  (add-hook 'projectile-after-switch-project-hook 'idee/template-load-from-project)

  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (when (not (file-exists-p idee/resources-dir)) (mkdir idee/resources-dir t))

  (run-with-idle-timer 1 nil (lambda ()
                               (if (not (file-exists-p idee/emacs-templates-dir))
                                 (progn
                                   (copy-directory (idee/template-source-dir) idee/emacs-templates-dir)
                                   (run-with-idle-timer 1 nil (lambda () (progn
                                                                           (yas-compile-directory idee/emacs-templates-dir)
                                                                           (yas-load-directory idee/emacs-templates-dir)))))
                                 (yas-load-directory idee/emacs-templates-dir))))

  (run-with-idle-timer 1 nil (lambda ()
                               (when (not (file-exists-p idee/emacs-snippets-dir))
                                 (progn
                                   (copy-directory (idee/snippet-source-dir) idee/emacs-snippets-dir)
                                   (run-with-idle-timer 1 nil (lambda () (progn
                                                                           (yas-compile-directory idee/emacs-snippets-dir)
                                                                           (yas-load-directory idee/emacs-snippets-dir)))))
                                 (yas-load-directory idee/emacs-templates-dir))))

  (run-with-idle-timer 1 nil (lambda ()
                               (when (not (file-exists-p idee/emacs-headers-dir)) (copy-directory (idee/header-source-dir) idee/emacs-headers-dir))))

  (add-to-list 'yas-snippet-dirs idee/emacs-templates-dir)
  (add-to-list 'yas-snippet-dirs idee/emacs-snippets-dir))

(provide 'idee-templates)
;;; idee-templates.el ends here
