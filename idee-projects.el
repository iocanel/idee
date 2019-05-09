;;;; idee-projects.el --- Project Factories

;; Copyright (C) 2018 Ioannis Canellos
;;     
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;         http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;; Commentary:

;;; Code:

(require 'ido)
(require 'projectile)
(require 'idee-views)

(cl-defstruct idee-project-factory
  name
  description
  func)

(defconst idee-cask-project-factory (make-idee-project-factory
  :name "Cask"
  :description "Create an elisp project based on Cask."
  :func ()))

(defvar idee-project-factories-list `(,idee-cask-project-factory))

(defconst idee-project-root-markers '(".idee" ".projectile" ".git"))

(defun idee-project-root-dir (&optional f)
  "Find the directory of the module that owns the source file F."
  (let ((current-dir (f-full (if f f default-directory))))
    (while (not (idee-project-root-dir-p current-dir))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    current-dir))

(defun idee-project-root-dir-p (f)
  "Return non-nil if F is a module directory."
  (if (seq-filter 'file-exists-p (seq-map (lambda (p) (concat f p)) idee-project-root-markers))
      t
    nil))

(defun idee-new-project-function()
  "Create a new project."
  (interactive)
  (let ((factory (idee--select-project-factory)))
    (funcall (idee-project-factory-func factory))
    ;; Make sure we are pointing to a non ide buffer.
    (idee-jump-to-non-ide-window)))

(defun idee--select-new-project-dir()
 "Select a new project directory."
  (interactive)
  (let ((project-dir (car (find-file-read-args "Select project directory:" nil))))
    (make-directory project-dir t)
    project-dir))

(defun idee--select-project-factory()
  "Select a project factory from the list of registered factories."
  (let ((factory (projectile-completing-read "Select project type:"
                                             (mapcar 'idee--project-factory-entry idee-project-factories-list))))

    (car (seq-filter
          (lambda (f)
            (equal (idee-project-factory-name f) (car (split-string factory " ")))) idee-project-factories-list))))

(defun idee--project-factory-entry (f)
  "Create an entry for the specified project factory F."
  (concat (idee-project-factory-name f) " - " (idee-project-factory-description f)))

(defun idee-create-project-with-shell (path &rest commands)
  "Create a new project with in the specified PATH and the specified COMMANDS."
  (let ((dired-auto-revert-buffer t))
    (make-directory path t)
    (setq default-directory path)
    (shell-command "git init")
    (shell-command "touch .projectile")
    (projectile-add-known-project path)
    (setq projectile-project-root path)
    (projectile-switch-project-by-name path)
    (treemacs-add-project path)
    (dired path)
    (auto-revert-mode 1)
    (idee-ide-view)
    (idee-switch-cli-on)
    (idee-refresh-view)
    (idee-eshell-project-command-enqueue commands)
    (idee-jump-to-non-ide-window)))

(defun idee-revert-visible-dired-buffers ()
  "Revert all visible dired buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (let* ((name (buffer-name buffer))
           (visible (get-buffer-window name)))
      (if visible
          (progn
            (set-buffer buffer)
            (when (derived-mode-p 'dired-mode) (revert-buffer)))))))

(provide 'idee-projects)
;;; idee-projects.el ends here
