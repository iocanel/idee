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
(require 'idee-vars)

;;;###autoload (autoload 'idee-register-project-factory "idee-projects")
(defmacro idee-register-project-factory (project-factory)
  "Register a PROJECT-FACTORY."
  (list 'push project-factory 'idee-project-factories-list))

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

(defun idee-new-module-function()
  "Create a new module."
  (interactive)
  (let ((factory (idee--select-project-factory)))
    (funcall (idee-project-factory-func factory) 'idee-create-module-with-shell)
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
    (idee-jump-to-non-ide-window)
    (projectile-add-known-project path)
    (setq projectile-project-root path)
    (projectile-switch-project-by-name path)
    (delete-other-windows-internal)
    (dired path)
    (auto-revert-mode 1)
    (idee-eshell-project-command-enqueue commands)
    (idee-jump-to-non-ide-window)))

(defun idee-create-module-with-shell (path &rest commands)
  "Create a new module with in the specified PATH and the specified COMMANDS."
  (let* ((project-path (projectile-project-root path))
        (relative-path (file-relative-name path project-path))
        (dired-auto-revert-buffer t))
    (idee-eshell-project-command-execute (format "cd %s" relative-path))
    (make-directory path t)
    (setq default-directory path)
    (idee-jump-to-non-ide-window)
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

(defun idee-close-project-buffers (&optional project-dir)
  (interactive)
  (when project-dir
    (progn
      (setq projectile-project-root project-dir)
      (projectile-kill-buffers)))
  ;; Kill all buffers containing the PROJECT-DIR.
  (when (get-buffer project-dir) (kill-buffer project-dir))
  (dolist (buffer (buffer-list))
    (let* ((name (buffer-name buffer)))
      (cond
       ((cl-search project-dir name) (kill-buffer name))
       ((cl-search "*helm-ag*" name) (kill-buffer name))
       ((cl-search "*grep*" name) (kill-buffer name))
       (t (message "buffer %s not a project buffer" name)))))

  ;; Kill all buffers containing the project name
  (let* ((project (projectile-ensure-project (or project-dir (projectile-project-root))))
         (project-name (projectile-project-name project)))
    (dolist (buffer (buffer-list))
      (let* ((name (buffer-name buffer)))
        (when (cl-search project-name name) (kill-buffer name))))))

(defun idee-project-info ()
  "Initialize project."
  (let* ((path (or (projectile-project-root) default-directory))
         (name (or (projectile-project-name)  (file-name-nondirectory (directory-file-name path))))
         (info (alist-get (intern name) idee-project-info-alist)))

    (when (not info)
      (setq info (make-idee-project-info
                   :name name
                   :path path))
      (add-to-list 'idee-project-info-alist `(,(intern name) . ,info)))
    info))

(defun idee-project-get-version ()
  "Return the project version variable."
  (let ((info (idee-project-info)))
    (if info (idee-project-info-version info) nil)))

(defun idee-project-set-version (version)
  "Set the project VERSION."
  (let ((name (idee-project-get-name))
        (info (idee-project-info)))
    (when info
      (setf (idee-project-info-version info) version)
      (setq idee-project-info-alist (delq (assoc (intern name) idee-project-info-alist) idee-project-info-alist))
      (add-to-list 'idee-project-info-alist `(,(intern name). ,info)))))

(defun idee-project-get-name ()
  "Return the project name variable."
  (let ((info (idee-project-info)))
    (if info (idee-project-info-name info) nil)))

(defun idee-project-set-name (name)
  "Set the project NAME."
  (let ((info (idee-project-info)))
    (when info (setf (idee-project-info-name info) name))))

(defun idee-project-get-property (key)
  "Return the project name property with KEY."
  (let ((info (idee-project-info)))
    (if info
        (alist-get (intern key) (idee-project-info-properties info))
      nil)))

(defun idee-project-set-property (key value)
  "Set the project VALUE for KEY."
  (let* ((name (idee-project-get-name))
        (info (idee-project-info))
        (props (idee-project-info-properties info)))
    (when info
      (setq props (delq (assoc key props) props))
      (add-to-list 'props `(,(intern key) . ,value))
      (setf (idee-project-info-properties info) props)
      (setq idee-project-info-alist (delq (assoc (intern name) idee-project-info-alist) idee-project-info-alist))
      (add-to-list 'idee-project-info-alist `(,(intern name) . ,info)))))


;;;###autoload
(defun idee--projects-init ()
  "Initialize idee projects."
  (add-to-list 'projectile-after-switch-project-hook 'idee-project-info))

(provide 'idee-projects)
;;; idee-projects.el ends here
