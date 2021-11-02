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

;; Commentary:

;;; Code:

(require 'ido)
(require 'projectile)

(require 'idee-eshell)
(require 'idee-vars)
(require 'idee-views)

(defun ide-project-new()
  "Create a new project."
  (interactive)
  (let ((factory (ide-project-factory-select)))
    (funcall (ide-project-factory-func factory))
    ;; Make sure we are pointing to a non ide buffer.
    (idee-jump-to-non-ide-window)))

(defun ide-module-new()
  "Create a new module."
  (interactive)
  (let ((factory (ide-project-factory-select)))
    (funcall (ide-project-factory-func factory) 'ide-module-create-with-shell)
    ;; Make sure we are pointing to a non ide buffer.
    (idee-jump-to-non-ide-window)))

(defun ide-project-grep()
  "Grep."
  (interactive)
  (when (require 'helm-ag nil t)
    (let ((buf (buffer-name (window-buffer))))
      (helm-do-ag (projectile-project-root))
      (other-window 1)
      (while (and
              (not (equal "*grep*" (buffer-name (window-buffer))))
              (not (equal buf (buffer-name (window-buffer)))))
        (other-window 1)))))


;;;###autoload (autoload 'ide-project-factory-register "idee-projects")
(defmacro ide-project-factory-register (project-factory)
  "Register a PROJECT-FACTORY."
  (list 'push project-factory 'ide-project-factories-list))

(defun ide-project-root-dir (&optional f)
  "Find the directory of the module that owns the source file F."
  (let ((current-dir (f-full (if f f default-directory))))
    (while (not (ide-project-root-dir-p current-dir))
      (setq current-dir (idee-parent-dir current-dir)))
    current-dir))

(defun ide-project-root-dir-p (f)
  "Return non-nil if F is a module directory."
  (cond
   ((seq-filter 'file-exists-p (seq-map (lambda (p) (concat f p)) ide-project-root-markers)) t)
   ((and (ide-module-root-dir-p f) (not (ide-module-root-dir-p (idee-parent-dir f)))) t)
   (t nil)))


(defun ide-module-root-dir (&optional f)
  "Find the directory of the module that owns the source file F."
  (let ((current-dir (f-full (if f f default-directory))))
    (while (not (ide-module-root-dir-p current-dir))
      (setq current-dir (idee-parent-dir current-dir)))
    current-dir))

(defun ide-module-root-dir-p (f)
  "Return non-nil if F is a module directory."
  (if (seq-filter 'file-exists-p (seq-map (lambda (p) (concat f p)) ide-module-root-markers))
      t
    nil))

(defun ide-project-dir-select()
 "Select a new project directory."
  (interactive)
  (let ((project-dir (car (find-file-read-args "Select project directory:" nil))))
    (make-directory project-dir t)
    project-dir))

(defun ide-project-factory-select()
  "Select a project factory from the list of registered factories."
  (let ((factory (projectile-completing-read "Select project type:"
                                             (mapcar 'ide-project-factory-entry ide-project-factories-list))))

    (car (seq-filter
          (lambda (f)
            (equal (ide-project-factory-name f) (car (split-string factory " ")))) ide-project-factories-list))))

(defun ide-project-factory-entry (f)
  "Create an entry for the specified project factory F."
  (concat (ide-project-factory-name f) " - " (ide-project-factory-description f)))

(defun ide-project-create-with-shell (path &rest commands)
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

(defun ide-module-create-with-shell (path &rest commands)
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
    (idee-switch-cli-on)
    (idee-eshell-project-command-enqueue commands)
    (idee-jump-to-non-ide-window)))

(defun idee-buffers-revert-visible-dired ()
  "Revert all visible dired buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (let* ((name (buffer-name buffer))
           (visible (get-buffer-window name)))
      (if visible
          (progn
            (set-buffer buffer)
            (when (derived-mode-p 'dired-mode) (revert-buffer)))))))

;;;###autoload
(defun ide-project-close-buffers (&optional project-dir)
  (interactive)
  (let* ((project-dir (or project-dir (projectile-project-root)))
         (project (projectile-ensure-project (or project-dir (projectile-project-root))))
         (project-name (projectile-project-name project))
         (project-dir-name (file-name-nondirectory (directory-file-name (file-name-directory project-dir)))))
       (when project-dir
         (progn
           (setq projectile-project-root project-dir)
           (projectile-kill-buffers)
           ;; Kill all buffers containing the PROJECT-DIR.
           (when (get-buffer project-dir) (kill-buffer project-dir))))

       (dolist (buffer (buffer-list))
         (let* ((name (buffer-name buffer))
                (file-name (buffer-file-name buffer)))
           (cond
            ((and project-dir (cl-search project-dir name) (kill-buffer name)))
            ((and project-dir (cl-search project-dir file-name) (kill-buffer name)))
            ((and project-name (cl-search project-name name) (kill-buffer name)))
            ((and project-dir-name (eq project-dir-name name) (kill-buffer name)))
            ((cl-search "*helm-ag*" name) (kill-buffer name))
            ((cl-search "*grep*" name) (kill-buffer name)))))))

(defun ide-project-close-other-buffers (&optional project-dir)
  (interactive)
  (let* ((project-dir (or project-dir (projectile-project-root)))
         (project (projectile-ensure-project (or project-dir (projectile-project-root))))
         (project-name (projectile-project-name project))
         (project-dir-name (file-name-nondirectory (directory-file-name (file-name-directory project-dir)))))
       (when project-dir
         (progn
           (dolist (buffer (buffer-list))
             (let* ((name (buffer-name buffer))
                    (file-name (buffer-file-name buffer)))
               (when (not (projectile-project-buffer-p buffer project-dir))
                 (message "Killing buffer: %s that does not belong to project: %s." name project-dir)
                 (kill-buffer name))))))))

(defun ide-project-init (&optional path)
  (interactive)
  "Initialize project."
  (let* ((path (or path (or (projectile-project-root) default-directory)))
         (name (or (projectile-project-name)  (file-name-nondirectory (directory-file-name path))))
         (info (alist-get (intern name) ide-project-info-alist)))

    (when (not info)
      (setq info (make-ide-project-info
                   :name name
                   :path path))
      (add-to-list 'ide-project-info-alist `(,(intern name) . ,info)))
    info))

(defun ide-project-version-get ()
  "Return the project version variable."
  (let ((info (ide-project-init)))
    (if info (ide-project-info-version info) nil)))

(defun ide-project-version-set (version)
  "Set the project VERSION."
  (let ((name (ide-project-name-get))
        (info (ide-project-init)))
    (when info
      (setf (ide-project-info-version info) version)
      (setq ide-project-info-alist (delq (assoc (intern name) ide-project-info-alist) ide-project-info-alist))
      (add-to-list 'ide-project-info-alist `(,(intern name). ,info)))))

(defun ide-project-name-get ()
  "Return the project name variable."
  (let ((info (ide-project-init)))
    (if info (ide-project-info-name info) nil)))

(defun ide-project-name-set (name)
  "Set the project NAME."
  (let ((info (ide-project-init)))
    (when info (setf (ide-project-info-name info) name))))

(defun ide-project-property-get (key)
  "Return the project name property with KEY."
  (let ((info (ide-project-init)))
    (if info
        (alist-get (intern key) (ide-project-info-properties info))
      nil)))

(defun ide-project-property-set (key value)
  "Set the project VALUE for KEY."
  (let* ((name (ide-project-name-get))
        (info (ide-project-init))
        (props (ide-project-info-properties info)))
    (when info
      (setq props (delq (assoc key props) props))
      (add-to-list 'props `(,(intern key) . ,value))
      (setf (ide-project-info-properties info) props)
      (setq ide-project-info-alist (delq (assoc (intern name) ide-project-info-alist) ide-project-info-alist))
      (add-to-list 'ide-project-info-alist `(,(intern name) . ,info)))))

;;
;; Initialization
;;
(defun ide-project-initialize ()
  "Initialize idee project.
   When called this function will look at the project root for an elisp script called .idee/init.el and will load it if present."
  (interactive)
  (let* ((root-dir (ide-project-root-dir (buffer-file-name)))
         (conf-dir (concat (file-name-as-directory root-dir) ide-project-conf-dir))
         (init-el (concat (file-name-as-directory conf-dir) "init.el")))
    (when (file-exists-p init-el) (load-file init-el))))

;;;###autoload
(defun ide-project-setup ()
  "Initialize idee projects."
  (add-hook 'projectile-after-switch-project-hook 'ide-project-initialize))


(provide 'idee-projects)
;;; idee-projects.el ends here
