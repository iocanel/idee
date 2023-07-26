;; idee-projects.el --- Project Factories -*- lexical-binding: t -*-

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

(require 'idee-vars)
(require 'idee-views)

(defcustom idee/project-fixed-buffer-name-list '("*grep*") "A list buffer names that should be considered project buffers" :group 'idee/project :type '(repeat string)) 

(defun idee/project-new()
  "Create a new project."
  (interactive)
  (let ((factory (idee/project-factory-select)))
    (funcall (idee/project-factory-func factory))
    ;; Make sure we are pointing to a non ide buffer.
    (idee/jump-to-non-idee/window)))

(defun idee/module-new()
  "Create a new module."
  (interactive)
  (let ((factory (idee/project-factory-select)))
    (funcall (idee/project-factory-func factory) 'idee/module-create-with-shell)
    ;; Make sure we are pointing to a non ide buffer.
    (idee/jump-to-non-idee/window)))

;;;###autoload
(defun idee/project-factory-register (project-factory)
  "Register a PROJECT-FACTORY."
  (when (not (member (idee/project-factory-name project-factory) (mapcar 'idee/project-factory-name idee/project-factories-list)))
    (setq idee/function-alist (delq (assoc 'idee/mode-tab-width-function idee/function-alist) idee/function-alist))
    (add-to-list 'idee/project-factories-list project-factory)))

(defun idee/project-root-dir (&optional f)
  "Find the directory of the module that owns the source file F."
  (let* ((current-dir (f-full (if f f default-directory)))
        (parent-dir (idee/parent-dir current-dir)))
    (cond ((equal "/" current-dir) nil)
          ((not parent-dir) nil)
          ((idee/project-root-dir-p current-dir) current-dir)
          (:default (idee/project-root-dir parent-dir)))))
           
(defun idee/project-root-dir-p (f)
  "Return non-nil if F is a module directory."
  (cond
   ((seq-filter 'file-exists-p (seq-map (lambda (p) (concat f p)) idee/project-root-markers)) t)
   ((and (idee/module-root-dir-p f) (not (idee/module-root-dir-p (idee/parent-dir f)))) t)
   (t nil)))

(defun idee/module-root-dir (&optional f)
  "Find the directory of the module that owns the source file F."
  (let* ((current-dir (f-full (if f f default-directory)))
        (parent-dir (idee/parent-dir current-dir)))
    (cond ((equal "/" current-dir) nil)
          ((not parent-dir) nil)
          ((idee/module-root-dir-p current-dir) current-dir)
          ((idee/project-root-dir-p current-dir) current-dir)
          (:default (idee/module-root-dir parent-dir)))))

(defun idee/module-root-dir-p (f)
  "Return non-nil if F is a module directory."
  (if (seq-filter 'file-exists-p (seq-map (lambda (p) (concat f p)) idee/module-root-markers))
      t
    nil))

(defun idee/project-dir-select()
 "Select a new project directory."
  (interactive)
  (let ((project-dir (car (find-file-read-args "Select project directory:" nil))))
    (make-directory project-dir t)
    project-dir))

(defun idee/project-factory-select()
  "Select a project factory from the list of registered factories."
  (let ((factory (completing-read "Select project type:"
                                             (mapcar 'idee/project-factory-entry idee/project-factories-list))))

    (car (seq-filter
          (lambda (f)
            (equal (idee/project-factory-name f) (car (split-string factory " ")))) idee/project-factories-list))))

(defun idee/project-factory-entry (f)
  "Create an entry for the specified project factory F."
  (concat (idee/project-factory-name f) " - " (idee/project-factory-description f)))

(defun idee/project-create-with-shell (path &rest commands)
  "Create a new project with in the specified PATH and the specified COMMANDS."
  (let ((dired-auto-revert-buffer t)
        (default-directory path))
    (make-directory path t)
    (shell-command "git init")
    (idee/jump-to-non-idee/window)
    (add-to-list 'project-list path)
    (project-switch-project path)
    (delete-other-windows-internal)
    (dired path)
    (auto-revert-mode 1)
    ;; So tools don't work great when files exist in the repo (e.g. npm)
    ;; To work around this, let's delete .project and recreate it afterwards.
    (dolist (c commands)
      (idee/shell-command-execute-in-project c))
    (idee/jump-to-non-idee/window)))

(defun idee/module-create-with-shell (path &rest commands)
  "Create a new module with in the specified PATH and the specified COMMANDS."
  (let* ((project-path (project-root (project-current nil path))
        (relative-path (file-relative-name path project-path))
        (dired-auto-revert-buffer t))
    (idee/shell-command-execute-in-project (format "cd %s" relative-path))
    (make-directory path t)
    (setq default-directory path)
    (idee/jump-to-non-idee/window)
    (dired path)
    (auto-revert-mode 1)
    (idee/switch-cli-on)
    (dolist (c commands)
      (idee/shell-command-execute-in-project c))
    (idee/jump-to-non-idee/window))))

(defun idee/buffers-revert-visible-dired ()
  "Revert all visible dired buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (let* ((name (buffer-name buffer))
           (visible (get-buffer-window name)))
      (if visible
          (progn
            (set-buffer buffer)
            (when (derived-mode-p 'dired-mode) (revert-buffer)))))))


(defun idee/project-buffer-p (buffer &optional project)
  "Checks if BUFFER belongs to the project."
  (let* ((name (buffer-name buffer))
         (file-name (buffer-file-name buffer)))
         (project (or project (project-current)))
         (project-dir (or project-dir (project-root project)))
         (project-name (project-name project))
         (projec-buffers (project-buffers project))
         (project-dir-name (file-name-nondirectory (directory-file-name (file-name-directory project-dir)))))
         
    (or (member buffer project-buffers) ;; known project buffer
        (and project-dir (cl-search project-dir file-name)) ;; the file literaly belongs to the project
        (name member idee/project-fixed-buffer-name-list)))

(defun idee/project-save-buffers ()
  "Save porject buvvers"
  (interactive)
  (dolist (buffer (project-buffers (project-current)))
    (with-current-buffer buffer
      (when (and (buffer-file-name) (member (buffer-file-name) project-files))
        (save-buffer)))))

;;;###autoload
(defun idee/project-close-buffers (&optional project-dir)
  (interactive)
  (let* ((project (project-current))
         (project-dir (or project-dir (project-root project)))
         (project-name (project-name project))
         (project-dir-name (file-name-nondirectory (directory-file-name (file-name-directory project-dir)))))

       (when project-dir
           (project-kill-buffers t)
           ;; Kill all buffers containing the PROJECT-DIR.
           (when (get-buffer project-dir) (kill-buffer project-dir)))

       (dolist (buffer (buffer-list))
         (when (idee/project-buffer-p buffer project) (kill-buffer buffer))))

(defun idee/project-close-other-buffers (&optional project-dir)
  (interactive)
  (let* ((project (project-current))
         (project-dir (or project-dir (project-root project)))
         (project-name (project-name project))
         (project-dir-name (file-name-nondirectory (directory-file-name (file-name-directory project-dir)))))
       (when project-dir
           (dolist (buffer (buffer-list))
               (when (not (idee/project-buffer-p buffer) project)
                 (message "Killing buffer: %s that does not belong to project: %s." name project-dir)
                 (kill-buffer name)))))))

(defun idee/project-create (&optional path)
  "Create project in the optionally specified PATH, existing project root or current directory."
  (interactive)
  (let* ((project (project-current nil))
         (path (or path (or (project-root project) default-directory)))
         (name (or (project-name project) (file-name-nondirectory (directory-file-name path))))
         (info (alist-get (intern name) idee/project-info-alist)))

    (when (not info)
      (setq info (make-idee/project-info
                   :name name
                   :path path))
      (add-to-list 'idee/project-info-alist `(,(intern name) . ,info)))
    info))

(defun idee/project-version-get ()
  "Return the project version variable."
  (let ((info (idee/project-create)))
    (if info (idee/project-info-version info) nil)))

(defun idee/project-version-set (version)
  "Set the project VERSION."
  (let ((name (idee/project-name-get))
        (info (idee/project-create)))
    (when info
      (setf (idee/project-info-version info) version)
      (setq idee/project-info-alist (delq (assoc (intern name) idee/project-info-alist) idee/project-info-alist))
      (add-to-list 'idee/project-info-alist `(,(intern name). ,info)))))

(defun idee/project-name-get ()
  "Return the project name variable."
  (let ((info (idee/project-create)))
    (if info (idee/project-info-name info) nil)))

(defun idee/project-name-set (name)
  "Set the project NAME."
  (let ((info (idee/project-create)))
    (when info (setf (idee/project-info-name info) name))))

(defun idee/project-property-get (key)
  "Return the project name property with KEY."
  (let ((info (idee/project-create)))
    (if info
        (alist-get (intern key) (idee/project-info-properties info))
      nil)))

(defun idee/project-property-set (key value)
  "Set the project VALUE for KEY."
  (let* ((name (idee/project-name-get))
        (info (idee/project-create))
        (props (idee/project-info-properties info)))
    (when info
      (setq props (delq (assoc key props) props))
      (add-to-list 'props `(,(intern key) . ,value))
      (setf (idee/project-info-properties info) props)
      (setq idee/project-info-alist (delq (assoc (intern name) idee/project-info-alist) idee/project-info-alist))
      (add-to-list 'idee/project-info-alist `(,(intern name) . ,info)))))

;;
;; Initialization
;;
(defun idee/project-initialize ()
  "Initialize ide project.
When called this function will look at the project root for an elisp script called .idee/init.el and will load it if present."
  (interactive)
  (let* ((root-dir (idee/project-root-dir (buffer-file-name)))
         (conf-dir (concat (file-name-as-directory root-dir) idee/project-conf-dir))
         (init-el (concat (file-name-as-directory conf-dir) "init.el")))
    (when (file-exists-p init-el) (load-file init-el))))

(provide 'idee-projects)
;;; idee-projects.el ends here
