;; idee-treemacs.el --- Treemacs integration


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

;;; Commentary:

;;; Code:

(require 'treemacs-projectile)
(require 'treemacs)
(require 'hydra)
(require 'idee-views)

(require 'treemacs-projectile)
(require 'treemacs)
(require 'treemacs-bookmarks)
(require 'hydra)
(require 'idee-views)

(defun idee/treemacs-collapse-dir-toggle ()
  "Toggle value of treemacs-collapse-dir between 3 0."
  (interactive)
  (if (= treemacs-collapse-dirs 0)
      (setq treemacs-collapse-dirs 3)
    (setq treemacs-collapse-dirs 0))
  (treemacs-refresh))

;;;###autoload
(defun idee/treemacs-create-and-switch-to-workspace ()
  "Create and switch to a new treemacs workspace."
  (interactive)
  (idee/workspace-close)
  (when (and idee/tree-enabled (not (equal (treemacs-current-visibility) 'visible))) (treemacs))
  (let* ((response (treemacs-do-create-workspace))
         (workspace (car (cdr response))))
    (setf (treemacs-current-workspace) workspace)
    ;; BEGIN: Copy and paste from treemacs-switch-workspace
    (let ((window-visible? nil)
          (buffer-exists? nil))
      (pcase (treemacs-current-visibility)
        ('visible
         (setq window-visible? t
               buffer-exists? t))
        ('exists
         (setq buffer-exists? t)))
      (when window-visible?
        (delete-window (treemacs-get-local-window)))
      (when buffer-exists?
        (kill-buffer (treemacs-get-local-buffer)))
      (when buffer-exists?
        (let ((treemacs-follow-after-init nil)
              (treemacs-follow-mode nil))
          (treemacs-select-window)))
      (when (not window-visible?)
        (bury-buffer)))
    (treemacs-pulse-on-success "Selected workspace %s."
      (propertize (treemacs-workspace->name workspace)))
    ;; END
    (idee/treemacs-open-project-workspace workspace)))

;;;###autoload
(defun idee/treemacs-switch-to-project-workspace ()
    "Select a different workspace for treemacs."
    (interactive)
    (idee/workspace-close)
    (when (and idee/tree-enabled (not (equal (treemacs-current-visibility) 'visible))) (treemacs))
    (pcase (treemacs-do-switch-workspace)
      ('only-one-workspace
       (treemacs-pulse-on-failure "There are no other workspaces to select."))
      (`(success ,workspace)
       (let ((window-visible? nil)
             (buffer-exists? nil))
         (pcase (treemacs-current-visibility)
           ('visible
            (setq window-visible? t
                  buffer-exists? t))
           ('exists
            (setq buffer-exists? t)))
         (when window-visible?
           (delete-window (treemacs-get-local-window)))
         (when buffer-exists?
           (kill-buffer (treemacs-get-local-buffer)))
         (when buffer-exists?
           (let ((treemacs-follow-after-init nil)
                 (treemacs-follow-mode nil))
             (treemacs-select-window)))
         (when (not window-visible?)
           (bury-buffer)))
       (treemacs-pulse-on-success "Selected workspace %s."
         (propertize (treemacs-workspace->name workspace))
         (idee/treemacs-open-project-workspace workspace)))))

(defun idee/treemacs-open-project-workspace (workspace)
  "Open the first project of the WORKSPACE."
  (when (and workspace (treemacs-project->path workspace))
      (let* ((name (treemacs-project->name workspace))
             (project (treemacs-project->path workspace))
             (path (treemacs-project->path (car project))))
        (when path
          (projectile-switch-project-by-name path)
          (idee/reset-view)
          (idee/jump-to-non-idee/window)))))


;;;###autoload
(defun idee/workspace-close ()
  (interactive)
  (idee/view-reset)
  (when (treemacs-current-workspace)
  (let* ((projects (treemacs-workspace->projects (treemacs-current-workspace))))
      (dolist (project projects)
        (idee/project-close-buffers (treemacs-project->path project))))))

(defun idee/treemacs--enabled-flag ()
  "Visual represntation of the offline flag."
  (if (eq 'visible (treemacs-current-visibility)) [*] [_]))
(defun idee/treemacs--show-hidden-files-flag ()
  "Visual represntation of the offline flag."
  (if treemacs-show-hidden-files [*] [_]))
(defun idee/treemacs--workspace-name ()
  "Visual represntation of the workspace name."
  (intern (treemacs-workspace->name (treemacs-current-workspace))))

;;;###autoload (autoload 'idee/treemacs-hydra/body "idee-treemacs")
(defhydra idee/treemacs-hydra (:hint none :exit t)
  ;; The '_' character is not displayed. This affects columns alignment.
  ;; Remove s many spaces as needed to make up for the '_' deficit.
  "
    ^Workspaces               ^Windows^                     ^Navigation^       ^Toggles^                
    [%(idee/treemacs--workspace-name)]
    ^^^^^^----------------------------------------------------------------------------------------------   
    _N_: new workspace         _s_: select window            _b_: bookmark      _t_: ?t? tree view toggle
    _S_: switch workspace      _d_: delete other windows     _f_: find file     _h_: ?h? show hidden files
    _R_: remove workspace                                  _T_: find tag      _c_: collapse dirs     
    _E_: edit workspace                                                     _g_: magit             
    _F_: finish edit         

   [_q_]: quit
   "
  ; Toggles
  ("N" idee/treemacs-create-and-switch-to-workspace)
  ("R" treemacs-remove-workspace)
  ("S" idee/treemacs-switch-to-project-workspace)
  ("E" treemacs-edit-workspaces)
  ("F" treemacs-finish-edit)
  ("t" treemacs (if (eq (treemacs-current-visibility) 'visible) "[*]" "[ ]") :exit nil)
  ("h" treemacs-toggle-show-dotfiles (if treemacs-show-hidden-files "[*]" "[ ]") :exit nil)
  ("c" idee/treemacs-collapse-dir-toggle :exit nil)
  ("g" magit-status)
                                        ; Windows
  ("s" treemacs-select-window)
  ("d" treemacs-delete-other-windows)
                                        ; Navifation
  ("b" treemacs-bookmark)
  ("f" treemacs-find-file)
  ("T" treemacs-find-tag)
  ("q" nil "quit"))

(provide 'idee-treemacs)
;; idee-treemacs.el ends here.
