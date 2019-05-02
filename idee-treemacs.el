;;; idee-treemacs.el --- Treemacs integration

;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(defun idee-treemacs-collapse-dir-toggle ()
  "Toggle value of treemacs-collapse-dir between 3 0."
  (interactive)
  (if (= treemacs-collapse-dirs 0)
      (setq treemacs-collapse-dirs 3)
    (setq treemacs-collapse-dirs 0))
  (treemacs-refresh))

(defun idee-treemacs-create-and-switch-to-workspace ()
  "Create and switch to a new treemacs workspace."
  (interactive)
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
     (idee-treemacs-open-project-workspace workspace)))


(defun idee-treemacs-open-project-workspace (workspace)
  "Open the first project of the WORKSPACE."
  (let* ((selected workspace)
         (name (treemacs-project->name selected))
         (project (treemacs-project->path selected))
         (path (treemacs-project->path (car project))))
    (projectile-switch-project-by-name path)
    (idee-refresh-view)
    (idee-jump-to-non-ide-window)))

(advice-add 'treemacs-switch-workspace :after (lambda () (idee-treemacs-open-project-workspace (treemacs-current-workspace))))

(provide 'idee-treemacs)
;;; idee-treemacs.el ends here.
