;; idee-lsp.el --- LSP support for IDE -*- lexical-binding: t -*-

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

;; Package-Requires: ((lsp-mode "6.2"))

;;; Code:
(require 'lsp-mode)

(defcustom idee/lsp-workspace-per-project-enabled t "Lsp Workspace per Project Feature Toggle" :group 'idee/lsp :type 'boolean)

(defcustom idee/lsp-before-workspace-restart-hook nil "The hooks to run before restarting the workspace"
  :type 'hook
  :group 'idee/lsp)

(defun idee/lsp-enable()
  "Enable lsp bindings."
  (interactive)

  ;; Clear functions
  (setq idee/function-alist (delq (assoc 'idee/execute-code-actions-function idee/function-alist) idee/function-alist))

  ;; Set functions
  (add-to-list 'idee/function-alist '(idee/execute-code-actions-function . idee/lsp-execute-code-actions)))
(defun idee/lsp-execute-code-actions ()

  "Execute code actions."
  (interactive)
  (let ((action (list (lsp--select-action (lsp-code-actions-at-point)))))
    (lsp-execute-code-action action)))

(defun idee/lsp-close-workspace ()
  "Close the current workspace."
  (let ((workspace (idee/lsp-get-current-workspace)))
    (when workspace (lsp-workspace-shutdown workspace))))

(defun idee/lsp-switch-workspace (&optional dir)
  "Switch to workspace directory DIR."
  (let* ((current (treemacs-current-workspace))
         (name (if current (treemacs-workspace->name current) nil))
         (workspace (idee/lsp-get-current-workspace))
         (workspace-dir
          (cond
           (dir dir)
           ((and name idee/lsp-workspace-per-project-enabled (f-join (f-join (locate-user-emacs-file "lsp") "workspace") name)))
           (t (locate-user-emacs-file "workspace")))))

    (when workspace-dir
      (make-directory workspace-dir t)
      (message (format "Using LSP workspace: %s." workspace-dir))
      (setq lsp-session-file (f-join workspace-dir ".lsp-session-v1"))
      (message "Setting lsp session file: %s" lsp-session-file)
      (dolist (element idee/lsp-before-workspace-restart-hook)
        (funcall element workspace-dir)))

    (when workspace
      (when (lsp-workspaces) (lsp-workspace-restart workspace)))))

(defun idee/lsp-get-current-workspace ()
  (let ((project-root (projectile-project-root)))
    (car (seq-filter (lambda (w) (eq 
                                  (file-name-as-directory (file-truename project-root))
                                  (file-name-as-directory (file-truename w)))) (lsp-workspaces)))))

(defun idee/lsp-init ()
  (interactive)
  "Intialize LSP."
  (advice-add 'idee/workspace-close :before #'idee/lsp-close-workspace)
  (advice-add 'treemacs-switch-workspace :before #'idee/lsp-close-workspace)
  (add-hook 'treemacs-switch-workspace-hook 'idee/lsp-switch-workspace)
  (add-hook 'lsp-mode-hook 'idee/lsp-enable))

(provide 'idee-lsp)
;; idee-lsp.el ends here
