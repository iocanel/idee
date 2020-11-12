;;; idee-java.el --- Java IDE

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

(defcustom idee-lsp-workspace-per-project-enabled t "Lsp Workspace per Project Feature Toggle" :group 'idee-lsp :type 'boolean)

(defcustom idee-lsp-before-workspace-restart-hook nil "The hooks to run before restarting the workspace"
  :type 'hook
  :group 'idee-lsp)

(defun idee-lsp-close-workspace ()
  "Close the current workspace."
    (when (lsp-workspaces) (lsp-shutdown-workspace)))

(defun idee-lsp-switch-workspace (&optional w)
  "Switch to workspace W."
  (let* ((current (treemacs-current-workspace))
         (name (if current (treemacs-workspace->name current) nil))
         (workspace
          (cond
           (w w)
           ((and name idee-lsp-workspace-per-project-enabled (f-join (f-join (locate-user-emacs-file "lsp") "workspace") name)))
           (t (locate-user-emacs-file "workspace")))))

    (make-directory workspace t)
    (message (format "Using LSP workspace: %s." workspace))
    (dolist (element idee-lsp-before-workspace-restart-hook)
      (funcall element workspace))

    (setq lsp-session-file (f-join workspace ".lsp-session-v1"))
    (when (lsp-workspaces) (lsp-restart-workspace))))


(defun idee-lsp-init ()
  (interactive)
  "Intialize LSP."
  (advice-add 'idee-workspace-close :before #'idee-lsp-close-workspace)
  (advice-add 'treemacs-switch-workspace :before #'idee-lsp-close-workspace)
  (add-hook 'treemacs-switch-workspace-hook 'idee-lsp-switch-workspace))

(provide 'idee-lsp)
;;; idee-lsp.el ends here
