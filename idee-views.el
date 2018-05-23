;;; idee-views.el --- Views

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

(require 'idee-vars)
(require 'idee-actions)

(require 'magit)
(require 'treemacs-projectile)

(setq idee-current-view 'idee-ide-view)

(defun idee-project-open-view()
  "Switches to a traditional IDE view for the buffer. (project tree, main buffer & terminal"
  (interactive)
  (idee-ide-view)
  (magit-status-internal (projectile-project-root))
  )

(defun idee-ide-view()
  "Switches to a traditional IDE view for the buffer. (project tree, main buffer & terminal"
  (interactive)
  (setq idee-current-view 'idee-ide-view)
  (delete-other-windows-internal)
  (if idee-cli-enabled
      (progn
        (idee-split-and-follow-vertically)
        (minimize-window)
        (projectile-run-eshell)
        (evil-window-set-height 12)))
  (if idee-tree-enabled
      (treemacs--init (projectile-project-root)))
  (other-window 1)
  (goto-char (point-min))
  )

(defun idee-side-by-side-view()
  "Open a new buffer from the project to the side for side by side view"
  (interactive)
  (delete-other-windows-internal)
  (idee-split-and-follow-horizontally)
  ;; reduce the noise by switching to an untitled buffer
  (idee-new-empty-buffer)
  (projectile-find-file-dwim)
  )

(defun idee-repl-view()
  "Just like IDE view but with a REPL instead of a terminal (project tree, main buffer & repl)."
  (interactive)
  (setq idee-current-view 'idee-repl-view)
  (delete-other-windows-internal)
  (if idee-tree-enabled
      (treemacs--init (projectile-project-root)))
  (if idee-repl-enabled
      (if (get-buffer (format "*cider-repl %s*" (projectile-project-name)))
          (progn
            (other-window 1)
            (idee-split-and-follow-vertically)
            (switch-to-buffer (get-buffer (format "*cider-repl %s*" (projectile-project-name))))
            (goto-char (point-max))
            (evil-window-set-height 12)
            (other-window -1))
        (progn
          (other-window 1)
          (idee-split-and-follow-vertically)
          (idee-new-empty-buffer)
          (evil-window-set-height 12)
          ;; The following commands need to get executed when idee-cider-on-connected is actually executed
          (setq idee-on-event-command-alist (delq (assoc 'on-repl-connected idee-on-event-command-alist) idee-on-event-command-alist))
          (add-to-list 'idee-on-event-command-alist '(on-repl-connected . (
                                                                           (other-window -1)
                                                                           (goto-char (point-min))
                                                                           )))
          (idee-repl)
          )))
  )

(defun idee-terminal-view()
  "Maximized terminal in the project root"
  (interactive)
  (setq idee-current-view 'idee-terminal-view)
  (delete-other-windows-internal)
  (projectile-run-eshell)
  )


;;
;;
;; View Mode Helpers

(defun idee-update-tree-state()
  "Updates the state of the tree switch (in case the winodw has been externally closed)"
  (if (equal (format "%s" (treemacs--current-visibility)) "visible")
      (setq idee-tree-enabled t)
    (setq idee-tree-enabled nil))
  )


(defun idee-toggle-tree ()
  "Toggles the tree"
  (interactive)
  (idee-update-tree-state)
  (if idee-tree-enabled
      (progn
        (setq idee-tree-enabled nil)      
        (idee-refresh-view))
    (progn
      (setq idee-tree-enabled t)
      (idee-refresh-view)
      )))


(defun idee-update-cli-state()
  "Updates the state of the cli switch (in case the winodw has been externally closed)"
  (if (get-buffer-window (format "*eshell %s*" (projectile-project-name)))
      (setq idee-cli-enabled t)
    (setq idee-cli-enabled nil))
  )

(defun idee-toggle-cli ()
  "Toggles the cli"
  (interactive)
  (idee-update-cli-state)
  (if idee-cli-enabled
      (progn
        (setq idee-cli-enabled nil)      
        (idee-refresh-view))
    (progn
      (setq idee-cli-enabled t)
      (idee-refresh-view)
      (other-window 1)
      (goto-char (point-max)))))

(defun idee-refresh-view ()
  "Refreshes the current view"
  (interactive)
  (funcall idee-current-view)
  )

(defun idee-new-empty-buffer()
  "Just creates an empty buffer"
  (let ((fl (make-temp-file "Untitled")))
    (switch-to-buffer fl)))

(defun idee-split-and-follow-horizontally ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))


(defun idee-split-and-follow-vertically ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

                                        ;
(advice-add 'projectile-switch-project :after 'idee-project-open-view)

(provide 'idee-views)
;;; idee-views.el ends here
