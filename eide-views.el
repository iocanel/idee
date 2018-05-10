;;; eide-views.el --- Views

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

(require 'eide-vars)
(require 'eide-actions)

(require 'magit)

(defun eide-project-open-view()
  "Switches to a traditional IDE view for the buffer. (project tree, main buffer & terminal"
  (interactive)
  (eide-ide-view)
  (magit-status)
  )

(defun eide-ide-view()
  "Switches to a traditional IDE view for the buffer. (project tree, main buffer & terminal"
  (interactive)
  (setq eide-current-view 'eide-ide-view)
  (delete-other-windows-internal)
  (if eide-cli-enabled
      (progn
        (split-and-follow-vertically)
        (minimize-window)
        (projectile-run-eshell)
        (evil-window-set-height 12)))
  (if eide-tree-enabled
      (treemacs-projectile))
  (other-window 1)
  (goto-char (point-min))
  )

(defun eide-side-by-side-view()
  "Open a new buffer from the project to the side for side by side view"
  (interactive)
  (delete-other-windows-internal)
  (split-and-follow-horizontally)
  ;; reduce the noise by switching to an untitled buffer
  (eide-new-empty-buffer)
  (projectile-find-file-dwim)
  )

(defun eide-repl-view()
  "Just like IDE view but with a REPL instead of a terminal (project tree, main buffer & repl)."
  (interactive)
  (setq eide-current-view 'eide-repl-view)
  (delete-other-windows-internal)
  (if eide-tree-enabled
      (treemacs-projectile))
  (if eide-repl-enabled
      (if (get-buffer (format "*cider-repl %s*" (projectile-project-name)))
          (progn
            (other-window 1)
            (split-and-follow-vertically)
            (switch-to-buffer (get-buffer (format "*cider-repl %s*" (projectile-project-name))))
            (goto-char (point-max))
            (evil-window-set-height 12)
            (other-window -1))
        (progn
          (other-window 1)
          (split-and-follow-vertically)
          (eide-new-empty-buffer)
          (evil-window-set-height 12)
          ;; The following commands need to get executed when eide-cider-on-connected is actually executed
          (setq eide-on-event-command-alist (delq (assoc 'on-repl-connected eide-on-event-command-alist) eide-on-event-command-alist))
          (add-to-list 'eide-on-event-command-alist '(on-repl-connected . (
                                                                           (other-window -1)
                                                                           (goto-char (point-min))
                                                                           )))
          (eide-repl)
          )))
  )

(defun eide-terminal-view()
  "Maximized terminal in the project root"
  (interactive)
  (setq eide-current-view eide-terminal-view)
  (delete-other-windows-internal)
  (projectile-run-eshell)
  )


;;
;;
;; View Mode Helpers
(defun eide-toggle-tree ()
  "Toggles the tree"
  (interactive)
  (if eide-tree-enabled
      (progn
        (setq eide-tree-enabled nil)      
        (eide-refresh-view))
    (progn
      (setq eide-tree-enabled t)
      (eide-refresh-view)
      )))

(defun eide-toggle-cli ()
  "Toggles the cli"
  (interactive)
  (if eide-cli-enabled
      (progn
        (setq eide-cli-enabled nil)      
        (eide-refresh-view))
    (progn
      (setq eide-cli-enabled t)
      (eide-refresh-view)
      (other-window 1)
      (goto-char (point-max)))))

(defun eide-refresh-view ()
  "Refreshes the current view"
  (interactive)
  (funcall eide-current-view)
  )

(defun eide-new-empty-buffer()
  "Just creates an empty buffer"
  (let ((fl (make-temp-file "Untitled")))
    (switch-to-buffer fl)))
;
(advice-add 'projectile-switch-project :after 'eide-project-open-view)

(provide 'eide-views)
;;; eide-views.el ends here
