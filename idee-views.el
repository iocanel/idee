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

(require 'magit)
(require 'treemacs-projectile)
(require 'treemacs)

;;
;; State
;;
(defvar idee-current-view 'idee-ide-view)

;; Toggles
(defvar idee-tree-enabled t)
(defvar idee-cli-enabled t)
(defvar idee-output-enabled t)
(defvar idee-repl-enabled t)
(defvar idee-diagnostics-enabled t)
(defvar idee-errors-enabled t)
(defvar idee-messages-enabled t)
(defvar idee-grep-enabled nil)
(defvar idee-helm-ag-enabled nil)
(defvar idee-bottom-buffer-command 'idee-projectile-run-eshell)

;; A list with all component switches that are meant to be placed in the bottom
(defvar idee-bottom-area-switch-list '(idee-cli-enabled idee-repl-enabled idee-diagnostics-enabled idee-errors-enabled idee-messages-enabled idee-grep-enabled idee-helm-ag-enabled))

;;
;; Functions
;;
(setq idee-current-view 'idee-ide-view)

(defun idee-project-open-view()
  "Switch to a traditional IDE view for the buffer.  (project tree, main buffer & terminal)."
 (interactive)
 (dolist (b idee-bottom-area-switch-list)
   (message (format "%s"b))
   (setq b nil))
  (idee-ide-view)
  (idee-jump-to-non-ide-window)
  (magit-status-internal (projectile-project-root)))

(defun idee-ide-view()
  "Switch to a traditional IDE view for the buffer.  (project tree, main buffer & terminal)."
  (interactive)
  (setq idee-current-view 'idee-ide-view)
  (idee-jump-to-non-ide-window ())
  (delete-other-windows-internal)
  (if idee-tree-enabled
      (progn
        (treemacs--init (projectile-project-root))
        ;; we remove the mode-line to hide the treemacs label
        (setq mode-line-format nil)))
  (idee-jump-to-non-ide-window)
  ;; bottom area
  (cond (idee-grep-enabled (idee-grep-subview))
        (idee-helm-ag-enabled (idee-helm-ag-subview))
        (idee-cli-enabled (idee-cli-subview))
        (idee-diagnostics-enabled (idee-diagnostics-subview))
        (idee-errors-enabled (idee-errors-subview))
        (idee-messages-enabled (idee-messages-subview))))

(defun idee-cli-subview ()
  (when (not (idee-cli-visible-p))
    (idee-split-and-follow-vertically)
    (minimize-window)
    (idee-projectile-run-eshell)
    (evil-window-set-height 12)))

(defun idee-diagnostics-subview ()
  (flymake-show-diagnostics-buffer)
  (let ((name (flymake--diagnostics-buffer-name)))
  (idee-jump-to-non-ide-window)
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer name)
  (minimize-window)
  (evil-window-set-height 12)))

(defun idee-errors-subview ()
  (flycheck-list-errors)
  (idee-jump-to-non-ide-window)
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*Flycheck errors*")
  (minimize-window)
  (evil-window-set-height 12))

(defun idee-messages-subview ()
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*Messages*")
  (minimize-window)
 (evil-window-set-height 12))

 (defun idee-grep-subview ()
  (if (get-buffer "*grep*")
      (progn
        (split-window-below)
        (other-window 1)
        (switch-to-buffer "*grep*"))
    (progn
      (projectile-grep)
      (idee-jump-to-non-ide-window)
      (delete-other-windows)
      (split-window-below)
      (other-window 1)
      (switch-to-buffer "*grep*")))
  (minimize-window)
  (evil-window-set-height 12))

(defun idee-helm-ag-subview ()
  (require 'helm-projectile)
  (require 'helm-ag)
  (cond
   ((get-buffer "*helm-ag-edit*") (progn
                                    (split-window-below)
                                    (other-window 1)
                                    (switch-to-buffer "*helm-ag-edit*")))
   ((get-buffer "*helm-ag*") (progn
                                    (split-window-below)
                                    (other-window 1)
                                    (switch-to-buffer "*helm-ag*")))
   (t (progn
        (helm-projectile-ag)
        (idee-jump-to-non-ide-window)
        (delete-other-windows)
        (split-window-below)
        (other-window 1)
        (switch-to-buffer "*helm-ag*"))))
  (minimize-window)
  (evil-window-set-height 12))

(defun idee-side-by-side-view()
  "Open a new buffer from the project to the side for side by side view."
  (interactive)
  (delete-other-windows-internal)
  (idee-split-and-follow-horizontally)
  ;; reduce the noise by switching to an untitled buffer
  (idee-new-empty-buffer)
  (projectile-find-file-dwim))

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
                                                                           (goto-char (point-min)))))
          (idee-repl)))))

(defun idee-terminal-view()
  "Maximize terminal in the project root."
  (interactive)
  (setq idee-current-view 'idee-terminal-view)
  (delete-other-windows-internal)
  (idee-projectile-run-eshell))

;;
;;
;; View Mode Helpers

(defun idee-ide-buffer-p (buffer-name)
  "Predicate to check if BUFFER-NAME is an ide buffer (e.g. tree, cli, repl, diagnostics etc)."
  (let ((mode (with-current-buffer buffer-name major-mode))
        (name (string-trim buffer-name)))
  (cond ((provided-mode-derived-p 'prog-mode mode) t)
        ((and (string-prefix-p "*" name)  (string-suffix-p "*" name)) t)
        (t nil))))

(defun idee-jump-to-non-ide-window(&optional visited)
  "Jump to a non IDE window.
VISITED is an optional list with windows already visited."
  (interactive)
  (let* ((visited (or visited '()))
         (buffer (current-buffer))
         (name (buffer-name buffer))
         (ide-buffer (idee-ide-buffer-p name)))

         (cond ((not ide-buffer) t)
               ((member name visited) nil)
               (t (progn (other-window 1)
                         (idee-jump-to-non-ide-window (add-to-list 'visited name)))))))

(defun idee-update-tree-state()
  "Update the state of the tree switch (in case the winodw has been externally closed)."
  (if (equal (treemacs-current-visibility) 'visible)
      (setq idee-tree-enabled t)
    (setq idee-tree-enabled nil)))


(defun idee-toggle-tree ()
  "Toggle the tree."
  (interactive)
  (idee-update-tree-state)
  (if idee-tree-enabled
      (progn
        (setq idee-tree-enabled nil)
        (idee-refresh-view))
    (progn
      (setq idee-tree-enabled t)
      (idee-refresh-view))))


 (defun idee-refresh-view ()
  "Refresh the current view."
  (interactive)
  (funcall idee-current-view))

(defun idee-new-empty-buffer()
  "Create an empty buffer."
  (let ((fl (make-temp-file "Untitled")))
    (switch-to-buffer fl)))

(defun idee-split-and-follow-horizontally ()
  "Split window horizontally and follow."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))


(defun idee-split-and-follow-vertically ()
  "Split window vertically and follow."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun idee-projectile-run-eshell ()
  "Invoke `eshell' in the project's root.

Switch to the project specific eshell buffer if it already exists."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (let ((eshell-buffer-name (concat "*eshell " (projectile-project-name) "*")))
      (when (not (idee-cli-visible-p))
        (if (not (string-prefix-p "*eshell" (buffer-name)))
            ;; If running inside doom use +eshell/here.
            (if (fboundp '+eshell/here)
                (+eshell/here nil)
              (eshell)))))))
;;
;; Buffer providers
;;
(defun idee-hydra-visible-p ()
  "Return non-nil if hydra is visible."
  (get-buffer-window " *LV"))

(defun idee-cli-visible-p ()
  "Return non-nil if cli is visible."
  (seq-filter (lambda (w) (string-prefix-p "*eshell" (buffer-name (window-buffer w)))) (get-buffer-window-list)))

(defun idee-diagnostics-visible-p ()
  "Return non-nil if diagnostics is visible."
  (get-buffer-window (flymake--diagnostics-buffer-name)))

(defun idee-errors-visible-p ()
  "Return non-nil if errors is visible."
  (get-buffer-window "*Flycheck errors*"))

(defun idee-messages-visible-p ()
  "Return non-nil if messages is visible."
  (get-buffer-window "*Messages*"))

(defun idee-grep-visible-p ()
  "Return non-nil if grep is visible."
  (get-buffer-window "*grep*"))

(defun idee-helm-ag-visible-p ()
  "Return non-nil if helm-ag is visible."
  (get-buffer-window "*helm-ag*"))

(defun idee-after-next-error ()
  "Refresh the view each time next error is caled."
  (if next-error-last-buffer
      (idee-refresh-view)))
;;
;; Macros
;;
(defmacro idee--create-view-component (name buffer-predicate flag candidates pivot)
  "Update the state of the FLAG (in case the winodw has been externally closed).

NAME is the name of the view component.
BUFFER-PREDICATE is a function that returns non-nil if buffer is currently visible.
FLAG is the variable that holds the  visibility state of the component (e.g. visible or not visible).
CANDIDATES is a list containing all other flags that take up the same space as the target component (e.g. cli and  diagnostics use the same area).
PIVOT indicates how many windows should be switched at the end of the operation."
  (declare (indent 1) (debug t))
  `(progn
    (defun ,(intern (format "idee-update-%s-state" name)) ()
  ,(format "Update the state of the %s (in case the winodw has been externally closed)." name)
  (idee-jump-to-non-ide-window)
  (if (,buffer-predicate)
      (progn (dolist (c ,candidates)
               (set c nil))
               (setq ,flag t))
    (setq ,flag nil)))
  
 (defun ,(intern (format "idee-toggle-%s" name)) ()
  ,(format "Toggle the state of the %s." name)
  (interactive)
  (funcall (intern (format "idee-update-%s-state" ,name)))
  (if ,flag
      (progn
        (setq ,flag nil)
        (idee-refresh-view))
    (progn
      (dolist (c ,candidates)
               (set c nil))
      (setq ,flag t)
      (idee-refresh-view)
      (other-window ,pivot)
      (goto-char (point-max)))))

 (defun ,(intern (format "idee-switch-%s-on" name)) ()
  ,(format "Switch %s on." name)
  (interactive)
  (funcall (intern (format "idee-update-%s-state" ,name)))
  (if (not ,flag)
      (idee-toggle-cli)
    (idee-refresh-view)))))

;;
;; Create component view functions
;;
(idee--create-view-component "errors" idee-errors-visible-p idee-errors-enabled idee-bottom-area-switch-list 0)
(idee--create-view-component "diagnostics" idee-diagnostics-visible-p idee-diagnostics-enabled idee-bottom-area-switch-list 0)
(idee--create-view-component "cli"  idee-cli-visible-p idee-cli-enabled idee-bottom-area-switch-list 0)
(idee--create-view-component "messages"  idee-messages-visible-p idee-messages-enabled idee-bottom-area-switch-list 0)
(idee--create-view-component "grep"  idee-grep-visible-p idee-grep-enabled idee-bottom-area-switch-list 0)
(idee--create-view-component "helm-ag"  idee-helm-ag-visible-p idee-helm-ag-enabled idee-bottom-area-switch-list 0)

(defun idee-toggle-helm-ag-or-grep  ()
  "Toggle helm-ag if helm-ag is installed or fallback to projectile-grep."
  (interactive)
  (if (and (require 'helm-projectile nil 'noerror) (require 'helm-ag nil 'noerror))
      (idee-toggle-helm-ag)
    (idee-toggle-grep)))

(defun idee-kill-grep-and-window ()
  "Kill the grep window and buffer.  Return t if grep window was found."
  (let ((buffer (current-buffer)))
    (if (equal "*grep*" (buffer-name buffer))
        (progn
          (kill-buffer-and-window)
          t)
      nil)))

(defun idee-kill-helm-ag-and-window ()
  "Kill the helm-ag window and buffer.  Return t if helm-ag/edit window was found."
  (let ((buffer (current-buffer)))
    (if (or (equal "*helm-ag*" (buffer-name buffer)) (equal "*helm-ag-edit*" (buffer-name buffer)))
        (progn
          (kill-buffer-and-window)
          t)
      nil)))

(defadvice quit-window (around idee-on-quit-window (&optional kill window))
  "Handles things when quiting window."
  (cond
   ((idee-kill-grep-and-window) t)
   ((idee-kill-helm-ag-and-window) t)
   (t ad-do-it)))


(ad-activate 'quit-window)
(advice-add 'projectile-switch-project :after 'idee-project-open-view)
(advice-add 'treemacs-switch-workspace :after 'idee-project-open-view)
(advice-add 'next-error :after 'idee-after-next-error)

(advice-add 'helm-ag-edit :after 'idee-refresh-view)
(advice-add 'helm-ag-edit--commit :after 'idee-refresh-view)
(advice-add 'helm-ag-edit--abort :after 'idee-refresh-view)

(provide 'idee-views)
;;; idee-views.el ends here
