;;; idee-views.el --- Views -*- lexical-binding: t -*-

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

(require 'idee-vars)
(require 'idee-actions)

(require 'magit)
(require 'treemacs-projectile)
(require 'treemacs)

(require 'fringe)
(require 'helm-projectile nil t)

;;
;; State
;;

(defcustom idee-tree-enabled-default t "Default state of the tree view" :group 'idee-view :type 'boolean)
(defcustom idee-eww-default-url nil "The url to use when opening the eww subview." :group 'idee-view :type 'boolean)
(defcustom idee-focus-center-buffer t "Flag to specify that the focus buffer needs to be centered." :group 'idee-view :type 'boolean) 
(defcustom idee-focus-center-buffer-columns 160 "The number of columns of the centered buffer." :group 'idee-view :type 'int) 
(defvar idee-current-view 'idee-ide-view)
(defvar idee-tree-enabled idee-tree-enabled-default)

;; Active Components
(defvar idee-tree-active t)
(defvar idee-cli-active t)
(defvar idee-output-active t)
(defvar idee-repl-active t)
(defvar idee-diagnostics-active t)
(defvar idee-errors-active t)
(defvar idee-messages-active t)
(defvar idee-grep-active nil)
(defvar idee-helm-ag-active nil)
(defvar idee-xref-active nil)
(defvar idee-eww-active nil)
(defvar idee-side-by-side-active nil)
(defvar idee-bottom-buffer-command 'idee-projectile-run-eshell)

(defvar idee-primary-buffer nil "Primary buffer")
(defvar idee-side-by-side-buffer nil "Secondary buffer")
(defvar idee-repl-kind nil "The kind of the repl buffer. This is framework/lang specific.")
(defvar idee-repl-buffer-prefix nil "The prefix of the repl buffer. This is framework/lang specific.")
(defvar idee-repl-buffer-prompt nil "The prompt of the repl buffer. This is framework/lang specific.")
;;; The following is based on Protesilaos Stavrou configuration: https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/emacs-init.org
(defvar idee-focus-window-configuration nil "Current window configuration.")
(defvar idee-focus-treemacs-visible nil "Additional varaible to hold info about treemacs, as idee-window-configuration does not apply to treemacs.")
(defvar idee-focus-fringe-mode fringe-mode)
(define-minor-mode idee-focus-mode
  "Toggle between multiple windows and single window. This is the equivalent of maximising a window."
  :lighter " [M]"
  :global nil
  (if (not (and (boundp 'idee-focus-mode) idee-focus-mode))       ;; If we have idee-focus-mode
      (when idee-focus-window-configuration                       ;; And and exisitng configuration
        (progn                                                    ;; Restore ...
          (set-window-configuration idee-focus-window-configuration)
          (when idee-focus-center-buffer (set-fringe-mode idee-focus-fringe-mode))
          (when (and
                 (equal 'visible idee-focus-treemacs-visible)
                 (not (equal 'visible (treemacs-current-visibility)))) (treemacs))))
    ;; Focus
    (progn
      (setq idee-focus-window-configuration (current-window-configuration))
      (setq idee-focus-treemacs-visible (treemacs-current-visibility))
      (delete-other-windows)
      (when idee-focus-center-buffer (set-fringe-mode (/ (- (frame-pixel-width) (* idee-focus-center-buffer-columns (frame-char-width))) 2)))))
  (setq idee-focus-treemacs-visible (treemacs-current-visibility)))
;; A list with all component switches that are meant to be placed in the bottom
(defvar idee-bottom-area-switch-list '(idee-cli-active idee-repl-active idee-diagnostics-active idee-errors-active idee-messages-active idee-grep-active idee-helm-ag-active idee-xref-active))

(defvar idee-right-area-switch-list '(idee-eww-active idee-side-by-side-active))
(setq idee-current-view 'idee-ide-view)
;; Functions
;;;###autoload
(defun idee-view-reset()
    "Reset view variables."
    (setq idee-cli-active nil
          idee-repl-active nil
          idee-output-active nil
          idee-diagnostics-active nil
          idee-errors-active nil
          idee-messages-active nil
          idee-grep-active nil
          idee-helm-ag-active nil
          idee-xref-active nil
          idee-eww-active nil
          idee-side-by-side-active nil))

;;;###autoload
(defun idee-project-open-view(&optional path)
  "Switch to a traditional IDE view for the buffer.  (project tree, main buffer & terminal)."
 (interactive)
  (let* ((path (or path (or (projectile-project-root) default-directory))))
    (dolist (b idee-bottom-area-switch-list)
      (set b nil))
    (dired path)
    (idee-ide-view)
    (idee-jump-to-non-ide-window)
    (magit-status-setup-buffer path)))

;;;###autoload
(defun idee-ide-view()
  "Switch to a traditional IDE view for the buffer.  (project tree, main buffer & terminal)."
  (interactive)
  (setq idee-current-view 'idee-ide-view)
  (idee-jump-to-non-ide-window ())
  ;; In some cases, it's better to swtich (e.g. when current bufer is a side buffer
  (when (and idee-primary-buffer (idee-ide-buffer-p (buffer-name (current-buffer)))
             (progn
               (message "No project buffer visible, switching to %s." (buffer-name idee-primary-buffer))
               (switch-to-buffer idee-primary-buffer))))
  (delete-other-windows-internal)
  (if (and idee-tree-enabled idee-tree-active)
      (progn
        (treemacs--init (projectile-project-root))
        ;; we remove the mode-line to hide the treemacs label
        (setq mode-line-format nil)))
  (idee-jump-to-non-ide-window)
  (setq idee-primary-buffer (current-buffer))
  ;; bottom area
  (cond (idee-grep-active (idee-grep-subview))
        (idee-helm-ag-active (idee-helm-ag-subview))
        (idee-cli-active (idee-cli-subview))
        (idee-repl-active (idee-repl-subview))
        (idee-diagnostics-active (idee-diagnostics-subview))
        (idee-errors-active (idee-errors-subview))
        (idee-messages-active (idee-messages-subview))
        (idee-xref-active (idee-xref-subview)))
  ;; right area
  (cond (idee-eww-active (idee-eww-subview))
        (idee-side-by-side-active (idee-side-by-side-subview))))
;;;###autoload
(defun idee-cli-subview ()
  (when (not (idee-cli-visible-p))
    (idee-split-and-follow-vertically)
    (minimize-window)
    (idee-projectile-run-eshell)
    (evil-window-set-height 12)))
(defun idee-repl-subview ()
  (when (not (idee-repl-visible-p))
    (idee-split-and-follow-vertically)
    (minimize-window)
    (idee-repl)
    (evil-window-set-height 12)))
;;;###autoload
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

;;;###autoload
(defun idee-errors-subview ()
  (flycheck-list-errors)
  (idee-jump-to-non-ide-window)
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*Flycheck errors*")
  (minimize-window)
  (evil-window-set-height 12))

;;;###autoload
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

;;;###autoload
(defun idee-helm-ag-subview ()
  (when (and (require 'helm-projectile nil t) (require 'helm-ag nil t))
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
    (evil-window-set-height 12)))

;;;###autoload
(defun idee-xref-subview ()
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*xref*")
  (minimize-window)
 (evil-window-set-height 12))

;;;###autoload
(defun idee-eww-subview ()
  (idee-jump-to-non-ide-window)
  (split-window-right)
  (other-window 1)
  (cond ((get-buffer "*eww*") (switch-to-buffer "*eww*"))
        (t (progn
             (if idee-eww-default-url
                 (eww idee-eww-default-url)
               (eww (format "http://localhost:%s" (idee-eshell-find-web-port))))
             (switch-to-buffer "*eww*"))))
  (evil-window-set-width 80))
(defun idee-side-by-side-subview ()
  (idee-jump-to-non-ide-window)
  (split-window-right)
  (other-window 1)
     (cond ((and idee-side-by-side-buffer (get-buffer idee-side-by-side-buffer)) (switch-to-buffer idee-side-by-side-buffer))
           (t (progn
                (let ((tmp-buffer (generate-new-buffer "*side*")))
                  (switch-to-buffer tmp-buffer) 
                  (idee-find-file)
                  (let* ((name (buffer-name (current-buffer)))
                         (actual-name (if (idee-starts-with "*side " name) (substring name 6 (- (length name)  7)) name))
                         (side-name (format "*side %s*" actual-name)))
                    (rename-buffer side-name)
                    (setq idee-side-by-side-buffer side-name))
                  (kill-buffer tmp-buffer))))))
(defun idee-open-side-by-side ()
  (interactive)
  "Open a new file in the side buffer."
  (setq idee-side-by-side-buffer nil)
  (idee-switch-side-by-side-on))
;;;###autoload
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

(defun idee-window-has-current-buffer-p ()
  "Returns non-nil when the current window display the current buffer."
  (let ((b (current-buffer))
        (w (frame-selected-window)))
    (equal (get-buffer-window b) w)))

;;;###autoload
(defun idee-jump-to-non-ide-window(&optional visited)
  "Jump to a non IDE window.
VISITED is an optional list with windows already visited."
  (interactive)
  (let* ((visited (or visited '()))
         (buffer (current-buffer))
         (name (buffer-name buffer))
         (ide-buffer (idee-ide-buffer-p name))
         (current-buffer-selected (idee-window-has-current-buffer-p)))

    (cond
     ((not (and ide-buffer current-buffer-selected)) t)
     ((member name visited) nil)
     (t (progn (other-window 1)
               (idee-jump-to-non-ide-window (push name visited)))))))

(defun idee-update-tree-state()
  "Update the state of the tree switch (in case the winodw has been externally closed)."
  (if (equal (treemacs-current-visibility) 'visible)
      (setq idee-tree-active t)
    (setq idee-tree-active nil)))


;;;###autoload
(defun idee-toggle-tree ()
  "Toggle the tree."
  (interactive)
  (idee-update-tree-state)
  (if idee-tree-enabled
      (progn
        (setq idee-tree-enabled nil)
        (setq idee-tree-active nil)
        (idee-refresh-view))
    (progn
      (setq idee-tree-enabled t)
      (setq idee-tree-active t)
      (idee-refresh-view))))

;;;###autoload
(defun idee-toggle-helm-ag-or-grep  ()
  "Toggle helm-ag if helm-ag is installed or fallback to projectile-grep."
  (interactive)
  (if (and (require 'helm-projectile nil 'noerror) (require 'helm-ag nil 'noerror))
      (idee-toggle-helm-ag)
    (idee-toggle-grep)))
;;;###autoload
 (defun idee-refresh-view ()
  "Refresh the current view."
  (interactive)
  (funcall idee-current-view))

;;;###autoload
(defun idee-new-empty-buffer()
  "Create an empty buffer."
  (let ((fl (make-temp-file "Untitled")))
    (switch-to-buffer fl)))

;;;###autoload
(defun idee-split-and-follow-horizontally ()
  "Split window horizontally and follow."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))


;;;###autoload
(defun idee-split-and-follow-vertically ()
  "Split window vertically and follow."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

;;;###autoload
(defun idee-projectile-run-eshell ()
  "Invoke `eshell' in the project's root.
   Switch to the project specific eshell buffer if it already exists."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
    (let ((eshell-buffer-name (format "*eshell %s*" (projectile-project-name)))
          (buf (get-buffer eshell-buffer-name)))
      (when (and (not (idee-cli-visible-p)) (not (string-prefix-p "*eshell " (buffer-name))))
        (cond (buf (switch-to-buffer buf))
              ((fboundp '+eshell/here) (+eshell/here)) ;; If running inside doom use +eshell/here.
              (:else (eshell)))
        ;; In some cases just setting eshell-buffer-name doesn't cut it
        (when (not (equal (buffer-file-name) eshell-buffer-name)) (rename-buffer eshell-buffer-name))))))
;;
;; Buffer providers
;;

(defun idee-get-visible-windows (pred)
  "Return a list of all visible windows that satisfy the PRED.
   PRED can be a function predicate, a buffer name or a buffer."
  (mapcar (lambda (b) (get-buffer-window b 'visible))
          (seq-filter (lambda (b) (and (get-buffer-window b 'visible)
                                       (cond ((functionp pred) (funcall pred b))
                                             ((stringp pred) (equal pred (buffer-name b)))
                                             ((bufferp pred) (equal (buffer-name pred) (buffer-name b)))
                                             (t nil)))) (buffer-list))))
(defun idee-hydra-visible-window ()
  "Return the hydra window if visible."
  (car (idee-get-visible-windows (lambda (b) (string-prefix-p "*LV" (buffer-name b))))))
(defun idee-hydra-visible-p ()
  "Return non-nil if hydra is visible."
  (idee-hydra-visible-window))
(defun idee-cli-visible-window ()
  "Return the visible cli window."
  (car (idee-get-visible-windows (lambda (b) (string-prefix-p "*eshell" (buffer-name b))))))
(defun idee-cli-visible-p ()
  "Return non-nil if cli is visible."
  (idee-cli-visible-window))
(defun idee-repl-visible-window ()
  "Return the visible repl window."
  (car (idee-get-visible-windows (lambda (b) (and idee-repl-buffer-prefix (string-prefix-p idee-repl-buffer-prefix (buffer-name b)))))))
(defun idee-repl-visible-p ()
  "Return non-nil if repl is visible."
  (idee-repl-visible-window))
(defun idee-diagnostics-visible-window ()
  "Return the visible diagnostics-window."
  (car (idee-get-visible-windows (flymake--diagnostics-buffer-name))))
(defun idee-diagnostics-visible-p ()
  "Return non-nil if diagnostics is visible."
  (idee-diagnostics-visible-window))
(defun idee-errors-visible-window ()
  "Return the errors window if visible."
  (car (idee-get-visible-windows "*Flycheck errors*")))
(defun idee-errors-visible-p ()
  "Return non-nil if error is visible."
  (idee-errors-visible-window))
(defun idee-messages-visible-window ()
  "Return the messages if visible."
  (car (idee-get-visible-windows "*Messages*")))
(defun idee-messages-visible-p ()
  "Return non-nil if messages is visible."
  (idee-messages-visible-window))
(defun idee-grep-visible-window ()
  "Return the grep window if visible."
  (car (idee-get-visible-windows "*grep*")))
(defun idee-grep-visible-p ()
  "Return non-nil if grep is visible."
  (idee-grep-visible-window))
(defun idee-helm-ag-visible-window ()
  "Return the helm-ag if visible."
  (car (idee-get-visible-windows "*helm-ag*")))
(defun idee-helm-ag-visible-p ()
  "Return non-nil if helm-ag is visible."
  (idee-helm-ag-visible-window))
(defun idee-xref-visible-window ()
  "Return the xref if visible."
  (car (idee-get-visible-windows "*xref*")))
(defun idee-xref-visible-p ()
  "Return non-nil if xref is visible."
  (idee-xref-visible-window))
(defun idee-eww-visible-window ()
  "Return the eww window if visible."
  (car (idee-get-visible-windows "*eww*")))
(defun idee-eww-visible-p ()
  "Return non-nil if eww is visible."
  (idee-eww-visible-window))

(defun idee-side-by-side-visible-window ()
  "Return the side-by-side window if visible."
  (and idee-side-by-side-buffer (get-buffer-window idee-side-by-side-buffer 'visible)))

(defun idee-side-by-side-visible-p ()
  "Return non-nil if eww is visible."
  (idee-side-by-side-visible-window))
(defun idee-after-next-error ()
  "Refresh the view each time next error is caled."
  (if next-error-last-buffer
      (idee-refresh-view)))

;;;###autoload
(defun idee-region-copy-to-other-window (start end)
  "Copy selected text from START to END over to other non IDE window."
  (interactive "r")
  (if (use-region-p) 
      (let* ((buffer (current-buffer))
            (name (buffer-name buffer))
            (current-window (selected-window)))
        (save-excursion
          (kill-ring-save start end)
          (other-window 1)
          (idee-jump-to-non-ide-window (list name))
          (evil-end-of-line)
          (evil-insert-newline-below)
          (yank)
          (select-window current-window)))))

;;;###autoload
(defun idee-region-move-to-other-window (start end)
  "Move selected text from START to END over to other non IDE window."
  (interactive "r")
  (if (use-region-p) 
      (let* ((buffer (current-buffer))
            (name (buffer-name buffer))
            (current-window (selected-window)))
        (save-excursion
          (kill-region start end)
          (other-window 1)
          (idee-jump-to-non-ide-window (list name))
          (evil-end-of-line)
          (evil-insert-newline-below)
          (yank)
          (select-window current-window)))))

;;
(defun idee-ediff()
  (interactive)
  (when (not idee-side-by-side-buffer) (idee-open-side-by-side))
  (let* ((left (buffer-file-name idee-primary-buffer))
         (right (buffer-file-name (get-buffer idee-side-by-side-buffer))))
    (ediff-files3 left right "/tmp/ediff-down")))
;; Macros
(defmacro idee--create-view-component (name window-provider flag candidates pivot)
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
  (if (,window-provider)
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
      (select-window (,window-provider))
      (goto-char (point-max)))))

 (defun ,(intern (format "idee-switch-%s-on" name)) ()
  ,(format "Switch %s on." name)
  (interactive)
  (funcall (intern (format "idee-update-%s-state" ,name)))
  (if (not ,flag)
      (funcall (intern (format "idee-toggle-%s" ,name)))
    (idee-refresh-view)))))

;;
;; Create component view functions
;;

;;;###autoload (autoload 'idee-toggle-errors "idee-views")
(idee--create-view-component "errors" idee-errors-visible-window idee-errors-active idee-bottom-area-switch-list 0)
;;;###autoload (autoload 'idee-toggle-diagnostics "idee-views")
(idee--create-view-component "diagnostics" idee-diagnostics-visible-window idee-diagnostics-active idee-bottom-area-switch-list 0)
;;;###autoload (autoload 'idee-toggle-cli "idee-views")
;;;###autoload (autoload 'idee-switch-cli-on "idee-views")
(idee--create-view-component "cli"  idee-cli-visible-window idee-cli-active idee-bottom-area-switch-list 0)
;;;###autoload (autoload 'idee-toggle-repl "idee-views")
;;;###autoload (autoload 'idee-switch-repl-on "idee-views")
(idee--create-view-component "repl"  idee-repl-visible-window idee-repl-active idee-bottom-area-switch-list 0)
;;;###autoload (autoload 'idee-toggle-messages "idee-views")
(idee--create-view-component "messages"  idee-messages-visible-window idee-messages-active idee-bottom-area-switch-list 0)
;;;###autoload (autoload 'idee-toggle-grep "idee-views")
(idee--create-view-component "grep"  idee-grep-visible-window idee-grep-active idee-bottom-area-switch-list 0)
;;;###autoload (autoload 'idee-toggle-helm-ag "idee-views")
(idee--create-view-component "helm-ag"  idee-helm-ag-visible-window idee-helm-ag-active idee-bottom-area-switch-list 0)
;;;###autoload (autoload 'idee-toggle-xref "idee-views")
(idee--create-view-component "xref"  idee-xref-visible-window idee-xref-active idee-bottom-area-switch-list 0)
;;;###autoload (autoload 'idee-toggle-eww "idee-views")
(idee--create-view-component "eww"  idee-eww-visible-window idee-eww-active idee-right-area-switch-list 0)
;;;###autoload (autoload 'idee-toggle-side-by-side "idee-views")
(idee--create-view-component "side-by-side"  idee-side-by-side-visible-window idee-side-by-side-active idee-right-area-switch-list 0)
(defun idee-repl--get-buffer ()
  "Return first matching repl buffer."
  (car (idee-repl--get-buffers)))
(defun idee-repl--get-buffers ()
  "Return matching repl buffers."
  (seq-filter (lambda (b) (and idee-repl-buffer-prefix (string-prefix-p idee-repl-buffer-prefix (buffer-name b)))) (buffer-list)))
(defun idee-repl-eval-string (s)
  "Evaluate S in the repl."
  (let* ((buffer (idee-repl--get-buffer))
         (process (buffer-name buffer)))
    (when process
      (comint-send-string process (format "%s\n" s))
      (sit-for 0.5)
      (with-current-buffer process
        (goto-char (point-max))
        (beginning-of-line)
        (let* ((end (- (point) 1))
               (start (+ (length idee-repl-buffer-prompt) (search-backward idee-repl-buffer-prompt nil t)))
               (result (buffer-substring start end)))
          result)))))
          
(defun idee-repl-eval-region (beginning end)
  "Evaluate region in the repl."
  (interactive "r")
  (idee-repl-eval-string (buffer-substring beginning end)))
(defun idee-repl-eval-region-tooltip (beginning end)
  "Evaluate region in the repl and show result in a tooltip."
  (interactive "r")
  (let ((result (string-trim (idee-repl-eval-region beginning end))))
    (if (idee-string-blank result) (tooltip-show "Ok")
      (tooltip-show result))))
;; Close and kill
(defun idee-kill-eshell-and-window ()
  "Kill the eshell window and buffer.  Return t if grep window was found."
  (let ((buffer (current-buffer)))
    (if (string-prefix-p "*eshell" (buffer-name buffer))
        (progn
          (kill-buffer-and-window)
          (setq idee-cli-active nil)
          (idee-refresh-view)
          t)
      nil)))

(defun idee-kill-messages-and-window ()
  "Kill the eshell window and buffer.  Return t if grep window was found."
  (let ((buffer (current-buffer)))
    (if (equal "*Messages*" (buffer-name buffer))
        (progn
          (kill-buffer-and-window)
          (setq idee-messages-active nil)
          (idee-refresh-view)
          t)
      nil)))

(defun idee-kill-grep-and-window ()
  "Kill the grep window and buffer.  Return t if grep window was found."
  (let ((buffer (current-buffer)))
    (if (equal "*grep*" (buffer-name buffer))
        (progn
          (kill-buffer-and-window)
          (setq idee-grep-active nil)
          (idee-refresh-view)
          t)
      nil)))

(defun idee-kill-helm-ag-and-window ()
  "Kill the helm-ag window and buffer.  Return t if helm-ag/edit window was found."
  (let ((buffer (current-buffer)))
    (if (or (equal "*helm-ag*" (buffer-name buffer)) (equal "*helm-ag-edit*" (buffer-name buffer)))
        (progn
          (kill-buffer-and-window)
          (setq idee-helm-ag-active nil)
          (idee-refresh-view)
          t)
      nil)))

(defun idee-kill-xref-and-window ()
  "Kill the xref window and buffer.  Return t if grep window was found."
  (let ((buffer (current-buffer)))
    (if (equal "*xref*" (buffer-name buffer))
        (progn
          (kill-buffer-and-window)
          (setq idee-xref-active nil)
          (idee-refresh-view)
          t)
      nil)))

(defun idee-kill-eww-and-window ()
  "Kill the eww window and buffer.  Return t if grep window was found."
  (let ((buffer (current-buffer)))
    (if (equal "*eww*" (buffer-name buffer))
        (progn
          (kill-buffer-and-window)
          (setq idee-eww-active nil)
          (idee-refresh-view)
          t)
      nil)))

(defun idee-on-delete-other-windows-internal (orig-fun &rest args)
    (let ((window (or (car args) (selected-window))))
      (when (not (window-parameter window 'window-side))
                 (apply orig-fun args))))

(defun idee-on-projectile-switch-project-by-name (orig-fun &rest args)
  "Intercept projectile-switch-project-by-name to get a hold of the name."
  (apply orig-fun args)
  (let ((project-name (car args)))
    (idee-project-init project-name)
    (idee-project-open-view project-name)))

(defadvice kill-current-buffer (around idee-on-kill-current-buffer (&optional kill window))
  "Handles things when quiting window."
  (cond
   ((idee-kill-messages-and-window) t)
   ((idee-kill-eshell-and-window) t)
   ((idee-kill-grep-and-window) t)
   ((idee-kill-helm-ag-and-window) t)
   ((idee-kill-xref-and-window) t)
   ((idee-kill-eww-and-window) t)
   (t ad-do-it)))
(defadvice quit-window (around idee-on-quit-window (&optional kill window))
  "Handles things when quiting window."
  (cond
   ((idee-kill-messages-and-window) t)
   ((idee-kill-eshell-and-window) t)
   ((idee-kill-grep-and-window) t)
   ((idee-kill-helm-ag-and-window) t)
   ((idee-kill-xref-and-window) t)
   ((idee-kill-eww-and-window) t)
   (t ad-do-it)))
;;;###autoload
(defun idee--views-init ()
  "Initialize idee views."
  (ad-activate 'quit-window)
  (ad-activate 'kill-current-buffer)
  (advice-add 'delete-other-windows-internal :around #'idee-on-delete-other-windows-internal)
  (advice-add 'projectile-switch-project-by-name :around #'idee-on-projectile-switch-project-by-name)

  (advice-add 'next-error :after 'idee-after-next-error)

  (advice-add 'helm-ag--edit :after 'idee-refresh-view)
  (advice-add 'helm-ag-edit--commit :after 'idee-refresh-view)
  (advice-add 'helm-ag-edit--abort :after 'idee-refresh-view)
  (advice-add 'lsp-show-xrefs :after 'idee-refresh-view))

(provide 'idee-views)
;;; idee-views.el ends here
