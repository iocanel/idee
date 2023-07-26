;; idee-views.el --- Views -*- lexical-binding: t -*-

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

(require 'idee-actions)
(require 'idee-vars)
(require 'idee-utils)

(require 'magit)

(require 'fringe)

;;
;; State
;;
(defcustom idee/tree-enabled-default t "Default state of the tree view" :group 'idee/view :type 'boolean)
(defcustom idee/www-default-url nil "The url to use when opening the eww view." :group 'idee/view :type 'boolean)

(defcustom idee/focus-center-buffer t "Flag to specify that the focus buffer needs to be centered." :group 'idee/view :type 'boolean) 
(defcustom idee/focus-center-buffer-columns 160 "The number of columns of the centered buffer." :group 'idee/view :type 'int) 
(defcustom idee/display-buffer-setup-enabled nil "Enabled management of display-buffer-alist." :group 'idee/view :type 'boolean) 
(defcustom idee/popper-enabled nil "Enabled popper." :group 'idee/view :type 'boolean) 

(defvar idee/tree-enabled idee/tree-enabled-default)

;; Active Components
(defvar idee/tree-active t)
(defvar idee/cli-active t)
(defvar idee/output-active t)
(defvar idee/repl-active t)
(defvar idee/diagnostics-active t)
(defvar idee/errors-active t)
(defvar idee/messages-active t)
(defvar idee/grep-active nil)
(defvar idee/eww-active nil)
(defvar idee/xwidget-webkit-active nil)
(defvar idee/side-by-side-active nil)
(defvar idee/bottom-buffer-command 'idee/project-run-eshell)


(defvar idee/selected-window nil "The selected window. This should be selected after refresh.")
(defvar idee/primary-buffer nil "Primary buffer")
(defvar idee/side-by-side-buffer nil "Secondary buffer")

(defvar idee/repl-kind nil "The kind of the repl buffer. This is framework/lang specific.")
(defvar idee/repl-buffer-prefix nil "The prefix of the repl buffer. This is framework/lang specific.")
(defvar idee/repl-buffer-prompt nil "The prompt of the repl buffer. This is framework/lang specific.")


(defcustom idee/eww-url-default "http://duckduckgo.com" "The default eww url." :group 'idee/view :type 'string)
(defvar idee/eww-url () "The url the browser should point at when opened.")

;;; The following is based on Protesilaos Stavrou configuration: https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/emacs-init.org
(defvar idee/focus-window-configuration nil "Current window configuration.")
(defvar idee/focus-treemacs-visible nil "Treemacs visibility, as idee/window-configuration does not apply to treemacs.")
(defvar idee/focus-fringe-mode fringe-mode)

(define-minor-mode idee/focus-mode
  "Toggle between multiple windows and single window. This is the equivalent of maximising a window."
  :lighter " [M]"
  :global nil
  (if (not (and (boundp 'idee/focus-mode) idee/focus-mode))       ;; If we have idee/focus-mode
      (when idee/focus-window-configuration                       ;; And and exisitng configuration
        (progn                                                    ;; Restore ...
          (set-window-configuration idee/focus-window-configuration)
          (when idee/focus-center-buffer (set-fringe-mode idee/focus-fringe-mode))
          (when (and
                 (equal 'visible idee/focus-treemacs-visible)
                 (boundp 'treemacs-current-visibility)
                 (not (equal 'visible (treemacs-current-visibility)))) (treemacs))))
    ;; Focus
    (progn
      (setq idee/focus-window-configuration (current-window-configuration))
      (setq idee/focus-treemacs-visible (treemacs-current-visibility))
      (delete-other-windows)
      (when idee/focus-center-buffer (set-fringe-mode (/ (- (frame-pixel-width) (* idee/focus-center-buffer-columns (frame-char-width))) 2)))))
  (setq idee/focus-treemacs-visible (and (boundp 'treemacs-current-visibility) (treemacs-current-visibility))))

;; Functions

;;;###autoload
(defun idee/view-reset()
  "Reset view variables.")

;;;###autoload
(defun idee/project-open-view(&optional path)
  "Switch to a traditional IDE view for the buffer.  (project tree, main buffer & terminal)."
  (interactive)
  (let* ((path (or path (or (project-root (project-current (or path default-directory))) default-directory))))
    (dired path)
    (idee/jump-to-non-idee/window)
    (magit-status-init-buffer path)))


;;;###autoload
(defun idee/reset-view()
  "Reset the view by closing all buffers and keep the first non ide buffer available."
  (interactive)
  (setq idee/side-by-side-buffer nil
        idee/side-by-side-active nil)
  (idee/jump-to-non-idee/window ())
  ;; In some cases, it's better to swtich (e.g. when current bufer is a side buffer
  (when (and idee/primary-buffer (idee/idee/buffer-p (buffer-name (current-buffer)))
             (progn
               (message "No project buffer visible, switching to %s." (buffer-name idee/primary-buffer))
               (switch-to-buffer idee/primary-buffer))))
  (delete-other-windows-internal)
  (if (and idee/tree-enabled (not (eq 'visible (treemacs-current-visibility)))
           (progn
             (treemacs--init (project-root (project-current)))
             ;; we remove the mode-line to hide the treemacs label
             (setq mode-line-format nil)))
      (idee/jump-to-non-idee/window)
    (setq idee/primary-buffer (current-buffer))))


(defun idee/open-side-by-side ()
  (interactive)
  "Open a new file in the side buffer."
  (setq idee/side-by-side-buffer nil)
  (idee/switch-side-by-side-on))

;;;###autoload
(defun idee/terminal-view()
  "Maximize terminal in the project root."
  (interactive)
  (idee/project-run-eshell)
  (delete-other-windows-internal))

;;
;;
;; View Mode Helpers

(defun idee/idee/buffer-p (buffer-name)
  "Predicate to check if BUFFER-NAME is an ide buffer (e.g. tree, cli, repl, diagnostics etc)."
  (let ((mode (with-current-buffer buffer-name major-mode))
        (name (string-trim buffer-name)))
    (cond ((provided-mode-derived-p 'prog-mode mode) t)
          ((and (string-prefix-p "*" name)  (string-suffix-p "*" name)) t)
          ((string-prefix-p "magit:" name) t)
          (:else nil))))

(defun idee/window-has-current-buffer-p ()
  "Returns non-nil when the current window display the current buffer."
  (let ((b (current-buffer))
        (w (frame-selected-window)))
    (equal (get-buffer-window b 'visible) w)))

;;;###autoload
(defun idee/jump-to-non-idee/window(&optional visited)
  "Jump to a non IDE window.
VISITED is an optional list with windows already visited."
  (interactive)
  (let* ((visited (or visited '()))
         (buffer (current-buffer))
         (name (buffer-name buffer))
         (idee/buffer (idee/idee/buffer-p name))
         (current-buffer-selected (idee/window-has-current-buffer-p)))

    (cond
     ((not (and idee/buffer current-buffer-selected)) t)
     ((member name visited) nil)
     (t (progn (other-window 1)
               (idee/jump-to-non-idee/window (push name visited)))))))

(defun idee/update-tree-state()
  "Update the state of the tree switch (in case the winodw has been externally closed)."
  (if (equal (treemacs-current-visibility) 'visible)
      (setq idee/tree-active t)
    (setq idee/tree-active nil)))


;;;###autoload
(defun idee/toggle-tree ()
  "Toggle the tree."
  (interactive)
  (if (not (eq 'visible (treemacs-current-visibility)))
      (progn
        (setq idee/tree-enabled t)
        (treemacs))
    (progn
      (setq idee/tree-enabled nil)
      (treemacs))))

;;;###autoload
(defun idee/new-empty-buffer()
  "Create an empty buffer."
  (let ((fl (make-temp-file "Untitled")))
    (switch-to-buffer fl)))

;;;###autoload
(defun idee/split-and-follow-horizontally ()
  "Split window horizontally and follow."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))


;;;###autoload
(defun idee/split-and-follow-vertically ()
  "Split window vertically and follow."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

;;
;; Buffer providers
;;

(defun idee/hydra-visible-window ()
  "Return the hydra window if visible."
  (car (idee/windows-visible-get (lambda (b) (string-prefix-p "*LV" (buffer-name b))))))
(defun idee/hydra-visible-p ()
  "Return non-nil if hydra is visible."
  (idee/hydra-visible-window))
(defun idee/cli-visible-window ()
  "Return the visible cli window."
  (car (idee/windows-visible-get (lambda (b) (string-prefix-p "*eshell" (buffer-name b))))))
(defun idee/cli-visible-p ()
  "Return non-nil if cli is visible."
  (idee/cli-visible-window))
(defun idee/repl-visible-window ()
  "Return the visible repl window."
  (car (idee/windows-visible-get (lambda (b) (and idee/repl-buffer-prefix (string-prefix-p idee/repl-buffer-prefix (buffer-name b)))))))
(defun idee/repl-visible-p ()
  "Return non-nil if repl is visible."
  (idee/repl-visible-window))
(defun idee/diagnostics-visible-window ()
  "Return the visible diagnostics-window."
  (car (idee/windows-visible-get (car (idee/matching-buffer-names "^\*Flymake diagnostics")))))

(defun idee/diagnostics-visible-p ()
  "Return non-nil if diagnostics is visible."
  (idee/diagnostics-visible-window))
(defun idee/errors-visible-window ()
  "Return the errors window if visible."
  (car (idee/windows-visible-get "*Flycheck errors*")))
(defun idee/errors-visible-p ()
  "Return non-nil if error is visible."
  (idee/errors-visible-window))
(defun idee/messages-visible-window ()
  "Return the messages if visible."
  (car (idee/windows-visible-get "*Messages*")))
(defun idee/messages-visible-p ()
  "Return non-nil if messages is visible."
  (idee/messages-visible-window))
(defun idee/eww-visible-window ()
  "Return the eww window if visible."
  (car (idee/windows-visible-get "*eww*")))
(defun idee/eww-visible-p ()
  "Return non-nil if eww is visible."
  (idee/eww-visible-window))
(defun idee/xwidget-webkit-visible-window ()
  "Return the xwidget-webkit window if visible."
  (car (idee/windows-visible-get "*xwidget-webkit*")))
(defun idee/xwidget-webkit-visible-p ()
  "Return non-nil if xwidget-webkit is visible."
  (idee/xwidget-webkit-visible-window))

(defun idee/side-by-side-visible-window ()
  "Return the side-by-side window if visible."
  (and idee/side-by-side-buffer (get-buffer-window idee/side-by-side-buffer 'visible)))

(defun idee/side-by-side-visible-p ()
  "Return non-nil if eww is visible."
  (idee/side-by-side-visible-window))

;;;###autoload
(defun idee/region-copy-to-other-window (start end)
  "Copy selected text from START to END over to other non IDE window."
  (interactive "r")
  (if (use-region-p) 
      (let* ((buffer (current-buffer))
             (name (buffer-name buffer))
             (current-window (selected-window)))
        (save-excursion
          (kill-ring-save start end)
          (other-window 1)
          (idee/jump-to-non-idee/window (list name))
          (evil-end-of-line)
          (evil-insert-newline-below)
          (yank)
          (select-window current-window)))))

;;;###autoload
(defun idee/region-move-to-other-window (start end)
  "Move selected text from START to END over to other non IDE window."
  (interactive "r")
  (if (use-region-p) 
      (let* ((buffer (current-buffer))
             (name (buffer-name buffer))
             (current-window (selected-window)))
        (save-excursion
          (kill-region start end)
          (other-window 1)
          (idee/jump-to-non-idee/window (list name))
          (evil-end-of-line)
          (evil-insert-newline-below)
          (yank)
          (select-window current-window)))))

;;
(defun idee/ediff()
  (interactive)
  (when (not idee/side-by-side-buffer) (idee/open-side-by-side))
  (let* ((left (buffer-file-name idee/primary-buffer))
         (right (buffer-file-name (get-buffer idee/side-by-side-buffer))))
    (ediff-files3 left right "/tmp/ediff-down")))

;; Macros
(defmacro idee/create-view-component (name window-creator window-provider flag)
  "Update the state of the FLAG (in case the winodw has been externally closed).

NAME is the name of the view component.
WINDOW-CREATOR is a function that creator the buffer.
WINDOW-PROVIDER is a function that returns non-nil if buffer is currently visible.
FLAG is the variable that holds the  visibility state of the component (e.g. visible or not visible).
PIVOT indicates how many windows should be switched at the end of the operation."
  (declare (indent 1) (debug t))
  `(progn
     (defun ,(intern (format "idee/update-%s-state" name)) ()
       ,(format "Update the state of the %s (in case the winodw has been externally closed)." name)
       (idee/jump-to-non-idee/window)
       (if (,window-provider)
           (setq ,flag t)
         (setq ,flag nil)))
     
     (defun ,(intern (format "idee/toggle-%s" name)) ()
       ,(format "Toggle the state of the %s." name)
       (interactive)
       (funcall (intern (format "idee/update-%s-state" ,name)))
       (if ,flag
           (progn
             (setq ,flag nil)
             (let ((window (,window-provider)))
               (when window
                 (idee/jump-to-non-idee/window)
                 (delete-window (,window-provider)))))
         (progn
           (setq ,flag t)
           (let ((window (or (,window-provider) (,window-creator))))
             (when window
               (select-window (,window-provider))
               (goto-char (point-max)))))))

     (defun ,(intern (format "idee/switch-%s-on" name)) ()
       ,(format "Switch %s on." name)
       (interactive)
       (funcall (intern (format "idee/update-%s-state" ,name)))
       (if (not ,flag)
           (funcall (intern (format "idee/toggle-%s" ,name)))))))

(defun idee/side-by-side ()
  (interactive)
  "Display the side by side buffer."
  (if (and idee/side-by-side-buffer (get-buffer idee/side-by-side-buffer))
      (display-buffer idee/side-by-side-buffer)
    (progn
      (split-window-horizontally)
      (project-find-file)
      (let* ((name (buffer-name (current-buffer)))
             (actual-name (if (idee/starts-with "*side " name) (substring name 6 (- (length name)  7)) name))
             (side-name (format "*side %s*" actual-name)))
        (setq idee/side-by-side-buffer side-name
              idee/side-by-side-active t)
        (rename-buffer side-name)))))

(defun idee/messages ()
  (interactive)
  "Display the messages buffer."
  (display-buffer "*Messages*"))


(defun idee/eww ()
  (interactive)
  "Open the browser."
  (idee/with-project-settings "eww.el" idee/eww-url
                              (eww (or idee/eww-url idee/eww-url-default))))

(defun idee/xwidget-webkit ()
  (interactive)
  "Open the browser."
  (idee/with-project-settings "eww.el" idee/eww-url
                              (xwidget-webkit-browse-url (or idee/eww-url idee/eww-url-default))))
;;
;; Create component view functions
;;

;;;###autoload (autoload 'idee/toggle-errors "idee-views")
(idee/create-view-component "errors" flycheck-list-errors idee/errors-visible-window idee/errors-active)
;;;###autoload (autoload 'idee/toggle-diagnostics "idee-views")
(idee/create-view-component "diagnostics" flymake-show-diagnostics-buffer idee/diagnostics-visible-window idee/diagnostics-active)
;;;###autoload (autoload 'idee/toggle-cli "idee-views")
;;;###autoload (autoload 'idee/switch-cli-on "idee-views")
(idee/create-view-component "cli" idee/shell-open-in-project idee/shell-visible-window idee/cli-active)
;;;###autoload (autoload 'idee/toggle-repl "idee-views")
;;;###autoload (autoload 'idee/switch-repl-on "idee-views")
(idee/create-view-component "repl" idee/repl idee/repl-visible-window idee/repl-active)
;;;###autoload (autoload 'idee/toggle-messages "idee-views")
(idee/create-view-component "messages" idee/messages  idee/messages-visible-window idee/messages-active)
;;;###autoload (autoload 'idee/toggle-eww "idee-views")
(idee/create-view-component "eww" idee/eww idee/eww-visible-window idee/eww-active)
;;;###autoload (autoload 'idee/toggle-xwidget-webkit "idee-views")
(idee/create-view-component "xwidget-webkit" idee/xwidget-webkit idee/xwidget-webkit-visible-window idee/xwidget-webkit-active)
;;;###autoload (autoload 'idee/toggle-side-by-side "idee-views")
(idee/create-view-component "side-by-side" idee/side-by-side  idee/side-by-side-visible-window idee/side-by-side-active)

;;
;; Repl
;;
(defun idee/repl-buffer-get ()
  "Return first matching repl buffer."
  (car (idee/repl-buffers-get)))

(defun idee/repl-buffers-get ()
  "Return matching repl buffers."
  (seq-filter (lambda (b) (and idee/repl-buffer-prefix (string-prefix-p idee/repl-buffer-prefix (buffer-name b)))) (buffer-list)))

(defun idee/repl-eval-string (s)
  "Evaluate S in the repl."
  (let* ((buffer (idee/repl-buffer-get))
         (process (buffer-name buffer)))
    (when process
      (comint-send-string process (format "%s\n" s))
      (sit-for 0.5)
      (with-current-buffer process
        (goto-char (point-max))
        (beginning-of-line)
        (let* ((end (- (point) 1))
               (start (+ (length idee/repl-buffer-prompt) (search-backward idee/repl-buffer-prompt nil t)))
               (result (buffer-substring start end)))
          result)))))

(defun idee/repl-eval-region (beginning end)
  "Evaluate region in the repl."
  (interactive "r")
  (idee/repl-eval-string (buffer-substring beginning end)))

(defun idee/repl-eval-region-with-tooltip (beginning end)
  "Evaluate region in the repl and show result in a tooltip."
  (interactive "r")
  (let ((result (string-trim (idee/repl-eval-region beginning end))))
    (if (idee/string-blank result) (tooltip-show "Ok")
      (tooltip-show result))))

(defun idee/display-buffer-alist-contains (str)
  "Return non-nil if display-buffer-alsit has entry that contains STR."
  (seq-filter (lambda (s) (and (stringp s) (idee/contains-string s str))) (mapcar (lambda (e) (car e)) display-buffer-alist)))

(defun idee/views-init ()
  "Setup `display-buffer-alist`."
  (when idee/display-buffer-setup-enabled
    (when (and idee/popper-enabled (boundp  'popper-display-control-p))
      (setq display-buffer-alist (add-to-list 'display-buffer-alist `(popper-display-control-p (`popper-display-function))))

    (when (not (idee/display-buffer-alist-contains "*undo-tree"))
      (setq display-buffer-alist (add-to-list 'display-buffer-alist 
                                              `("\\*undo-tree\\*"
                                                (display-buffer-in-side-window)
                                                (window-width . 0.10)
                                                (side . right)
                                                (slot . 0)))))

    (when (not (idee/display-buffer-alist-contains "*side"))
      (setq display-buffer-alist (add-to-list 'display-buffer-alist 
                                              `("\\*side .*\\*"
                                                (display-buffer-in-side-window)
                                                (window-width . 0.50)
                                                (side . right)
                                                (slot . 1)))))

    (when (not (idee/display-buffer-alist-contains "*eww"))
      (setq display-buffer-alist (add-to-list 'display-buffer-alist 
                                              `("\\*eww\\*"
                                                (display-buffer-in-side-window)
                                                (window-width . 0.30)
                                                (side . right)
                                                (slot . 2)))))

    (when (not (idee/display-buffer-alist-contains "shell"))
      (setq display-buffer-alist (add-to-list 'display-buffer-alist 
                                              `("\\*\\(Async [s\\|S]hell [c\\|C]ommand.*\\|eshell.*\\|shell.*\\|vterm.*\\|xref\\|.*compilation\\)\\*"
                                                (display-buffer-in-side-window)
                                                (window-height . 0.40)
                                                (side . bottom)
                                                (slot . 0)))))

    (when (not (idee/display-buffer-alist-contains "Flycheck"))
      (setq display-buffer-alist (add-to-list 'display-buffer-alist 
                                              `("\\*\\(Flycheck errors\\|Flymake diagnostics for .*\\)\\*"
                                                (display-buffer-in-side-window)
                                                (window-height . 0.20)
                                                (side . bottom)
                                                (slot . 1)))))

    (when (not (idee/display-buffer-alist-contains "*Messages"))
      (setq display-buffer-alist (add-to-list 'display-buffer-alist 
                                              `("\\*Messages\\*"
                                                (display-buffer-in-side-window)
                                                (window-height . 0.20)
                                                (side . bottom)
                                                (slot . 1)))))

    (when (not (idee/display-buffer-alist-contains "*Warnings"))
      (setq display-buffer-alist (add-to-list 'display-buffer-alist 
                                              `("\\*\\(Warnings\\|Backtrace\\)\\*"
                                                (display-buffer-in-side-window)
                                                (window-height . 0.20)
                                                (side . bottom)
                                                (slot . 2)))))

    (when (not (idee/display-buffer-alist-contains "*Org QL"))
      (setq display-buffer-alist (add-to-list 'display-buffer-alist 
                                              `("\\*Org QL View: Github issues for .*\\*"
                                                (display-buffer-in-side-window)
                                                (window-height . 0.20)
                                                (side . bottom)
                                                (slot . 2))))))))

(provide 'idee-views)
;;; idee-views.el ends here
