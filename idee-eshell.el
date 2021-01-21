;;; idee-eshell.el --- Eshell integration -*- lexical-binding: t -*-

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

(require 'queue)
(require 'eshell)
(require 'esh-mode)
(require 'em-alias)
(require 'em-hist)
(require 'demo-it)
(require 'projectile)


(defvar idee-eshell-command-queue (queue-create))
(defvar idee-eshell-command-inserting nil)
(defvar idee-eshell-command-running nil)
(defvar idee-eshell-initialized nil)

(defcustom idee-eshell-cat-alias-enabled t "/dev/clip aware cat alias toggle." :group 'idee-eshell :type 'boolean)
(defcustom idee-eshell-edit-alias-enabled t "Edit alias toggle." :group 'idee-eshell :type 'boolean)
(defcustom idee-eshell-save-on-shell-enabled t "Save on shell toggle.  Save on shell will save all buffers each time the shell is used." :group 'idee-eshell :type 'boolean)
(defcustom idee-eshell-demo-it-enabled nil "Demo-it for eshell feature toggle." :group 'idee-eshell :type 'string)
(defcustom idee-eshell-demo-it-speed :fast "Demo-it for eshell typing speed." :group 'idee-eshell  :type '(choice (const :tag "fast" :fast)
                 (const :tag "faster"  :faster)
                 (const :tag "medium"  :medium)
                 (const :tag "slow"    :slow)
                 (const :tag "instant" :instant)))

(defun idee-eshell-cleanup  ()
  "Cleanup eshell queues and flags."
  (interactive)
  (setq idee-eshell-command-queue (queue-create))
  (setq idee-eshell-command-running nil))

(defun idee-eshell-command-started ()
  "Mark that an eshell command is running."
  (setq idee-eshell-command-running t))

(defun idee-eshell-command-finished ()
  "Mark that an eshell command is running."

  ;; The first time this function is called will be before the shell is even intialized.
  (if (not idee-eshell-initialized)
      (setq idee-eshell-initialized t)

  (run-with-timer 1 nil (lambda ()
                            (progn
                              (when (and (not idee-eshell-command-inserting) idee-eshell-command-running)
                                (idee-eshell-execute-next-command))
                              (setq idee-eshell-command-running (and idee-eshell-command-running (not (queue-empty idee-eshell-command-queue)))))))))

(defun idee-eshell-execute-next-command ()
  "Execute the next command found in the queue."
  (interactive)
  (setq idee-eshell-command-running t)
  (let* ((cmd (queue-dequeue idee-eshell-command-queue))
         (should-ignore (equal 'ignore cmd)))
    ;; dequeue one more if you have two.
    (if should-ignore
        (progn
          (setq cmd (queue-dequeue idee-eshell-command-queue))
          (setq should-ignore (equal 'ignore cmd))))

    (when (not should-ignore)
      (when cmd (idee-eshell-project-command-execute cmd)))
    should-ignore))

;;;###autoload (autoload 'idee-with-project-shell "idee-eshell")
(defmacro idee-with-project-shell (&rest body)
  "Load a SETTINGS-FILE as local OPTIONS and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ()
  (when idee-eshell-save-on-shell-enabled (idee-save-all))
  (idee-switch-cli-on)
  (with-current-buffer (format "*eshell %s*" (projectile-project-name))
    (let ((comint-scroll-to-bottom-on-output t))
      (eshell-send-input)
      (eshell-return-to-prompt)
      ,@body
      (eshell-send-input)))))

;;;###autoload
(defun idee-eshell-project-command-execute (command &optional new-shell)
  "Run a single COMMAND in the current project shell.
   When NEW-SHELL is specified the old eshell project buffer is killed."
  (let* ((name (format "*eshell %s*" (projectile-project-name)))
         (buf (get-buffer name))
         (eshell-scroll-to-bottom-on-input t)
         (comint-scroll-to-bottom-on-output t))
    (when (and buf new-shell) (kill-buffer buf))
    (idee-switch-cli-on)
    (with-current-buffer name
        (eshell-return-to-prompt)
        (eshell-wait-for-process)
        (idee-eshell-insert command)
        (eshell-send-input))))

;;;###autoload
(defun idee-eshell-project-command-enqueue (commands)
  "Execute COMMANDS on eshell."
  (idee-switch-cli-on)
  (with-current-buffer (format "*eshell %s*" (projectile-project-name))
    (let ((comint-scroll-to-bottom-on-output t)
          (eshell-scroll-to-bottom-on-input t))
      (dolist (cmd (if (listp commands) commands (list commands)))
        (when (not (idee-string-blank cmd)) (queue-enqueue idee-eshell-command-queue cmd)))
      (when (not idee-eshell-command-running) (idee-eshell-execute-next-command)))))


;;;###autoload
(defun idee-eshell-insert (str)
  "Insert STR into the current project eshell buffer."

  ;; Sometimes eshell decided to insert the last command when trying to insert the new one.
  ;; Not, sure exactly why this happens, but let's kill the line if it does.
  ;; This also helps, if left over chars or half written commands are there.
  (setq idee-eshell-command-inserting t)
  (when (< (point) (point-max)) (kill-line))

  (if (and idee-eshell-demo-it-enabled (require 'demo-it nil t))
      (demo-it-insert str idee-eshell-demo-it-speed)
    (insert str))

  (setq idee-eshell-command-inserting nil))

;;
;; Aliases
;;
(defun idee-eshell-cat (f)
  "Display the contents of file F."
  (if (equal f "/dev/clip")
      (current-kill 0)
    (idee-read-file f)))

(advice-add 'eshell-command-started :before 'idee-eshell-command-started)
;(advice-add 'eshell-command-finished :after 'idee-eshell-command-finished)
(add-hook 'eshell-after-prompt-hook 'idee-eshell-command-finished)
(add-hook 'eshell-after-prompt-hook 'idee-eshell-command-finished)

(when idee-eshell-cat-alias-enabled
  (add-hook 'eshell-mode-hook (lambda () (eshell/alias "cat" "idee-eshell-cat $1"))))

(defun idee-eshell-edit (&rest files)
  "Edit the the specified FILES."
  (let ((file (car files))
        (remaining (cdr files)))
    (idee-refresh-view)
    (idee-jump-to-non-ide-window)
    (find-file file)
    (mapc (lambda (f) (split-window-horizontally) (find-file f)) remaining))
    ;; We don't want to return anything back, so let's just return nil.
    nil)

(when idee-eshell-edit-alias-enabled
  (add-hook 'eshell-mode-hook (lambda () (eshell/alias "edit" "idee-eshell-edit $*"))))

(defun idee-eshell-open (file)
  "Edit the the specified FILE."
  (let* ((is-directory (file-directory-p file))
         (path (expand-file-name file))
         (git (concat (file-name-as-directory path) ".git")))
    (if is-directory
        (progn
          (setq default-directory path)
          (when (not (file-exists-p git)) (shell-command "git init"))
          (projectile-add-known-project path)
          (setq projectile-project-root path)
          (projectile-switch-project-by-name path)
          (idee-refresh-view))
      (idee-eshell-edit path))))

(when idee-eshell-edit-alias-enabled
  (add-hook 'eshell-mode-hook (lambda () (eshell/alias "open" "idee-eshell-open $1"))))

(defun idee-eshell-find-web-port ()
  "Find the web port in the current buffer, or return 8080 if none is found."
  (let* ((name (format "*eshell %s*" (projectile-project-name)))
         (buffer (get-buffer name)))
    (if buffer
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (if (re-search-backward "port.* \\([0-9]+\\)" nil t)
                (match-string 1)
              8080)))
      8080)))


(provide 'idee-eshell)
;;; idee-eshell.el ends here
