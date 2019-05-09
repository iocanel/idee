;;; idee-eshell.el --- Eshell integration  -*- lexical-binding: t -*-



;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(require 'queue)
(require 'async-await)

(defvar idee-eshell-command-queue (queue-create))
(defvar idee-eshell-command-running nil)

(defcustom idee-eshell-demo-it-enabled nil "Demo-it for eshell feature toggle" :group 'idee :type 'string)
(defcustom idee-eshell-demo-it-speed :fast "Demo-it for eshell typing speed" :group 'idee   :type '(choice (const :tag "fast" :fast)
                 (const :tag "faster"  :faster)
                 (const :tag "medium"  :medium)
                 (const :tag "slow"    :slow)
                 (const :tag "instant" :instant)))

(defun wait-async (n)
  (promise-new (lambda (resolve _reject) (run-at-time n nil (lambda () (funcall resolve n))))))

(defun idee-eshell-cleanup  ()
  "Cleanup eshell queues and flags."
  (interactive)
  (setq idee-eshell-command-queue (queue-create))
  (setq idee-eshell-command-running nil))

(defun idee-eshell-command-started ()
  "Mark that an eshell command is running."
  (setq idee-eshell-command-running t))

(async-defun idee-eshell-command-finished ()
  "Mark that an eshell command is running."

 (await (wait-async 0.1))
 ; TODO: comment out until we have a working version of idee-eshell-prompt-ready-p
 ; (let ((i 0)
 ;       (ready (idee-eshell-prompt-ready-p)))
 ;   (while (and (< i 100)(not ready))
 ;     (await (wait-async 0.1))
 ;     (setq i (+ i 1))
 ;     (setq ready (idee-eshell-prompt-ready-p))))

  (idee-eshell-execute-next-command)
  (setq idee-eshell-command-running (not (queue-empty idee-eshell-command-queue))))

(defun idee-eshell-prompt-promise (n)
  (promise-new (lambda (resolve _reject) ((if (idee-eshell-ready-prompt-p) (funcall resove n) (funcall _reject))))))

;;
;; TODO: This is still buggy, as it appears that `idee-eshell-command-running` is getting nil value prematurely.
(async-defun idee-eshell-await-command-finished ()
  "Wait until eshell finishes executing all queued commands."
  (interactive)
  (let ((running idee-eshell-command-running))
    (while running
      (await (wait-async 0.1))
      (setq running idee-eshell-command-running)))
      (await (wait-async 0.1)))
                 
(defun idee-eshell-prompt-ready-p ()
  "Return non-nil if the prompt is visible."
  (interactive)
  (with-current-buffer (format "*eshell %s*" (projectile-project-name))
    (let ((current (point))
          (col (current-column))
          (found (get-text-property (point) 'history)))
      (save-excursion
        (while  (and (> col 0) (not found))
          (goto-char (- (point) 1))
          (setq col (- col 1))
          (setq found (get-text-property (point) 'history))))
        found)))

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

(defmacro idee-with-project-shell (&rest body)
  "Load a SETTINGS-FILE as local OPTIONS and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ()
  (idee-switch-cli-on) 
  (with-current-buffer (format "*eshell %s*" (projectile-project-name))
    (let ((comint-scroll-to-bottom-on-output t))
     (eshell/clear-scrollback)
      (eshell-send-input)
      (eshell-return-to-prompt)
      ,@body
      (eshell-send-input)))))

(defun idee-eshell-project-command-execute (command)
  "Run a single COMMAND in the current project shell."
  (idee-switch-cli-on)
  (with-current-buffer (format "*eshell %s*" (projectile-project-name))
    (let ((comint-scroll-to-bottom-on-output t))
      (eshell-return-to-prompt)
      (idee-eshell-insert command)
      (eshell-send-input))))

(defun idee-eshell-project-command-enqueue (commands)
  "Execute COMMANDS on eshell."
  (idee-switch-cli-on)
  (with-current-buffer (format "*eshell %s*" (projectile-project-name))
    (let ((comint-scroll-to-bottom-on-output t)
          (eshell-scroll-to-bottom-on-input t))
      (dolist (cmd (if (listp commands) commands (list commands)))
        (queue-enqueue idee-eshell-command-queue cmd))
      (when (not idee-eshell-command-running) (idee-eshell-execute-next-command)))))

(defun idee-eshell-insert (str)
  "Insert STR into the current project eshell buffer."
  (if (and idee-eshell-demo-it-enabled (require 'demo-it nil t))
      (demo-it-insert str idee-eshell-demo-it-speed)
    (insert str)))


(advice-add 'eshell-command-started :before 'idee-eshell-command-started)
(advice-add 'eshell-command-finished :after 'idee-eshell-command-finished)
 
(provide 'idee-eshell)
;;; idee-eshell.el ends here
