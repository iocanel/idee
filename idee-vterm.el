;;; idee-vterm.el --- Vterm integration -*- lexical-binding: t -*-

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

(require 'vterm)
(require 'projectile)

(defun ide-vterm-open-in-project ()
  "Open vterm in project root."
  (interactive)
  (let* ((name (format "*vterm %s*" (projectile-project-name)))
         (buf (get-buffer name))
         (window (if buf (get-buffer-window buf) nil))
         (default-directory (projectile-project-root)))
    (cond (window (progn
             (select-window window)))
          (buf (progn
                 (idee-split-and-follow-vertically)
                 (set-window-buffer (get-buffer-window) buf)))
          (t (progn
               (vterm)
               (rename-buffer name))))))

;;;###autoload
(defun ide-vterm-command-execute-in-project (command &optional new-shell)
  "Run a single COMMAND in the current project shell.
   When NEW-SHELL is specified the old eshell project buffer is killed."
  (let* ((name (format "*vterm %s*" (projectile-project-name)))
         (buf (get-buffer name))
         (comint-scroll-to-bottom-on-output t))
    (when (and buf new-shell)
      ;; Let's didsable the process-kill-buffer-query-function before we kill the buffer
      ;; or we are going to get a very annoyng message.
      (let ((kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
        (kill-buffer buf)))
    (ide-vterm-open-in-project)
    (when (not (idee-string-blank command))
      (vterm-insert command))
    (vterm-send-return)))

(defun ide-vterm-visible-window ()
  "Return the visible vterm window."
  (car (idee-windows-visible-get (lambda (b) (string-prefix-p "*vterm" (buffer-name b))))))


(defun ide-vterm-enable ()
  "Enable vterm."
  (interactive)
  ;;Clear existing associations
  (setq idee-function-alist (delq (assoc 'idee-shell-command-execute-in-project-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-shell-visible-window-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-shell-open-in-project-function idee-function-alist) idee-function-alist))
  ;; Register vterm functions
  (add-to-list 'idee-function-alist '(idee-shell-command-execute-in-project-function . ide-vterm-command-execute-in-project))
  (add-to-list 'idee-function-alist '(idee-shell-visible-window-function . ide-vterm-visible-window))
  (add-to-list 'idee-function-alist '(idee-shell-open-in-project-function . ide-vterm-open-in-project)))

(provide 'idee-vterm)
;;; idee-vterm.el ends here
