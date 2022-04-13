;; idee-actions.el --- IDE actions  -*- lexical-binding: t -*-

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
(require 'idee-navigation)

;;
;; Optional
;;
(require 'helm-ag nil t)
(require 'cc-vars nil t)
(require 'evil-vars nil t)

;; 
;;Project
;;;###autoload
(defun idee/open()
  "Open project."
  (interactive)
  (idee/back-push)
  (funcall  (alist-get 'idee/open-function idee/function-alist)))

;;;###autoload
(defun idee/new-project()
  "Create a new file."
  (interactive)
  (idee/back-push)
  (funcall  (alist-get 'idee/new-project-function idee/function-alist)))

;;;###autoload
(defun idee/new-file()
  "Create a new file."
  (interactive)
  (idee/back-push)
  (funcall  (alist-get 'idee/new-file-function idee/function-alist)))

;;;###autoload
(defun idee/recent()
  "Recent project."
  (interactive)
  (idee/back-push)
  (funcall  (alist-get 'idee/recent-function idee/function-alist)))

;;;###autoload
(defun idee/save-all()
  "Save all project buffers."
  (interactive)
  (funcall  (alist-get 'idee/save-all-function idee/function-alist)))

;;;###autoload
(defun idee/close()
  "Close project."
  (interactive)
  (delete-other-windows-internal)
  (funcall  (alist-get 'idee/close-function idee/function-alist)))

;;;###autoload
(defun idee/build()
  "Build."
  (interactive)
  (funcall  (alist-get 'idee/build-function idee/function-alist)))

;;;###autoload
(defun idee/vcs()
  "Version Control."
  (interactive)
  (funcall  (alist-get 'idee/vcs-function idee/function-alist)))

;;Source
;;;###autoload
(defun idee/optimize-imports()
  "Optimize Imports."
  (interactive)
  (funcall  (alist-get 'idee/optimize-imports-function idee/function-alist)))

;;;###autoload
(defun idee/indent()
  "Indent."
  (interactive)
  (funcall  (alist-get 'idee/indent-function idee/function-alist)))

;;;###autoload
(defun idee/indent-region()
  "Indent Region."
  (interactive)
  (funcall  (alist-get 'idee/indent-region-function idee/function-alist)))

;;;###autoload
(defun idee/header-select()
  "Select header for project files."
  (interactive)
  (funcall  (alist-get 'idee/header-select-function idee/function-alist)))

(defun idee/apply-code-actions()
  "Select header for project files."
  (interactive)
  (funcall  (alist-get 'idee/apply-code-actions-function idee/function-alist)))

;;Navigate
;;;###autoload
(defun idee/references()
  "Find references."
  (interactive)
  (idee/back-push)
  (funcall  (alist-get 'idee/references-function idee/function-alist)))

;;;###autoload
(defun idee/implementation()
  "Find implementations."
  (interactive)
  (idee/back-push)
  (funcall  (alist-get 'idee/implementation-function idee/function-alist)))

;;;###autoload
(defun idee/declaration()
  "Jump to declaration."
  (interactive)
  (idee/back-push)
  (funcall  (alist-get 'idee/declaration-function idee/function-alist)))

;;;###autoload
(defun idee/back()
  "Jump back."
  (interactive)
  (funcall  (alist-get 'idee/back-function idee/function-alist)))

;;Search
;;;###autoload
(defun idee/grep()
  "Grep."
  (interactive)
  (funcall  (alist-get 'idee/grep-function idee/function-alist)))

;;;###autoload
(defun idee/find-file()
  "Find file."
  (interactive)
  (idee/back-push)
  (funcall  (alist-get 'idee/find-file-function idee/function-alist)))

;;;###autoload
(defun idee/find-variable()
  "Find variable."
  (interactive)
  (idee/back-push)
  (funcall  (alist-get 'idee/find-variable-function idee/function-alist)))

;;Task
;;;###autoload
(defun idee/run-or-eval ()
  "Run the project."
  (interactive)
  (funcall  (alist-get 'idee/run-or-eval-function idee/function-alist)))

;;;###autoload
(defun idee/test()
  "Run test."
  (interactive)
  (funcall  (alist-get 'idee/test-function idee/function-alist)))

;;Layout
;;;###autoload
(defun idee/repl ()
  "Run a REPL."
  (interactive)
  (let ((repl-funcition (alist-get 'idee/repl-function idee/function-alist)))
    (if repl-funcition (funcall repl-funcition)
      (message "No repl available for mode."))))

;;;###autoload
(defun idee/mode-hydra ()
  "Open the mode specific hydra."
  (interactive)
  (funcall  (alist-get 'idee/mode-hydra-function idee/function-alist)))

;; Toggles
;;;###autoload
(defun idee/toggle-tab-width ()
  "Toggle between 2 and 4 space indentation."
  (interactive)
  (if (equal 4 idee/tab-width)
      (setq idee/tab-width 2)
    (setq idee/tab-width 4))

  (setq tab-width idee/tab-width)
  (idee/set-tab-width))

;;;###autoload
(defun idee/set-tab-width ()
  "Set the tab width."
  (let ((f (alist-get 'idee/mode-tab-width-function idee/function-alist)))
    (if f
        (funcall f idee/tab-width)
      (idee/global-set-tab-width-function idee/tab-width))))

;;;###autoload
(defun idee/global-set-tab-width-function (width)
  "Set the tab WIDTH."
  (when (and (require 'cc-vars nil t) (require 'evil-vars nil t))
    (setq standard-indent width
          tab-width width
          evil-shift-width width
          c-basic-offset width)))

;;;###autoload
(defun idee/toggle-use-tabs ()
  "Toggle between tabs and spaces."
  (interactive)
  (when (require 'evil-vars nil t)
    (if idee/use-tabs
        (setq evil-indent-convert-tabs t)
      (setq evil-indent-convert-tabs nil)))

  (if idee/use-tabs (setq idee/use-tabs nil) (setq idee/use-tabs t))
  (setq indent-tabs-mode idee/use-tabs)
  (if idee/use-tabs
      (message "Use tabs: enabled.")
    (message "Use tabs: disabled.")))

;;;###autoload
(defun idee/execute-code-actions ()
  "Execute code actions."
  (interactive)
  (funcall  (alist-get 'idee/execute-code-actions-function idee/function-alist)))

(defun idee/shell-command-execute-in-project (command &optional new-shell)
  "Run a single COMMAND in the current project shell.
   When NEW-SHELL is specified the old eshell project buffer is killed."
  (funcall  (alist-get 'idee/shell-command-execute-in-project-function idee/function-alist) command new-shell))

(defun idee/shell-visible-window ()
  "Return the visible shell window."
  (funcall  (alist-get 'idee/shell-visible-window-function idee/function-alist)))

(defun idee/shell-open-in-project ()
  "Return the visible shell window."
  (funcall  (alist-get 'idee/shell-open-in-project-function idee/function-alist)))

(defmacro idee/shell-in-project (&rest body)
  "Load a SETTINGS-FILE as local OPTIONS and evaluate BODY."
  (declare (indent 1) (debug t))
  `(let ()
  (with-temp-buffer
      ,@body
      (let* ((begin (point-min))
             (end (point-max))
             (content (buffer-substring-no-properties begin end)))
        (mapc (lambda (l) (when (not (idee/string-blank l)) (idee/shell-command-execute-in-project l))) (split-string content "\n"))))))

(provide 'idee-actions)
;;; idee-actions.el ends here
