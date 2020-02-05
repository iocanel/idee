;;; idee-actions.el --- IDE actions

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

(require 'helm-ag)
(require 'idee-vars)

;;Project
;;;###autoload
(defun idee-open()
  "Open project."
  (interactive)
  (idee-back-push)
  (funcall  (alist-get 'idee-open-function idee-function-alist)))

;;;###autoload
(defun idee-new-project()
  "Create a new file."
  (interactive)
  (idee-back-push)
  (funcall  (alist-get 'idee-new-project-function idee-function-alist)))

;;;###autoload
(defun idee-new-file()
  "Create a new file."
  (interactive)
  (idee-back-push)
  (funcall  (alist-get 'idee-new-file-function idee-function-alist)))

;;;###autoload
(defun idee-recent()
  "Recent project."
  (interactive)
  (idee-back-push)
  (funcall  (alist-get 'idee-recent-function idee-function-alist)))

;;;###autoload
(defun idee-save-all()
  "Save all project buffers."
  (interactive)
  (funcall  (alist-get 'idee-save-all-function idee-function-alist)))

;;;###autoload
(defun idee-close()
  "Close project."
  (interactive)
  (delete-other-windows-internal)
  (funcall  (alist-get 'idee-close-function idee-function-alist)))

;;;###autoload
(defun idee-build()
  "Build."
  (interactive)
  (funcall  (alist-get 'idee-build-function idee-function-alist)))

;;;###autoload
(defun idee-vcs()
  "Version Control."
  (interactive)
  (funcall  (alist-get 'idee-vcs-function idee-function-alist)))

;;Source
;;;###autoload
(defun idee-optimize-imports()
  "Optimize Imports."
  (interactive)
  (funcall  (alist-get 'idee-optimize-imports-function idee-function-alist)))

;;;###autoload
(defun idee-indent()
  "Indent."
  (interactive)
  (funcall  (alist-get 'idee-indent-function idee-function-alist)))

;;;###autoload
(defun idee-indent-region()
  "Indent Region."
  (interactive)
  (funcall  (alist-get 'idee-indent-region-function idee-function-alist)))

;;;###autoload
(defun idee-select-project-header()
  "Select header for project files."
  (interactive)
  (funcall  (alist-get 'idee-select-project-header-function idee-function-alist)))

(defun idee-apply-code-actions()
  "Select header for project files."
  (interactive)
  (funcall  (alist-get 'idee-apply-code-actions-function idee-function-alist)))

;;Navigate
;;;###autoload
(defun idee-references()
  "Find references."
  (interactive)
  (idee-back-push)
  (funcall  (alist-get 'idee-references-function idee-function-alist)))

;;;###autoload
(defun idee-implementation()
  "Find implementations."
  (interactive)
  (idee-back-push)
  (funcall  (alist-get 'idee-implementation-function idee-function-alist)))

;;;###autoload
(defun idee-declaration()
  "Jump to declaration."
  (interactive)
  (idee-back-push)
  (funcall  (alist-get 'idee-declaration-function idee-function-alist)))

;;;###autoload
(defun idee-back()
  "Jump back."
  (interactive)
  (funcall  (alist-get 'idee-back-function idee-function-alist)))

;;Search
;;;###autoload
(defun idee-grep()
  "Grep."
  (interactive)
  (let (b (buffer-name (window-buffer)))
    (helm-do-ag (projectile-project-root))
    (other-window 1)
    (while (and
            (not (equal "*grep*" (buffer-name (window-buffer))))
            (not (equal b (buffer-name (window-buffer)))))
      (other-window 1))))

;;;###autoload
(defun idee-find-file()
  "Find file."
  (interactive)
  (idee-back-push)
  (funcall  (alist-get 'idee-find-file-function idee-function-alist)))

;;;###autoload
(defun idee-find-variable()
  "Find variable."
  (interactive)
  (idee-back-push)
  (funcall  (alist-get 'idee-find-variable-function idee-function-alist)))

;;Task
;;;###autoload
(defun idee-run-or-eval ()
  "Run the project."
  (interactive)
  (funcall  (alist-get 'idee-run-or-eval-function idee-function-alist)))

;;;###autoload
(defun idee-test()
  "Run test."
  (interactive)
  (funcall  (alist-get 'idee-test-function idee-function-alist)))

;;Layout
;;;###autoload
(defun idee-repl ()
  "Run a REPL."
  (interactive)
  (funcall  (alist-get 'idee-repl-function idee-function-alist)))

;;;###autoload
(defun idee-mode-hydra ()
  "Open the mode specific hydra."
  (interactive)
  (funcall  (alist-get 'idee-mode-hydra-function idee-function-alist)))

;; Toggles
;;;###autoload
(defun idee-toggle-tab-width ()
  "Toggle between 2 and 4 space indentation."
  (interactive)
  (if (equal 4 idee-tab-width)
      (setq idee-tab-width 2)
    (setq idee-tab-width 4))

  (setq tab-width idee-tab-width)
  (idee-set-tab-width))

;;;###autoload
(defun idee-set-tab-width ()
  "Set the tab width."
  (funcall  (alist-get 'idee-mode-tab-width-function idee-function-alist) idee-tab-width)
  (message (format "Indentation spaces: %s" idee-tab-width)))

;;;###autoload
(defun idee-global-set-tab-width-function (width)
  "Set the tab WIDTH."
  (setq standard-indent width
        tab-width width
        evil-shift-width width
        c-basic-offset width))

;;;###autoload
(defun idee-toggle-use-tabs ()
    "Toggle between tabs and spaces."
  (interactive)
  (if idee-use-tabs
      (setq idee-use-tabs nil
            evil-indent-convert-tabs t)
    (setq idee-use-tabs t
          evil-indent-convert-tabs nil))

  (setq indent-tabs-mode idee-use-tabs)
  (if idee-use-tabs
      (message "Use tabs: enabled.")
    (message "Use tabs: disabled.")))

;;;###autoload
(defun idee-execute-code-actions ()
  (let ((action) (list (lsp--select-action (lsp-code-actions-at-point))))
    (lsp-execute-code-action action)))

(provide 'idee-actions)
;;; idee-actions.el ends here
