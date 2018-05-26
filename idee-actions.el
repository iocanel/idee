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

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(require 'idee-vars)
(require 'idee-navigation)

;;Project
(defun idee-open()
  "Open project"
  (interactive)
  (idee-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-recent()
  "Recent project"
  (interactive)
  (idee-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-save-all()
  "Save all project buffers"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-close()
  "Close project"
  (interactive)
  (delete-other-windows-internal)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-build()
  "Build"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-vcs()
  "Version Control"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )


;;Source
(defun idee-optimize-imports()
  "Optimize Imports"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-indent()
  "Indent"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-indent-region()
  "Indent Region"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-license-headers()
  "Apply license headers"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )

;;Navigate
(defun idee-references()
  "Find references"
  (interactive)
  (idee-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-declaration()
  "Jump to declaration"
  (interactive)
  (idee-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-back()
  "Jump back"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )

;;Search
(defun idee-grep()
  "Grep"
  (interactive)
  (let (b (buffer-name (window-buffer)))
    (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
    (other-window 1)
    (while (and
            (not (equal "*grep*" (buffer-name (window-buffer))))
            (not (equal b (buffer-name (window-buffer)))))
      (other-window 1))
    ))


(defun idee-find-file()
  "Find file"
  (interactive)
  (idee-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )

(defun idee-find-variable()
  "Find variable"
  (interactive)
  (idee-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )

;;Task

(defun idee-run-or-eval ()
  "Run the project"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )
(defun idee-test()
  "Runs tests"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )

;;Layout
(defun idee-repl ()
  "Run a REPL"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )

(defun idee-mode-hydra ()
  "Open the mode specific hydra"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) idee-function-alist))
  )

;; Toggles
(defun idee-toggle-tab-width ()
  "Toggle between 2 and 4 space indentation."
  (interactive)
  (if (equal 4 idee-tab-width)
      (setq idee-tab-width 2)
    (setq idee-tab-width 4))

  (setq tab-width idee-tab-width)
  (idee-set-tab-width)
  )

(defun idee-set-tab-width ()
  "Set the tab width."
  (funcall  (alist-get 'idee-mode-tab-width-function idee-function-alist) idee-tab-width)
  (message (format "Indentation spaces: %s" idee-tab-width))
  )

(defun idee-global-set-tab-width-function (width)
  "Set the tab WIDTH."
  (setq standard-indent width)
  )

(defun idee-toggle-use-tabs ()
    "Toggle between tabs and spaces."
  (interactive)
  (if idee-use-tabs
      (setq idee-use-tabs nil)
    (setq idee-use-tabs t))

  (setq indent-tabs-mode idee-use-tabs)
  (if idee-use-tabs
      (message "Use tabs: enabled.")
    (message "Use tabs: disabled."))
  )



(provide 'idee-actions)
;;; idee-actions.el ends here
