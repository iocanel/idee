;;; eide-actions.el --- IDE actions

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
(require 'eide-navigation)

;;Project
(defun eide-open()
  "Open project"
  (interactive)
  (eide-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-recent-project()
  "Recent project"
  (interactive)
  (eide-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-save-all()
  "Save all project buffers"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-close()
  "Close project"
  (interactive)
  (delete-other-windows-internal)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-build()
  "Build"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-vcs()
  "Version Control"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )


;;Source
(defun eide-optimize-imports()
  "Optimize Imports"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-indent()
  "Indent"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-indent-region()
  "Indent Region"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-license-headers()
  "Apply license headers"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )

;;Navigate
(defun eide-references()
  "Find references"
  (interactive)
  (eide-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-declaration()
  "Jump to declaration"
  (interactive)
  (eide-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-back()
  "Jump back"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )

;;Search
(defun eide-grep()
  "Grep"
  (interactive)
  (let (b (buffer-name (window-buffer)))
    (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
    (other-window 1)
    (while (and
            (not (equal "*grep*" (buffer-name (window-buffer))))
            (not (equal b (buffer-name (window-buffer)))))
      (other-window 1))
    ))


(defun eide-find-file()
  "Find file"
  (interactive)
  (eide-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )

(defun eide-find-variable()
  "Find variable"
  (interactive)
  (eide-back-push)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )

;;Task

(defun eide-run-or-eval ()
  "Run the project"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )
(defun eide-test()
  "Runs tests"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )

;;Layout
(defun eide-repl ()
  "Run a REPL"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )

(defun eide-mode-hydra ()
  "Open the mode specific hydra"
  (interactive)
  (funcall  (alist-get (intern (format "%s-function" this-command)) eide-function-alist))
  )

;; Toggles
(defun eide-toggle-tab-width ()
  "Toggle between 2 and 4 space indentation."
  (interactive)
  (if (equal 4 eide-tab-width)
      (setq eide-tab-width 2)
    (setq eide-tab-width 4))

  (setq tab-width eide-tab-width)
  (message (format "Indentaion spaces: %s" eide-tab-width))
  )

(defun eide-toggle-use-tabs ()
  "Toggle between tabs and speacs"
  (interactive)
  (if eide-use-tabs
      (setq eide-use-tabs nil)
    (setq eide-use-tabs t))

  (setq indent-tabs-mode eide-use-tabs)
  (if eide-use-tabs
      (message "Use tabs: enabled.")
    (message "Use tabs: disabled."))
  )


(provide 'eide-actions)
;;; eide-actions.el ends here
