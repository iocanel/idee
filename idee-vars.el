;;; idee-vars.el ---  Custom and Variables

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


;; Project factories

(cl-defstruct idee-project-factory
  name
  description
  func
  )

(defconst idee-cask-project-factory (make-idee-project-factory
  :name "Cask"
  :description "Create an elisp project based on Cask."
  :func ()))

(defvar idee-project-factories-list `(,idee-cask-project-factory))

;; Back and Forth Navigation
(cl-defstruct idee-buffer-point
  buffer
  line
  column
  )
(defvar idee-back-stack ())
(defvar idee-forward-stack ())
(defvar ignore-current-buffer nil)

;; Tabs and indentation
(defvar idee-tab-width 4)
(defvar idee-use-tabs nil)

;; View Toggles
(defvar idee-tree-enabled t)
(defvar idee-cli-enabled t)
(defvar idee-repl-enabled t)
(defvar idee-bottom-buffer-command 'projectile-run-eshell)

;; View
(defvar idee-current-view 'idee-ide-view)

;; Functions
(defvar idee-function-alist '((idee-open-function . projectile-switch-project)
                              (idee-new-project-function . idee-new-project-function)
                              (idee-new-file-function . idee-new-file-function)
                              (idee-recent-function . projectile-recentf)
                              (idee-save-all-function . projectile-save-project-buffers)
                              (idee-close-function . projectile-kill-buffers)
                              (idee-build-function . projectile-compile-project)
                              (idee-run-or-eval-function . projectile-run-project)
                              (idee-vcs-function . magit-status)
                              (idee-optimizie-imports-function . nil)
                              (idee-indent-function . evil-indent)
                              (idee-indent-region-function . nil)
                              (idee-select-project-header-function . idee-select-project-header-function)
                              (idee-references-function . nil)
                              (idee-implementation-function . nil)
                              (idee-declaration-function . nil)
                              (idee-back-function . nil)
                              (idee-grep-function . projectile-grep)
                              (idee-find-file-function . projectile-find-file)
                              (idee-find-variable-function . projectile-find-variable)
                              (idee-test-function . nil)
                              (idee-repl-view-function . nil)
                              (idee-mode-tab-width-function . idee-global-set-tab-width-function)
                              (idee-mode-hydra-function . nil)))

;; On Event Command Association List
(defvar idee-on-event-command-alist '())

;;; Comments
(cl-defstruct idee-comment-style
  above
  prefix
  below
  )

;; General purpose comment styles
(defconst elisp-comment-style (make-idee-comment-style :prefix ";; "))
(defconst shell-comment-style (make-idee-comment-style :prefix "# "))
(defconst xml-comment-style (make-idee-comment-style :above "<!--" :below "-->"))

(defvar idee-type-comment-styles-alist `(
                                         ("el" . ,elisp-comment-style)
                                         ("html" . ,shell-comment-style)
                                         ("sh" . ,shell-comment-style)
                                         ("xml" . ,xml-comment-style)
  ))

(defvar idee-current-comment-style elisp-comment-style)

;;; Headers
(defcustom idee-emacs-headers-dir "~/.emacs.d/headers" "The directory where header files are stored.")

;;; Templates
(defcustom idee-emacs-templates-dir "~/.emacs.d/templates" "The directory where template files are stored.")

(defvar idee--current-header nil)

;;; File Types
(defvar idee-type-modes-alist '(("el" . "emacs-lisp-mode")
                                   ))

(provide 'idee-vars)
;;; idee-vars.el ends here
