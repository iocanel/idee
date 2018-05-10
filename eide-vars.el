;;; eide-vars.el ---  Custom and Variables

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
(cl-defstruct eide-buffer-point
  buffer
  line
  column
  )

;; Back and Forth Navigation
(defvar eide-back-stack ())
(defvar eide-forward-stack ())
(defvar ignore-current-buffer nil)

;; Tabs and indentation
(defvar eide-tab-width 4)
(defvar eide-use-tabs nil)

;; View Toggles
(defvar eide-tree-enabled nil)
(defvar eide-cli-enabled t)
(defvar eide-repl-enabled t)
(defvar eide-bottom-buffer-command 'projectile-run-eshell)

;; View
(defvar eide-current-view 'eide-ide-view)

;; Functions
(defvar eide-function-alist '((eide-open-function . projectile-switch-project)
                              (eide-recent-function . projectile-recentf)
                              (eide-save-all-function . projectile-save-project-buffers)
                              (eide-close-function . projectile-kill-buffers)
                              (eide-build-function . projectile-compile-project)
                              (eide-run-or-eval-function . projectile-run-project)
                              (eide-vcs-function . magit-status)
                              (eide-optimizie-imports-function . nil)
                              (eide-indent-function . evil-indent)
                              (eide-indent-region-function . nil)
                              (eide-license-headers-function . nil)
                              (eide-references-function . nil)
                              (eide-declaration-function . nil)
                              (eide-back-function . nil)
                              (eide-grep-function . projectile-grep)
                              (eide-find-file-function . projectile-find-file)
                              (eide-find-variable-function . projectile-find-variable)
                              (eide-test-function . nil)
                              (eide-repl-view-function . nil)
                              (eide-mode-hydra-function . nil))) 

;; On Event Command Association List
(defvar eide-on-event-command-alist '())

(provide 'eide-vars)
;;; eide-vars.el ends here
