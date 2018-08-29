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

;; Tabs and indentation
(defvar idee-tab-width 4)
(defvar idee-use-tabs nil)

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
                              (idee-find-file-function . projectile-find-file-dwim)
                              (idee-find-variable-function . find-variable)
                              (idee-test-function . nil)
                              (idee-repl-view-function . nil)
                              (idee-mode-tab-width-function . idee-global-set-tab-width-function)
                              (idee-mode-hydra-function . nil)))

;; On Event Command Association List
(defvar idee-on-event-command-alist '())

;;; File Types

(provide 'idee-vars)
;;; idee-vars.el ends here
