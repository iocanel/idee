;;; eide-golang.el --- GoLang IDE

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
(require 'projectile)
(require 'go-mode)

(defun golang-ide()
  "Enabled golang bindings"
  (interactive)
  (go-set-project)
  (setq eide-function-alist (delq (assoc 'eide-refernces-function eide-function-alist) eide-function-alist))
  (setq eide-function-alist (delq (assoc 'eide-declaration-function eide-function-alist) eide-function-alist))
  (setq eide-function-alist (delq (assoc 'eide-optimize-imports-function eide-function-alist) eide-function-alist))
  (setq eide-function-alist (delq (assoc 'eide-indent-function eide-function-alist) eide-function-alist))
  (setq eide-function-alist (delq (assoc 'eide-mode-hydra-function eide-function-alist) eide-function-alist))

  (add-to-list 'eide-function-alist '(eide-references-function . go-guru-callers))
  (add-to-list 'eide-function-alist '(eide-declaration-function . godef-jump-other-window))
  (add-to-list 'eide-function-alist '(eide-optimize-imports-function . goimports))
  (add-to-list 'eide-function-alist '(eide-indent-function . gofmt))
  (add-to-list 'eide-function-alist '(eide-mode-hydra-function . go-hydra/body))
  )

(provide 'eide-golang)
;;; eide-golang.el ends here
