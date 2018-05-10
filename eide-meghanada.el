;;; eide-meghanada.el --- Meghanada IDE

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
(require 'meghanada)

(defun meghanada-ide()
  "Enables java bindings"
  (interactive)
  (setq eide-function-alist (delq (assoc 'eide-refernces-function eide-function-alist) eide-function-alist))
  (setq eide-function-alist (delq (assoc 'eide-declaration-function eide-function-alist) eide-function-alist))
  (setq eide-function-alist (delq (assoc 'eide-optimize-imports-function eide-function-alist) eide-function-alist))
  (setq eide-function-alist (delq (assoc 'eide-indent-function eide-function-alist) eide-function-alist))
  (setq eide-function-alist (delq (assoc 'eide-mode-hydra-function eide-function-alist) eide-function-alist))

  (add-to-list 'eide-function-alist '(eide-references-function . meghanada-reference))
  (add-to-list 'eide-function-alist '(eide-declaration-function . meghanada-jump-declaration))
  (add-to-list 'eide-function-alist '(eide-optimize-imports-function . meghanada-optimize-import))
  (add-to-list 'eide-function-alist '(eide-mode-hydra-function . meghanada-hydra/body))
  )
(add-hook 'meghanada-mode-hook 'meghanada-ide)



(provide 'eide-meghanada)
;;; eide-meghanada.el ends here
