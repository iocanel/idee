;;; idee-meghanada.el --- Meghanada IDE

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
(require 'projectile)
(require 'meghanada)

(defun meghanada-ide()
  "Enables java bindings"
  (interactive)
  (setq idee-function-alist (delq (assoc 'idee-refernces-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-declaration-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-optimize-imports-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-indent-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-mode-tab-width-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-mode-hydra-function idee-function-alist) idee-function-alist))

  (add-to-list 'idee-function-alist '(idee-references-function . meghanada-reference))
  (add-to-list 'idee-function-alist '(idee-declaration-function . meghanada-jump-declaration))
  (add-to-list 'idee-function-alist '(idee-optimize-imports-function . meghanada-optimize-import))
  (add-to-list 'idee-function-alist '(idee-mode-tab-width-function . meghanada-tab-width))
  (add-to-list 'idee-function-alist '(idee-mode-hydra-function . meghanada-hydra/body))
  )


(defun meghanada-tab-width ()
  "Replace the hook that sets the tab width."
  (remove-hook 'java-mode-hook 'meghanada-update-tab-width)
  (add-hook 'java-mode-hook 'meghanada-update-tab-width)
  (java-mode)
  (java-mode)
  )

(defun meghanada-update-tab-width()
  "Update the tab width for java hook"
   (setq c-basic-offset idee-tab-width)
  )

(add-hook 'meghanada-mode-hook 'meghanada-ide)



(provide 'idee-meghanada)
;;; idee-meghanada.el ends here
