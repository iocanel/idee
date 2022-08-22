;; idee-python.el --- Python IDE

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
(require 'anaconda-mode)
(require 'python-mode)

(defun python-ide()
  "Enable python bindings."
  (interactive)
  (setq idee/function-alist (delq (assoc 'idee/refernces-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/declaration-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/optimize-imports-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/indent-function idee/function-alist) idee/function-alist))

  (add-to-list 'idee/function-alist '(idee/references-function . anaconda-mode-find-references))
  (add-to-list 'idee/function-alist '(idee/declaration-function . anaconda-mode-find-definitions))
  (add-to-list 'idee/function-alist '(idee/indent-function . python-indent)))

(provide 'idee-python)
;;; idee-python.el ends here
