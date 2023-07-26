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
(require 'python-mode)

(defconst pyproject-toml "pyproject.toml")

(defun idee/python-enable()
  "Enable python bindings."
  (interactive)
  ;; (setq idee/function-alist (delq (assoc 'idee/refernces-function idee/function-alist) idee/function-alist))
  ;; (setq idee/function-alist (delq (assoc 'idee/declaration-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/optimize-imports-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/indent-function idee/function-alist) idee/function-alist))

  ;; (add-to-list 'idee/function-alist '(idee/references-function . anaconda-mode-find-references))
  ;; (add-to-list 'idee/function-alist '(idee/declaration-function . anaconda-mode-find-definitions))
  (add-to-list 'idee/function-alist '(idee/indent-function . python-indent)))

;;; Visitor
(defun idee/pyton-project-p (root)
  "Check if ROOT is the root path of a python project."
  (seq-filter (lambda (x) (equal pyproject-toml x)) (directory-files root)))

(defun idee/python-visitor (root)
  "Check if a python project is available under the specified ROOT."
  (when (idee/python-project-p root)
    (idee/project-version-set (idee/python-pyproject-toml-version (concat root cargo-toml)))
    (idee/python-enable)))


(defun idee/python-pyproject-toml-version (c)
  "Get the project version from C."
  (with-temp-buffer
    (insert-file c)
    (goto-char (point-min))
    (if (re-search-forward "version = \"\\(.*\\)\"" nil t)
        (match-string 1)
      nil)))

;;; Init
(defun idee/pyton-init ()
  "Initialize IDE python."
  (interactive)
  (idee/only-once idee/python-initialized
    (idee/project-factory-register idee/pyproject-project-factory)
    (idee/visitor-register 'idee/python-visitor)
    ;; Hooks
    (add-hook 'python-mode-hook 'idee/python-enable)
    (add-hook 'pythonic-mode-hook 'idee/python-enable)))


(provide 'idee-python)
;;; idee-python.el ends here
