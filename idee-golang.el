;; idee-golang.el --- GoLang IDE

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

;; Package-Requires: ((emacs "28.0"))

;;; Commentary:

;;; Code:
(require 'idee-vars)
(require 'idee-visitors)

(defconst go-mod "go.mod")
(defconst glidee/yml "glide.yml")
(defconst gopkg-toml "Gopkg.toml")

(defvar idee/golang-initialized nil)

(defun idee/golang-enable()
  "Enabled golang bindings."
  (interactive)
  (idee/golang-hook))

(defun idee/golang-hook ()
  "Golang hook."
  (setq idee/function-alist (delq (assoc 'idee/refernces-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/declaration-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/optimize-imports-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/indent-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/mode-hydra-function idee/function-alist) idee/function-alist))

  (add-to-list 'idee/function-alist '(idee/references-function . go-guru-callers))
  (add-to-list 'idee/function-alist '(idee/declaration-function . go-guru-definition))
  (add-to-list 'idee/function-alist '(idee/optimize-imports-function . goimports))
  (add-to-list 'idee/function-alist '(idee/indent-function . gofmt))
  (add-to-list 'idee/function-alist '(idee/mode-hydra-function . go-hydra/body)))

;;; Project Factory
(defun idee/new-golang-module (&optional create-function)
  "Create a new golang module.
The command supports accepting an external CREATE-FUNCTION or defaults to idee/project-create-with-shell."
  (interactive)
  (let* ((module-name (read-string "Module:" "example.com/m"))
         (target-dir (idee/project-dir-select))
         (generate-command (format "go mod init %s" module-name)))
    (funcall (or create-function 'idee/project-create-with-shell) target-dir generate-command)))

(defconst idee/golang-module-factory
  (make-idee/project-factory
   :name "Go\ module"
   :description "A project factory that creates a new golang module."
   :func 'idee/new-golang-module))


;;; Visitor
(defun idee/golang-visitor (root)
  "Check if a golang project is available under the specified ROOT."
  (when (seq-filter (lambda (x)
                      (or (equal go-mod x)
                          (equal glidee/yml x)
                          (equal gopkg-toml x)))
                      (directory-files root))
                    (idee/golang-enable)))

;;; Init
(defun idee/golang-init ()
  "Initialize IDE golang."
  (interactive)
    (idee/only-once idee/golang-initialized
      (idee/visitor-register 'idee/golang-visitor)
      (idee/project-factory-register idee/golang-module-factory)
      ;; Hooks
      (add-hook 'go-mode-hook 'idee/golang-hook)))

(provide 'idee-golang)
;;; idee-golang.el ends here
