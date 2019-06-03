;;; idee-javascript.el --- JavaScript IDE

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

(require 'json)
(require 'tide)
(require 'flycheck)

(defconst package-json "package.json")
(defconst jsconfig-json "jsconfig.json")
(defconst tsconfig-json "tsconfig.json")

(defun idee-javascript-enable()
  "Enable javascript, add hooks, visitors etc."
  (interactive)
  (tide-restart-server))

(defun idee-javascript-disable()
  "Disable javascript, Remove hooks, visitors etc."
  (interactive)
  (delete-process (tide-current-server)))

(defun idee-javascript-hook()
  "Javascript hook."
  (flycheck-mode +1)
  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-refernces-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-declaration-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-implementation-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-optimize-imports-function idee-function-alist) idee-function-alist))

  ;; Set functions
  (add-to-list 'idee-function-alist '(idee-references-function . tide-references))
  (add-to-list 'idee-function-alist '(idee-declaration-function . tide-jump-to-definition))
  (add-to-list 'idee-function-alist '(idee-implementation-function . tide-jump-to-implementation))
  (add-to-list 'idee-function-alist '(idee-optimize-imports-function . tide-organize-imports)))

;;; Project Factory
(defun idee-new-npm-project (&optional create-function)
  "Create a new npm project.
The command supports accepting an external CREATE-FUNCTION or defaults to idee-create-project-with-shell."
  (interactive)
  (let* ((initializer (read-string "Intializer:" "react-app"))
         (recomended-dir (concat (file-name-as-directory default-directory) initializer))
         (temp-dir (concat temporary-file-directory "npm-" (format "%06x-%06x" (random (expt 16 6)) (random (expt 16 6)))))
         (generated-dir (concat (file-name-as-directory temp-dir) initializer))
         (target-dir (idee--select-new-project-dir))
         (parent-dir (file-name-directory (directory-file-name target-dir)))
         (dir-name (substring target-dir (length parent-dir)))
         (generate-command (format "npm init %s ." initializer)))

    (funcall (or create-function 'idee-create-project-with-shell) target-dir generate-command)
    (idee-project-set-name dir-name)
    (idee-project-set-version "1.0.0")))


(defconst idee-npm-project-factory
  (make-idee-project-factory
   :name "Npm"
   :description "A project factory that creates a new project using npm."
   :func 'idee-new-npm-project))

(add-to-list 'idee-project-factories-list idee-npm-project-factory)

;;; Visitor
(defun idee-javascript-project-p (root)
  "Check if ROOT is the root path of a javascript project."
  (seq-filter (lambda (x)
                      (or (equal package-json x)
                          (equal jsconfig-json x)
                          (equal tsconfig-json x))) (directory-files root)))

(defun idee-visitor-javascript (root)
  "Check if a javascript project is available under the specified ROOT."
  (when (idee-javascript-project-p root)
    (idee-project-set-version (idee-javascript-pacakge-json-version (concat root package-json)))
    (idee-javascript-enable)))

(add-to-list 'idee-project-visitors 'idee-visitor-javascript)

(defun idee-javascript-package-json-version (p-json)
  "Get the project version from P-JSON."
  (let* ((p (json-read-file p-json)))
    (car (gethash "version" p))))

;; Hooks
(add-hook 'javascipt-mode-hook 'idee-javascript-hook)
(add-hook 'js2-mode-hook 'idee-javascript-hook)
(add-hook 'typescirpt-mode-hook 'idee-javascript-hook)

(provide 'idee-javascript)
;;; idee-javascript.el ends here
