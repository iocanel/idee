;;; idee-java.el --- JavaScript IDE

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

(defconst package-json "package.json")
(defconst jsconfig-json "jsconfig.json")
(defconst tsconfig-json "tsconfig.json")

(defun idee-javascript-enable()
  "Enable javascript, add hooks, visitors etc."
  (interactive)
  (message "Enable javascript.")
  (tide-restart-server)
  (add-hook 'javascipt-mode-hook 'idee-javascript-start)
  (add-hook 'js2-mode-hook 'idee-javascript-start)
  (add-hook 'typescirpt-mode-hook 'idee-javascript-start))

(defun idee-javascript-disable()
  "Disable javascript, Remove hooks, visitors etc."
  (interactive)
  (remove-hook 'javascript-mode-hook 'idee-javascript-start t)
  (remove-hook 'js2-mode-hook 'idee-javascript-start t)
  (remove-hook 'typescript-mode-hook 'idee-javascript-start t))

(defun idee-javascript-start()
  "Set javascript bindings."
  (interactive)
  (message "Start javascript.")
  (idee-javascript-enable)
  (flycheck-mode +1)
  
  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-refernces-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-declaration-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-implementation-function idee-function-alist) idee-function-alist))

  ;; Set functions
  (add-to-list 'idee-function-alist '(idee-references-function . tide-references))
  (add-to-list 'idee-function-alist '(idee-declaration-function . tide-jump-to-definition)))
  (add-to-list 'idee-function-alist '(idee-implementation-function . tide-jump-to-implementation))
    

;;; Project Factory
(defun idee-new-npm-project ()
  "Create a new npm project."
  (interactive)
  (let* ((initializer (read-string "Intializer:" "react-app"))
         (recomended-dir (concat (file-name-as-directory default-directory) initializer))
         (temp-dir (concat temporary-file-directory "npm-" (format "%06x-%06x" (random (expt 16 6)) (random (expt 16 6)))))
         (generated-dir (concat (file-name-as-directory temp-dir) initializer))
         (target-dir (idee--select-new-project-dir))
         (parent-dir (file-name-directory (directory-file-name target-dir)))
         (dir-name (substring target-dir (length parent-dir)))
         (generate-command (format "npm init %s %s" initializer target-dir)))

    (make-directory temp-dir t)
    (setq default-directory temp-dir)
    (shell-command generate-command)
    (shell-command (format "mv %s/* %s" temp-dir target-dir))
    (write-region "" nil (concat (file-name-as-directory target-dir) ".projectile"))
    (projectile-add-known-project target-dir)
    (setq projectile-project-root target-dir)
    (projectile-switch-project-by-name target-dir)
    (revert-buffer)
    (dired target-dir)
    (idee-ide-view)))

(defconst idee-npm-project-factory
  (make-idee-project-factory
   :name "Npm"
   :description "A project factory that creates a new project using npm."
   :func 'idee-new-npm-project))

(add-to-list 'idee-project-factories-list idee-npm-project-factory)

;;; Visitor
(defun idee-visitor-javascript (root)
  "Check if a javascript project is available under the specified ROOT."
  (when (seq-filter (lambda (x)
                      (or (equal package-json x)
                          (equal jsconfig-json x)
                          (equal tsconfig-json x))
                      (directory-files root))
                    (idee-javascript-enable))))

(add-to-list 'idee-project-visitors 'idee-visitor-javascript)


(provide 'idee-javascript)
;;; idee-javascript.el ends here.
