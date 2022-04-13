;; idee-javascript.el --- JavaScript IDE

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
(require 'flycheck)
(require 'idee-arch)
(require 'idee-projects)
(require 'idee-visitors)

(defconst package-json "package.json")
(defconst jsconfig-json "jsconfig.json")
(defconst tsconfig-json "tsconfig.json")

(defun idee/javascript-enable()
  "Enable javascript, add hooks, visitors etc."
  (interactive))

(defun idee/javascript-disable()
  "Disable javascript, Remove hooks, visitors etc."
  (interactive))

(defun idee/javascript-hook()
  "Javascript hook."
  (flycheck-mode +1)
  ;; Clear functions
  (setq idee/function-alist (delq (assoc 'idee/refernces-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/declaration-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/implementation-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/optimize-imports-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/indent-function idee/function-alist) idee/function-alist))
  ;(setq idee/function-alist (delq (assoc 'idee/mode-hydra-function idee/function-alist) idee/function-alist))
  ;(setq idee/function-alist (delq (assoc 'idee/run-or-eval-function idee/function-alist) idee/function-alist))
  ;(setq idee/function-alist (delq (assoc 'idee/test idee/function-alist) idee/function-alist))

  ;; Set functions
  (add-to-list 'idee/function-alist '(idee/references-function . lsp-find-references))
  (add-to-list 'idee/function-alist '(idee/declaration-function . lsp-find-definition))
  (add-to-list 'idee/function-alist '(idee/implementation-function . lsp-find-implementation))
  (add-to-list 'idee/function-alist '(idee/optimize-imports-function . lsp-java-organize-imports)))

;;; Project Factory
(defun idee/new-npm-project (&optional create-function target-dir)
  "Create a new npm project.
The command supports accepting an external CREATE-FUNCTION or defaults to idee/project-create-with-shell."
  (interactive)
  (let* ((initializer (read-string "Intializer:" "react-app"))
         (recomended-dir (concat (file-name-as-directory default-directory) initializer))
         (temp-dir (concat temporary-file-directory "npm-" (format "%06x-%06x" (random (expt 16 6)) (random (expt 16 6)))))
         (generated-dir (concat (file-name-as-directory temp-dir) initializer))
         (target-dir (or target-dir (idee/project-dir-select)))
         (parent-dir (file-name-directory (directory-file-name target-dir)))
         (dir-name (substring target-dir (length parent-dir)))
         (generate-command (format "npm init %s ." initializer)))

    (funcall (or create-function 'idee/project-create-with-shell) target-dir generate-command)
    (idee/project-name-set dir-name)
    (idee/project-version-set "1.0.0")))

(defun idee/new-react-project (&optional create-function target-dir)
  "Create a new react project.
The command supports accepting an external CREATE-FUNCTION or defaults to idee/project-create-with-shell."
  (interactive)
  (let* ((recomended-dir (file-name-as-directory default-directory))
         (temp-dir (concat temporary-file-directory "npm-" (format "%06x-%06x" (random (expt 16 6)) (random (expt 16 6)))))
         (generated-dir (file-name-as-directory temp-dir))
         (target-dir (or target-dir (idee/project-dir-select)))
         (parent-dir (file-name-directory (directory-file-name target-dir)))
         (dir-name (substring target-dir (length parent-dir)))
         (generate-command (format "npx create-react-app %s" dir-name))
         (cleanup-command (format "mv %s/* . && rm -r %s" dir-name dir-name)))

    (funcall (or create-function 'idee/project-create-with-shell) target-dir generate-command cleanup-command)
    (idee/project-name-set dir-name)
    (idee/project-version-set "1.0.0")))

(defconst idee/npm-project-factory
  (make-idee/project-factory
   :name "Npm"
   :description "A project factory that creates a new project using npm."
   :func 'idee/new-npm-project))

;;; Visitor
(defun idee/javascript-project-p (root)
  "Check if ROOT is the root path of a javascript project."
  (seq-filter (lambda (x)
                      (or (equal package-json x)
                          (equal jsconfig-json x)
                          (equal tsconfig-json x))) (directory-files root)))

(defun idee/javascript-visitor (root)
  "Check if a javascript project is available under the specified ROOT."
  (when (idee/javascript-project-p root)
    (idee/project-version-set (idee/javascript-package-json-version (concat root package-json)))
    (idee/javascript-enable)))


(defun idee/javascript-package-json-version (p-json)
  "Get the project version from P-JSON."
  (if (file-exists-p p-json)
      (let* ((json (json-read-file p-json))
             (p (if json (car (gethash "version" p)) nil))))
    nil))

;;
;; Snippets
;;

(defun idee/javascript-create-template (source-file name key &optional templates-dir)
  "Create snippet from javascript file."
    (with-temp-buffer
      (let* ((root-dir (idee/project-root-dir (buffer-file-name)))
             (conf-dir (concat (file-name-as-directory root-dir) idee/project-conf-dir))
             (project-templates-dir (concat (file-name-as-directory conf-dir) "templates"))
             (templates-dir (or templates-dir project-templates-dir))
             (body (idee/javascript-snippet-body-from source-file))
             (target (concat (file-name-as-directory templates-dir) (format "js-mode/%s" key)))
             (parent (file-name-directory target)))
        (set-visited-file-name target)
        (insert (format "# name: %s" name))
        (newline)
        (insert (format "# key: %s" key))
        (newline)
        (insert "# --")
        (newline)
        (insert body)
        (write-file target)
        (yas-load-directory parent)
        (yas-compile-directory parent))))

(defun idee/javascript-snippet-body-from (f)
  "Create snippet from file F."
  (with-temp-buffer
    (save-excursion
        (insert-file-contents f)
        (goto-char (point-min))
        (idee/comment-remove-at-point java-comment-style)
        (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
               (name (idee/filename f))
               (updated (concat "`idee/header`\n" (replace-regexp-in-string (regexp-quote name) "`(idee/filename)`" content t))))
          updated))))

;;
;; Archetypes
;;
(defun idee/javascript-react-component (&optional base-path src-path)
  "Create a react component javascript and css."
  (interactive)
  (let* ((name (read-string "Component name:"))
         (base-path (file-name-as-directory (or base-path (idee/module-root-dir) default-directory)))
         (src-path (file-name-as-directory (or src-path "src/")))
         (js (concat base-path (format "%s/%s.js" src-path name)))
         (css (concat base-path (format "%s/%s.css" src-path name))))

    ;; Javascript
    (with-temp-buffer
      (message "With temp buffer 1")
        (set-visited-file-name js)
        (insert "rcomp")
        (js-mode)
        (yas-expand)
        (write-file js))

    ;; CSS
    (with-temp-buffer
        (set-visited-file-name css)
        (insert "")
        (yas-expand)
        (write-file css))

    (find-file js)))

 (idee/archetype-register
  (make-idee/archetype
   :name "React component"
   :description "A react component js and css"
   :func 'idee/javascript-react-component))


;;; Init
(defun idee/javascript-init ()
  "Initialize IDE javascript."
  (interactive)
  (idee/project-factory-register idee/npm-project-factory)
  (idee/visitor-register 'idee/javascript-visitor)

  (idee/template-factory-register (make-idee/template-factory
                                             :mode 'js-mode
                                             :description "A javascript temlate factory"
                                             :func 'idee/javascript-create-template))

;; Hooks
  (add-hook 'javascipt-mode-hook 'idee/javascript-hook)
  (add-hook 'js2-mode-hook 'idee/javascript-hook)
  (add-hook 'js-jsx-mode-hook 'idee/javascript-hook)
  (add-hook 'typescirpt-mode-hook 'idee/javascript-hook))

(provide 'idee-javascript)
;;; idee-javascript.el ends here
