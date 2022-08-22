;; idee-java.el --- Rust support for IDE -*- lexical-binding: t -*-

;; Copyright (C) 2022 Ioannis Canellos

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

(require 'flycheck)
(require 'idee-projects)
(require 'idee-visitors)

(defconst cargo-toml "Cargo.toml")
(defvar idee/rust-initialized nil)

(defun idee/rust-enable()
  "Enable rust, add hooks, visitors etc."
  (interactive))

(defun idee/rust-disable()
  "Disable rust, Remove hooks, visitors etc."
  (interactive))

(defun idee/rust-hook()
  "Rust hook."
  (flycheck-mode +1)
  ;; Clear functions
  (setq idee/function-alist (delq (assoc 'idee/indent-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/indent-region-function idee/function-alist) idee/function-alist))
  (setq idee/function-alist (delq (assoc 'idee/build-function idee/function-alist) idee/function-alist))

  ;; Set functions
  (add-to-list 'idee/function-alist '(idee/references-function . lsp-find-references))
  (add-to-list 'idee/function-alist '(idee/declaration-function . lsp-find-definition))
  (add-to-list 'idee/function-alist '(idee/implementation-function . lsp-find-implementation)) (add-to-list 'idee/function-alist '(idee/indent-function . rustic-format-file))
  (add-to-list 'idee/function-alist '(idee/indent-region-function . rustic-format-region))
  (add-to-list 'idee/function-alist '(idee/build-function . rustic-compile)))

;;; Project Factory
(defun idee/new-cargo-project (&optional create-function)
  "Create a new npm project.
The command supports accepting an external CREATE-FUNCTION or defaults to idee/project-create-with-shell."
  (interactive)
  (let* ((recomended-dir (concat (file-name-as-directory default-directory)))
         (temp-dir (concat temporary-file-directory "rust-" (format "%06x" (random (expt 16 6)))))
         (generated-dir (concat (file-name-as-directory temp-dir)))
         (target-dir (idee/project-dir-select))
         (parent-dir (file-name-directory (directory-file-name target-dir)))
         (dir-name (substring target-dir (length parent-dir)))
         (generate-command (format "cargo new %s" dir-name))
         (cleanup-command (format "mv %s/* . && rm -r %s" dir-name dir-name)))
    (funcall (or create-function 'idee/project-create-with-shell) target-dir generate-command cleanup-command)
    (idee/project-name-set dir-name)
    (idee/project-version-set "1.0.0")))


(defconst idee/cargo-project-factory
  (make-idee/project-factory
   :name "Cargo"
   :description "A project factory that creates a new project using cargo."
   :func 'idee/new-cargo-project))


;;; Visitor
(defun idee/rust-project-p (root)
  "Check if ROOT is the root path of a rust project."
  (seq-filter (lambda (x) (equal cargo-toml x)) (directory-files root)))

(defun idee/rust-visitor (root)
  "Check if a rust project is available under the specified ROOT."
  (when (idee/rust-project-p root)
    (idee/project-version-set (idee/rust-cargo-toml-version (concat root cargo-toml)))
    (idee/rust-enable)))


(defun idee/rust-cargo-toml-version (c)
  "Get the project version from C."
  (with-temp-buffer
    (insert-file c)
    (goto-char (point-min))
    (if (re-search-forward "version = \"\\(.*\\)\"" nil t)
        (match-string 1)
      nil)))

;;; Init
(defun idee/rust-init ()
  "Initialize IDE rust."
  (interactive)
  (idee/only-once idee/rust-initialized
    (idee/project-factory-register idee/cargo-project-factory)
    (idee/visitor-register 'idee/rust-visitor)
    ;; Hooks
    (add-hook 'rust-mode-hook 'idee/rust-hook)
    (add-hook 'rustic-mode-hook 'idee/rust-hook)))

(provide 'idee-rust)
;;; idee-rust.el ends here
