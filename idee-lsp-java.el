;; idee-lsp-java.el --- LSP and Java support for IDE -*- lexical-binding: t -*-

;; Copyright (C) 2018 Ioannis Canellos
;;     
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;
;;
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(require 'lsp-java)
(require 'dap-java)
(require 'markdown-mode)

(require 'idee-visitors)
(require 'idee-lsp)

(defcustom idee/lsp-java-enabled t "Lsp Java Feature Toggle" :group 'idee/java :type 'boolean)
(defcustom idee/lsp-java-completion-enabled t "Lsp Java Completion Feature Toggle" :group 'idee/java :type 'boolean)

(defun idee/lsp-java-enable()
  "Enable lsp-java, add hooks, visitors etc."
  (interactive)
  (when idee/lsp-java-enabled
      (lsp-workspace-folders-add (projectile-project-root))))

(defun idee/lsp-java-disable()
  "Disable lsp-java, remove hooks, visitors etc."
  (interactive))

(defun idee/lsp-java-hook()
  "Hook for lsp-java."
  
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
  ;(add-to-list 'idee/function-alist '(idee/run-or-eval-function . lsp-java-de))
  (add-to-list 'idee/function-alist '(idee/references-function . lsp-find-references))
  (add-to-list 'idee/function-alist '(idee/declaration-function . lsp-find-definition))
  (add-to-list 'idee/function-alist '(idee/implementation-function . lsp-find-implementation))
  (add-to-list 'idee/function-alist '(idee/optimize-imports-function . lsp-java-organize-imports)))

(defun idee/lsp-java-on-save-buffer(&rest args)
  "Save buffer handler."
  (when (and (buffer-file-name) (equal "pom.xml" (file-name-nondirectory (buffer-file-name))))
    (ignore-errors
      (lsp-java-update-project-configuration))))


;;; Visitor
(defun idee/lsp-java-project-p (root)
  "Check if lsp-java mode is applicable to the project."
  (seq-filter (lambda (x)
                (or
                 (equal "pom.xml" x)
                 (equal "build.gradle" x)
                 (equal ".project" x))) (directory-files root)))

;;;###autoload
(defun idee/lsp-java-switch-workspace (workspace-dir)
  "Switch to workspace W."
  (when workspace-dir
    (setq lsp-java-workspace-dir workspace-dir)
    (setq lsp-java-workspace-cache-dir (f-join lsp-java-workspace-dir ".cache"))))

;;;###autoload
(defun idee/lsp-visitor-java (root)
  "Check if a lsp-java project is available under the specified ROOT."
  (if (and idee/lsp-java-enabled (idee/lsp-java-project-p root))
      (idee/lsp-java-enable)))

;;;###autoload
(defun idee/lsp-java-init ()
  (idee/visitor-register 'idee/lsp-visitor-java)
  (add-hook 'java-mode-hook 'idee/lsp-java-hook)
  (advice-add 'save-buffer :after #'idee/lsp-java-on-save-buffer)
  (add-hook 'idee/lsp-before-workspace-restart-hook 'idee/lsp-java-switch-workspace))

(provide 'idee-lsp-java)
;;; idee-lsp-java.el ends here
