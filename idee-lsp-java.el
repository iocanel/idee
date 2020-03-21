;;; idee-lsp-java.el --- LSP Java

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

(defcustom idee-lsp-java-enabled t "Lsp Java Feature Toggle" :group 'idee-java :type 'boolean)
(defcustom idee-lsp-java-completion-enabled t "Lsp Java Completion Feature Toggle" :group 'idee-java :type 'boolean)

(defun idee-lsp-java-enable()
  "Enable lsp-java, add hooks, visitors etc."
  (interactive)
  (if idee-lsp-java-completion-enabled
      (progn (add-to-list 'company-backends 'company-lsp)
             (lsp-workspace-folders-add (projectile-project-root)))
    (setq company-backends (delete 'company-lsp company-backends))))

(defun idee-lsp-java-disable()
  "Disable lsp-java, remove hooks, visitors etc."
  (interactive)
  (setq company-backends (delete 'company-lsp company-backends)))

(defun idee-lsp-java-hook()
  "Hook for lsp-java."
  
  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-refernces-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-declaration-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-optimize-imports-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-indent-function idee-function-alist) idee-function-alist))
  ;(setq idee-function-alist (delq (assoc 'idee-mode-hydra-function idee-function-alist) idee-function-alist))
  ;(setq idee-function-alist (delq (assoc 'idee-run-or-eval-function idee-function-alist) idee-function-alist))
  ;(setq idee-function-alist (delq (assoc 'idee-test idee-function-alist) idee-function-alist))

  ;; Set functions
  ;(add-to-list 'idee-function-alist '(idee-run-or-eval-function . lsp-java-de))
  ;(add-to-list 'idee-function-alist '(idee-test-function . idee-meghanada-test-dwim))
  ;(add-to-list 'idee-function-alist '(idee-mode-hydra-function . meghanada-hydra/body))
  (add-to-list 'idee-function-alist '(idee-references-function . lsp-ui-peek-find-references))
  (add-to-list 'idee-function-alist '(idee-declaration-function . lsp-ui-peek-find-definitions))
  (add-to-list 'idee-function-alist '(idee-optimize-imports-function . lsp-java-organize-imports)))

(defun idee--lsp-java--on-save-buffer()
  "Save buffer handler."
  (if (and (buffer-file-name) (equal "pom.xml" (file-name-nondirectory (buffer-file-name))))
             (lsp-java-update-project-configuration)))


;;; Visitor
(defun idee-lsp-java-project-p (root)
  "Check if lsp-java mode is applicable to the project."
  (seq-filter (lambda (x)
                (or
                 (equal "pom.xml" x)
                 (equal "build.gradle" x)
                 (equal ".project" x))) (directory-files root)))

;;;###autoload
(defun idee-lsp-java-switch-workspace (w)
  "Switch to workspace W."
    (setq lsp-java-workspace-dir workspace)
    (setq lsp-java-workspace-cache-dir (f-join lsp-java-workspace-dir ".cache")))

;;;###autoload
(defun idee-visitor-lsp-java (root)
  "Check if a lsp-java project is available under the specified ROOT."
  (if (and idee-lsp-java-enabled (idee-lsp-java-project-p root))
      (idee-lsp-java-enable)))

;;;###autoload
(defun idee--lsp-java-init ()
  (idee-register-visitor 'idee-visitor-lsp-java)
  (add-hook 'java-mode-hook 'idee-lsp-java-hook)
  (advice-add 'save-buffer :after #'idee--lsp-java--on-save-buffer)
  (add-hook 'idee-lsp-before-workspace-restart-hook 'idee-lsp-java-switch-workspace))

(provide 'idee-lsp-java)
;;; idee-lsp-java.el ends here
