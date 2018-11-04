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
(require 'markdown-mode)

(defcustom idee-lsp-java-enabled t "Lsp Java Feature Toggle" :group 'idee :type 'boolean)
(defcustom idee-lsp-java-completion-enabled t "Lsp Java Completion Feature Toggle" :group 'idee :type 'boolean)

(defun idee-lsp-java-init()
  "Initialize lsp-java."
  (message "Intializing lsp-java")
  (if idee-lsp-java-enabled
      (idee-lsp-java-enable)))

(defun idee-lsp-java-enable()
  "Enable lsp-java. Add hooks, visitors etc."
  (interactive)
  (use-package lsp-java
    :ensure t
    :requires (lsp-ui-flycheck lsp-ui-sideline)
    :config
    (add-hook 'java-mode-hook  'lsp-java-enable)
    (add-hook 'java-mode-hook  'flycheck-mode)
    (add-hook 'java-mode-hook  'company-mode)
    (add-hook 'java-mode-hook  (lambda () (lsp-ui-flycheck-enable t)))
    (add-hook 'java-mode-hook  'lsp-ui-sideline-mode)
    )

  (add-to-list 'idee-project-visitors 'idee-visitor-lsp-java)
  (add-hook 'java-mode-hook 'idee-lsp-java-start)
  (if idee-lsp-java-completion-enabled
      (add-to-list 'company-backends 'company-lsp)
    (setq company-backends (delete 'company-lsp company-backends))
    )
  )

(defun idee-lsp-java-disable()
  "Disable lsp-java, remove hooks, visitors etc."
  (interactive)
  (setq idee-project-visitors (delete 'idee-visitor-lsp-java idee-project-visitors))
  (setq company-backends (delete 'company-lsp company-backends))
  (remove-hook 'java-mode-hook 'idee-lsp-java-start t)
  )

(defun idee-lsp-java-start()
  "Start LSP for Java."
  
  ;; Add project to lsp java workspace folders
  (setq lsp-java--workspace-folders (delq (assoc (projectile-project-root) lsp-java--workspace-folders) lsp-java--workspace-folders))
  (add-to-list 'lsp-java--workspace-folders (projectile-project-root))
  )


;;; Visitor
(defun idee-lsp-java-is-applicable()
  "Check if lsp-java mode is applicable to the project."
  (interactive)
  (seq-filter (lambda (x)
                (or
                 (equal "pom.xml" x)
                 (equal "build.gradle" x)
                 (equal ".project" x)
                 ))
              (directory-files (projectile-project-root))))

(defun idee-visitor-lsp-java (root)
  "Check if a lsp-java project is available under the specified ROOT."
  (if (idee-lsp-java-is-applicable)
      (idee-lsp-java-start))
  )

(provide 'idee-lsp-java)
;;; idee-lsp-java.el ends here
