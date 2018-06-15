;;; idee-lsp-intellij.el --- Lsp-Intellij IDE

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
(require 'idee-headers)
(require 'projectile)

(require 'cc-vars)

(require 'idee-java)

(defconst source-main-prefix "src/main/java")
(defconst source-test-prefix "src/test/java")
(defconst source-prefix "src")
(defconst java-prefix "java")
(defconst test-prefix "test")

(defconst source-directory-list `(,source-main-prefix ,source-test-prefix ,java-prefix ,source-prefix ,test-prefix))

(defcustom idee-lsp-intellij-enabled t "Lsp Intellij Feature Toggle" :group 'idee :type 'boolean)

(defun idee-lsp-intellij-init()
  "Initialize lsp-intellij."
  (if idee-lsp-intellij-enabled
      (idee-lsp-intellij-enable)))

(defun idee-lsp-intellij-enable()
  "Enable lsp-intellij. Add hooks, visitors etc."
  (interactive)
  (use-package lsp-intellij
    :ensure t)
  (setq company-lsp-enable-snippet t
        company-lsp-cache-candidates t)
  (push 'company-lsp company-backends)

  (add-to-list 'company-backends 'company-lsp)
  (add-to-list 'idee-project-visitors 'idee-visitor-lsp-intellij)
  (add-hook 'java-mode-hook 'idee-lsp-intellij-start)
  (add-hook 'java-mode-hook #'lsp-intellij-enable)
  )

(defun idee-lsp-intellij-disable()
  "Disable lsp-intellij. Remove hooks, visitors etc."
  (interactive)
  (setq company-lsp-enable-snippet nil
        company-lsp-cache-candidates nil)
  (setq company-backends (delete 'company-lsp company-backends))
  (setq idee-project-visitors (delete 'idee-visitor-lsp-intellij idee-project-visitors))
  (remove-hook 'java-mode-hook 'idee-lsp-intellij-start t)
  (remove-hook 'java-mode-hook #'lsp-intellij-enable t)
  )

(defun idee-lsp-intellij-start()
  "Enable intellij bindings."
  (interactive)
  (idee-java-enable)
  (lsp-intellij-enable)
  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-refernces-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-implementation-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-declaration-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-optimize-imports-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-indent-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-run-or-eval-function idee-function-alist) idee-function-alist))

  ;; Set functions
  (add-to-list 'idee-function-alist '(idee-references-function . xref-find-references))
  (add-to-list 'idee-function-alist '(idee-implementation-function . lsp-goto-implementation))
  (add-to-list 'idee-function-alist '(idee-declaration-function . xref-find-definitions))
  (add-to-list 'idee-function-alist '(idee-run-or-eval-function . lsp-intellij-run-at-point))


  ;; Add project to lsp java workspace folders
  (setq lsp-java--workspace-folders (delq (assoc (projectile-project-root) lsp-java--workspace-folders) lsp-java--workspace-folders))
  (add-to-list 'lsp-java--workspace-folders (projectile-project-root))
  )


(idee-lsp-intellij-init)

(provide 'idee-lsp-intellij)
;;; idee-lsp-intellij.el ends here
