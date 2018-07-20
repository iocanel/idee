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

(defun idee-lsp-java-start()
  "Start LSP for Java."
  
  ;; Add project to lsp java workspace folders
  (setq lsp-java--workspace-folders (delq (assoc (projectile-project-root) lsp-java--workspace-folders) lsp-java--workspace-folders))
  (add-to-list 'lsp-java--workspace-folders (projectile-project-root))
  )


(provide 'idee-lsp-java)
;;; idee-lsp-java.el ends here.
