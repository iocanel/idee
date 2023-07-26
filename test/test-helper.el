;; Copyright (C) 2018 Ioannis Canellos   -*- lexical-binding: t -*-
;;     
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;         http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(require 'f)

(defvar root-test-path
  (f-dirname (f-this-file)))

(defvar root-test-assets-path (f-join root-test-path "assets"))

(defvar root-code-path
  (f-parent root-test-path))

(defvar root-sandbox-path
  (make-temp-file "idee-test-sandbox" t))

;(require 'root (f-expand "root.el" root-code-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory root-sandbox-path))
     (when (f-dir? root-sandbox-path)
       (f-delete root-sandbox-path :force))
     (f-mkdir root-sandbox-path)
     ,@body))


(require 'idee-vars (f-expand "idee-vars.el" root-code-path))
(require 'idee-utils (f-expand "idee-utils.el" root-code-path))
(require 'idee-navigation (f-expand "idee-navigation.el" root-code-path))
(require 'idee-actions (f-expand "idee-actions.el" root-code-path))
(require 'idee-arch (f-expand "idee-arch.el" root-code-path))
(require 'idee-comments (f-expand "idee-comments.el" root-code-path))
(require 'idee-eshell (f-expand "idee-eshell.el" root-code-path))
(require 'idee-git (f-expand "idee-git.el" root-code-path))
(require 'idee-views (f-expand "idee-views.el" root-code-path))
(require 'idee-projects (f-expand "idee-projects.el" root-code-path))
(require 'idee-headers (f-expand "idee-headers.el" root-code-path))
(require 'idee-templates (f-expand "idee-templates.el" root-code-path))
(require 'idee-visitors (f-expand "idee-visitors.el" root-code-path))
(require 'idee-vars (f-expand "idee-vars.el" root-code-path))



;;
;; Optional
;;
(require 'idee-docker (f-expand "idee-docker.el" root-code-path))
;; Java
(require 'idee-java-utils (f-expand "idee-java-utils.el" root-code-path))
(require 'idee-lsp (f-expand "idee-lsp.el" root-code-path))
(require 'idee-lsp-java (f-expand "idee-lsp-java.el" root-code-path))
(require 'idee-java (f-expand "idee-java.el" root-code-path))
(require 'idee-dap (f-expand "idee-dap.el" root-code-path))
(require 'idee-maven (f-expand "idee-maven.el" root-code-path))
 
(require 'idee-javascript (f-expand "idee-javascript.el" root-code-path))



(provide 'test-helper)
;;; test-helper.el ends here
