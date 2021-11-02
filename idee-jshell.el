;;; idee-jshell.el --- Jshell support -*- lexical-binding: t -*-
;; Copyright (C) 2021 Ioannis Canellos 
;;     
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;         http://www.apache.org/licenses/LICENSE-2.0
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1") (polymode "0.2.2"))

;;; Commentary:

;;; Code:

(require 'polymode)
(require 'polymode-core)
(require 'polymode-methods)

(define-hostmode idee-term-hostmode :mode 'term-mode)

(define-innermode idee-java-innermode
  :mode 'java-mode
  :head-matcher "jshell>"
  :tail-matcher ";$"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode idee-term-java-mode
  :hostmode 'idee-term-hostmode
  :innermodes '(idee-java-innermode))

(defun idee-poly-test ()
  "Test stuff"
  (interactive)
  (let* ((buffer (pm-get-buffer-of-mode 'java-mode))
         (file-name (concat (concat (ide-project-root-dir) ".idee") ".jshell")))
    (with-current-buffer buffer
      (message "Enable lsp on %s using %s" buffer file-name)
      (setq-local buffer-file-name file-name) 
      (setq-local lsp-buffer-uri (lsp--path-to-uri buffer-file-name))
      (lsp))))
    
(provide 'idee-jshell)
;;; idee-jshell.el ends here
