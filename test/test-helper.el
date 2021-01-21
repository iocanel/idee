;; Copyright (C) 2018 Ioannis Canellos 
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

(defvar root-test-path (file-name-as-directory (file-truename (cond (load-in-progress load-file-name) ((and (boundp 'byte-compile-current-file) byte-compile-current-file) byte-compile-current-file) (:else (buffer-file-name))))
(defvar root-test-assets-path (concat root-test-path "assets"))

(defvar root-code-path (file-name-directory (directory-file-name root-test-path)))

(defvar root-sandbox-path
  (make-temp-file "idee-test-sandbox" t))

;(require 'root (f-expand "root.el" root-code-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory root-sandbox-path))
     (when (file-directory-p root-sandbox-path) (delete-directory root-sandbox-path t))
     (make-directory root-sandbox-path t)
     ,@body))

  

(provide 'test-helper)
;;; test-helper.el ends here
