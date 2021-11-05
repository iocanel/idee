;; idee-headers-test.el --- IDE headers test

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
(require 'test-helper)

(ert-deftest headers/read-from-root ()
  "Should read the header when in root."
  (with-sandbox
   (f-mkdir ".git")
   (append-to-file "bingo" nil "header.txt")
   (let ((header (idee/header-of-project)))
         (should header)
         (should (equal header "bingo")))))

(ert-deftest headers/read-and-eval ()
  "Should read the header and evaluate lisp code."
  (with-sandbox
   (f-mkdir ".git")
   (append-to-file "bingo `var`" nil "header.txt")
   (let* ((var "123")
          (header (idee/header-of-project)))
         (should header)
         (should (equal header "bingo 123")))))

(provide 'idee-headers-test)
;; idee-headers-test.el ends here
