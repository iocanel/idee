;; idee-templates-test.el --- IDE templates test

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

(ert-deftest templates/load-from-project ()
  "Should read the header when in root."

 (with-sandbox-java-project
   (make-directory ".idee/templates/java-mode" t)
   (make-directory "src/main/java/" t)
   ;; Create template

   (with-temp-buffer
     (insert "# name: Test template\n")
     (insert "# key: test\n")
     (insert "# --\n")
     (insert "`(idee/java-package-line)`\n")
     (insert "public class `(idee/java-class) {`\n")
     (insert "}`\n")
     (write-region (point-min) (point-max) ".idee/templates/java-mode/test"))
   
   (idee/template-load-from-project)

   ;; Create file
   (with-temp-buffer
     (write-file "src/main/java/Test.java")
     (insert "test")
     (java-mode)
     (yas-minor-mode)
     (yas/expand)
     (write-file "Test.java")
     (should (idee/buffer-contains-string "Test")))))

(provide 'idee-templates-test)
;; idee-templates-test.el ends here
