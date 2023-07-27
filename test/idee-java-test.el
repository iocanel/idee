;; idee-java-test.el --- IDE java test

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

;; Package-Requires: ((emacs "28.0"))

;;; Commentary:

;;; Code:
(require 'test-helper)

(idee/java-enable)

(ert-deftest java/comment-java ()
  "Should properly comment java."
    (should (equal (idee/comment "bingo" "java") "/**\n * bingo\n**/\n")))

(ert-deftest java/set-tab-width ()
  "Should set tab width."
  (idee/java-set-tab-width 2)
  (should (equal idee/tab-width 2))


  (idee/java-set-tab-width 4)
  (should (equal idee/tab-width 4)))

(ert-deftest java/create-template-from-file ()
  "Should create template from file."
  (idee/java-init)
  (let ((root-sandbox-path "/tmp/idee/"))
    (with-sandbox-java-project
     (with-temp-buffer
       (write-file "src/main/java/org/acme/Template.java")
       (insert "package org.acme;") (newline)
       (insert "public class Template {") (newline)
       (insert "  public Template () {") (newline)
       (insert "  }") (newline)
       (insert "  public static void main (String[] args) {") (newline)
       (insert "    System.out.println(\"Hello from Template\");") (newline)
       (insert "  }") (newline)
       (insert "}") (newline)
       (java-mode)
       (write-file "Template.java")
     (idee/template-create-from-buffer (current-buffer) "Test template" "template"))

   (should (file-exists-p ".idee/templates/java-mode/template")))))

(ert-deftest java/package-of-fqcn ()
  "Should properly comment java."
    (should (equal (idee/java-package-of-fqcn "org.acme.HelloWorld") "org.acme")))

(defmacro with-sandbox-java-project (&rest body)
  `(with-sandbox
    (let* ((idee/dir (f-join root-sandbox-path ".idee"))
           (templates-dir (f-join idee/dir "templates"))
           (java-templates-dir (f-join templates-dir "java-mode"))
           (relative-dir "src/main/java/org/acme")
           (simple-class "SimpleClass.java")
           (absolute-simple-class (f-join root-sandbox-path relative-dir simple-class))
           (pom-xml "pom.xml")
           (absolute-pom-xml (f-join root-sandbox-path pom-xml)))
   (make-directory (f-join ".git"))
   (make-directory java-templates-dir t)
   (make-directory (f-join root-sandbox-path relative-dir) t)
   (copy-file (f-join root-test-assets-path simple-class) absolute-simple-class)
   (copy-file (f-join root-test-assets-path pom-xml) absolute-pom-xml)
   ,@body)))
 
(ert-deftest java/class-name-of ()
  "Should extract the class name from the file name."
  (with-sandbox-java-project
   (should (equal "SimpleClass" (idee/java-class-name-of absolute-simple-class)))))

(ert-deftest java/package-of ()
  "Should extract the class name from the file name."
  (with-sandbox-java-project
   (should (equal "org.acme" (idee/java-package-of absolute-simple-class)))))

(ert-deftest java/create-template ()
  "Should extract the class name from the file name."
  (with-sandbox-java-project
     (idee/java-create-template absolute-simple-class "Hello World" "hello")
     (let ((template (f-join java-templates-dir "hello")))
       (should (file-exists-p template))
       (should (string-match (regexp-quote "`idee/java-package-line`") (idee/read-file template))))))
             
(provide 'idee-java-test)
;;; idee-java-test.el ends here
