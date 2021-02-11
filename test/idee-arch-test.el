;;; idee-arch-test.el --- IDEE arch test

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

;;; Commentary:

;;; Code:

(require 'test-helper)


(ert-deftest arch/test-expand-path()
  "Should successfully extract exposes ports from Dockerfile"
  (should (equal "src/main/java/org/acme/HelloWorld.java" (let ((package "org/acme")
                                                                (class "HelloWorld"))
                                                            (idee-expand-path "src/main/java/__package__/__class__.java")))))

(ert-deftest arch/test-expand-path-with-dashes()
  "Should successfully extract exposes ports from Dockerfile"
  (should (equal "src/main/java/org/acme/HelloWorld.java" (let ((package-path "org/acme")
                                                                (class-name "HelloWorld"))
                                                            (idee-expand-path "src/main/java/__package-path__/__class-name__.java")))))
(ert-deftest arch/test-expand-path-in-let()
  "Should successfully extract exposes ports from Dockerfile"
  (should (equal "src/main/java/org/acme/HelloWorld.java" (let* ((package-path "org/acme")
                                                                 (class-name "HelloWorld")
                                                                 (expanded-path (idee-expand-path "src/main/java/__package-path__/__class-name__.java")))
                                                            expanded-path))))
(provide 'idee-arch-test)
;;; idee-arch-test.el ends here
