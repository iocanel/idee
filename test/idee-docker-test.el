;; idee-docker-test.el --- IDE docker test

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


(ert-deftest dockerfile/test-exposed-ports()
  (with-sandbox
   (f-mkdir ".git")
   (let ((dockerfile-no-ports (f-join root-test-assets-path "Dockerfile.no-ports"))
         (dockerfile-single-port (f-join root-test-assets-path "Dockerfile.single-port"))
         (dockerfile-multiple-ports (f-join root-test-assets-path "Dockerfile.multiple-ports")))


     (should-not (idee/docker-get-exposed-ports dockerfile-no-ports))
     (should (equal "8080" (car (idee/docker-get-exposed-ports dockerfile-single-port))))
     (should (equal (list "8080" "9090") (idee/docker-get-exposed-ports dockerfile-multiple-ports))))))

(provide 'idee-docker-test)
;; idee-projects-test.el ends here
