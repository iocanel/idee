;;; idee-projects-test.el --- IDEE projects test

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

(ert-deftest project-root-dir-test/from-root ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir ".git")
   (should (equal (ide-project-root-dir root-sandbox-path) (f-full root-sandbox-path)))))

(ert-deftest project-root-dir-test/from-root-with-projectile ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir ".projectile")
   (should (equal (ide-project-root-dir root-sandbox-path) (f-full root-sandbox-path)))))

(ert-deftest project-root-dir-test/from-module ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir ".git")
   (f-mkdir "module")
   (f-mkdir "module/src")
   (let ((current-dir (concat (file-name-as-directory root-sandbox-path) "module/src")))
   (should (equal (ide-project-root-dir current-dir) (f-full root-sandbox-path))))))

(ert-deftest project-state-test/intial-name ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir "test-project")
   (let ((default-directory (f-join default-directory "test-project")))
     (f-mkdir ".git")
     (should (equal "test-project" (ide-project-name-get))))))

(ert-deftest project-state-test/intial-path ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir "test-project")
   (let ((default-directory (f-join default-directory "test-project")))
     (f-mkdir ".git")
     (should (equal (projectile-project-root) (ide-project-info-path (ide-project-init)))))))

(ert-deftest project-state-test/set-version ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir "test-project")
   (let ((default-directory (f-join default-directory "test-project")))
     (f-mkdir ".git")
     (ide-project-version-set "1.0")
     (should (equal (ide-project-version-get) "1.0")))))

(ert-deftest project-state-test/set-property ()
  "Should successfully set a property on the project."
  (with-sandbox
   (f-mkdir "test-project")
   (let ((default-directory (f-join default-directory "test-project")))
     (f-mkdir ".git")
     (ide-project-property-set "foo" "bar")
     (should (equal (ide-project-property-get "foo") "bar")))))

(provide 'idee-projects-test)
;;; idee-projects-test.el ends here
