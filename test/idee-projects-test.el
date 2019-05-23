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
(require 'idee-eshell (f-expand "idee-eshell.el" root-code-path))
(require 'idee-views (f-expand "idee-views.el" root-code-path))
(require 'idee-vars (f-expand "idee-vars.el" root-code-path))
(require 'idee-actions (f-expand "idee-actions.el" root-code-path))
(require 'idee-projects (f-expand "idee-projects.el" root-code-path))

(ert-deftest project-root-dir-test/from-root ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir ".git")
   (should (equal (idee-project-root-dir root-sandbox-path) (f-full root-sandbox-path)))))

(ert-deftest project-root-dir-test/from-root-with-projectile ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir ".projectile")
   (should (equal (idee-project-root-dir root-sandbox-path) (f-full root-sandbox-path)))))

(ert-deftest project-root-dir-test/from-module ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir ".git")
   (f-mkdir "module")
   (f-mkdir "module/src")
   (let ((current-dir (concat (file-name-as-directory root-sandbox-path) "module/src")))
   (should (equal (idee-project-root-dir current-dir) (f-full root-sandbox-path))))))

(ert-deftest project-state-test/intial-name ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir "test-project")
   (let ((default-directory (f-join default-directory "test-project")))
     (f-mkdir ".git")
     (should (equal "test-project" (idee-project-get-name))))))

(ert-deftest project-state-test/intial-path ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir "test-project")
   (let ((default-directory (f-join default-directory "test-project")))
     (f-mkdir ".git")
     (should (equal (projectile-project-root) (idee-project-info-path (idee-project-info)))))))

(ert-deftest project-state-test/set-version ()
  "Should use `default-directory' when no argument."
  (with-sandbox
   (f-mkdir "test-project")
   (let ((default-directory (f-join default-directory "test-project")))
     (f-mkdir ".git")
     (should-not (idee-project-get-version))
     (idee-project-set-version "1.0")
     (should (equal (idee-project-get-version) "1.0")))))

(ert-deftest project-state-test/set-property ()
  "Should successfully set a property on the project."
  (with-sandbox
   (f-mkdir "test-project")
   (let ((default-directory (f-join default-directory "test-project")))
     (f-mkdir ".git")
     (should-not (idee-project-get-property "foo"))
     (idee-project-set-property "foo" "bar")
     (should (equal (idee-project-get-property "foo") "bar")))))

(provide 'idee-projects-test)
;;; idee-projects-test.el ends here
