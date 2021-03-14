;;; idee-utils-test.el --- Utilities tests  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Ioannis Canellos 
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

(ert-deftest util/should-convert-camel-case-to-kebab ()
  (should (equal nil (idee-string-camelcase-to-kebabcase nil)))
  (should (equal "my" (idee-string-camelcase-to-kebabcase "My")))
  (should (equal "my-class" (idee-string-camelcase-to-kebabcase "MyClass")))
  (should (equal "my-a-class" (idee-string-camelcase-to-kebabcase "MyAClass")))
)

(ert-deftest util/should-find-projectile-project-root ()
  "Should find project root."
  (with-sandbox
   (f-mkdir ".git")
   (should (equal (file-name-as-directory root-sandbox-path) (projectile-project-root)))))

;; (ert-deftest util/should-not-find-projectile-project-root ()
;;   "Should not find project root."
;;   (with-sandbox
;;    (should-not (projectile-project-p (projectile-project-root)))))

(ert-deftest util/should-find-project-settings ()
  "Should find project settings."
  (with-sandbox
   (f-mkdir ".git")
   (f-mkdir ".idee")
   (f-touch ".idee/settings.el")
   (should (equal (concat root-sandbox-path "/.idee/settings.el") (idee-project-settings "settings.el")))))

(ert-deftest util/should-not-find-project-settings ()
  "Should find not project settings."
  (with-sandbox
   (f-mkdir ".git")
   (should-not (file-exists-p (idee-project-settings "settings.el")))))

(ert-deftest util/should-load-project-settings ()
  "Should load project settings."
  (setq test-var nil)
  (with-sandbox
   (f-mkdir ".git")
   (f-mkdir ".idee")
   (f-touch ".idee/settings.el")
   (f-write-text "(setq test-var 'foo-bar-baz)" 'utf-8 (f-expand ".idee/settings.el" root-sandbox-path))
   (idee-with-project-settings "settings.el"
     (should (equal test-var 'foo-bar-baz)))))

(ert-deftest util/should-try-to-load-missing-project-settings ()
  "Should try to load project settings, without error (when settings are missing.)."
  (setq test-var nil)
  (with-sandbox
   (f-mkdir ".git")
   (idee-with-project-settings "settings.el"
     (should-not test-var))))

(ert-deftest util/should-try-to-load-settings-from-missing-project ()
  "Should try to load project settings, without error (when project is missing.)."
  (setq test-var nil)
  (with-sandbox
   (idee-with-project-settings "settings.el"
     (should-not test-var)))) 

(provide 'idee-utils-test)
;;; idee-utils-test.el ends here.
