;;; idee-maven-test.el --- IDEE maven test


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:


(require 'test-helper)
(require 'idee-utils (f-expand "idee-utils.el" root-code-path))
(require 'idee-vars (f-expand "idee-vars.el" root-code-path))
(require 'idee-views (f-expand "idee-views.el" root-code-path))
(require 'idee-dap (f-expand "idee-dap.el" root-code-path))
(require 'idee-projects (f-expand "idee-projects.el" root-code-path))
(require 'idee-visitors (f-expand "idee-visitors.el" root-code-path))
(require 'idee-maven (f-expand "idee-maven.el" root-code-path))
 

(ert-deftest maven/should-return-mvn-clean-install ()
  "Should properly comment java."
    (should (equal (idee-maven-cmd :goals "clean install") "mvn clean install")))

(provide 'idee-maven-test)
;;; idee-maven-test.el ends here.
