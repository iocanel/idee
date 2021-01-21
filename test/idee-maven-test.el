;;; idee-maven-test.el --- IDEE maven test


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:


(require 'test-helper)


(ert-deftest maven/should-return-mvn-clean-install ()
  "Should properly comment java."
    (should (equal (idee-maven-cmd :goals "clean install") "mvn clean install")))

(provide 'idee-maven-test)
;;; idee-maven-test.el ends here.
