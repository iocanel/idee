;; idee-git-test.el --- IDE git test

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


(ert-deftest git/test-git-log-entry-sha()
  "Should successfully extract exposes ports from Dockerfile"
 (should (equal "12345" (idee/git-log-entry-sha "commit 12345\nAuthor: Ioannis Canellos <iocanel@gmail.com>\nDate:"))))

(ert-deftest git/test-git-log-entry-author()
  "Should successfully extract exposes ports from Dockerfile"
 (should (equal "Ioannis Canellos" (idee/git-log-entry-author  "commit 12345\nAuthor: Ioannis Canellos <iocanel@gmail.com>\nDate:"))))

(ert-deftest git/test-git-log-entry-message()
  "Should successfully extract exposes ports from Dockerfile"
 (should (equal "feat: this is a test." (idee/git-log-entry-message "commit 12345\nAuthor: Ioannis Canellos <iocanel@gmail.com>\nDate: Tue\n    feat: this is a test."))))
 
(provide 'idee-git-test)
;; idee-git-test.el ends here
