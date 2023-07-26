;; idee-comments-test.el --- IDE comments test

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

(ert-deftest idee/comments/comment-sh ()
  "Should properly comment shell."
    (should (equal (idee/comment "bingo" "sh") "# bingo\n")))

(ert-deftest idee/comments/comment-el ()
  "Should properly comment elisp."
    (should (equal (idee/comment "bingo" "el") ";; bingo\n")))

(ert-deftest idee/comments/comment-xml ()
  "Should properly comment xml."
    (should (equal (idee/comment "bingo" "xml") "<!--\nbingo\n-->\n")))

(provide 'idee-comments-test)
;; idee-comments-test.el ends here
