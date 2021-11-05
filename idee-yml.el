;; idee-yml.el --- YML -*- lexical-binding: t -*-

;; Copyright (C) 2021 Ioannis Canellos 
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

(defcustom idee/comment-yml-custom-block-beginning nil "Custom block comment beginning to use when commenting yml code." :group 'idee/yml :type 'string)
(defcustom idee/comment-yml-custom-block-ending nil "Custom block comment ending to use when commenting yml code." :group 'idee/yml :type 'string)
(defcustom idee/comment-yml-custom-line-prefix "# " "Custom line prefix to use when commenting yml code." :group 'idee/yml :type 'string)

(defconst yml-comment-style (make-idee/comment-style :line-prefix " #"
                                                      :custom-block-beginning idee/comment-yml-custom-block-beginning :custom-line-prefix idee/comment-yml-custom-line-prefix :custom-block-ending idee/comment-yml-custom-block-ending))


;;;###autoload
(defun idee/yml-init ()
  (interactive)
  "Initialize yml settings"
  (add-to-list 'idee/type-comment-styles-alist `("yml" . ,yml-comment-style))
  (add-to-list 'idee/type-comment-styles-alist `("yaml" . ,yml-comment-style)))

(provide 'idee-yml)
;; idee-yml.el ends here
