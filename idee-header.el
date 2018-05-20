;;; idee-header.el --- Emacs IDE Header support.

;; Copyright (C) 2018 Ioannis Canellos

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;       http://www.apache.org/licenses/LICENSE-2.0

;;Unless required by applicable law or agreed to in writing, software
;;distributed under the License is distributed on an "AS IS" BASIS,
;;WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;See the License for the specific language governing permissions and
;;limitations under the License.

;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(provide 'idee-header)

(defun idee-read-header()
  "Read the header from header.txt."
  (interactive)
  (let ((h (idee-read-and-eval-template(concat projectile-project-root "header.txt"))))
     (concat idee-comment-above
             (format "%s" (mapcar 
                           (lambda (l) (concat idee-comment-prefix l "\n"))
                           (split-string h "\n")))
             idee-comment-below)) 
  )

(defun idee-render-line (line)
  "Renders the specified LINE evaluating backquoted lisp expressions."
 (let (t (yas--make-template :table       nil ;; no tables for ephemeral snippets
                                   :key         "key" 
                                   :content     line
                                   :name        "a line" 
                                   :expand-env  t))
   (yas-expand-snippet t)

   )
  )

;;; idee-header.el ends here
