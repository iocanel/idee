;;; eide-visitors.el --- Visitors

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

(defvar eide-project-visitors)
(setq eide-project-visitors ())

(defun eide-project-visit()
  "Calls all registered visitors"
  (interactive)
  (let (v)
    (dolist (v eide-project-visitors)
      (funcall v (projectile-project-root)))
    ) 
  ) 
;; Clojure
(defun eide-visitor-clojure (root)
  (when (seq-filter (lambda (x) (equal "project.clj" x)) (directory-files root))
    (clojure-ide))
  ) 

(add-to-list 'eide-project-visitors 'eide-visitor-clojure)
(advice-add 'projectile-switch-project :after 'eide-project-visit)

(provide 'eide-visitors)
;;;  eide-visitors.el ends here
