;;; idee-visitors.el --- Visitors

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

(require 'projectile)

(defvar idee-project-visitors)
(setq idee-project-visitors ())

(defun idee-project-visit()
  "Call all registered visitors."
  (interactive)
  (let (v)
    (dolist (v idee-project-visitors)
      (funcall v default-directory))))

(add-to-list 'idee-project-visitors 'idee-visitor-clojure)
(add-to-list 'projectile-after-switch-project-hook 'idee-project-visit)

(provide 'idee-visitors)
;;;  idee-visitors.el ends here
