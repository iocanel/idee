;;; idee-visitors.el --- Visitors -*- lexical-binding: t -*-

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

;;; Commentary:

;;; Code:

(require 'projectile)
(require 'idee-vars)

;;;###autoload (autoload 'ide-visitor-register "idee-visitors")
(defmacro ide-visitor-register (visitor)
  "Register a VISITOR."
  (list 'push  visitor 'ide-visitor-list))

;;;###autoload
(defun ide-visitor-apply()
  "Call all registered visitors."
  (dolist (v ide-visitor-list)
    (funcall v default-directory)))

;;;###autoload
(defun ide-visitor-setup ()
  (add-to-list 'projectile-after-switch-project-hook 'ide-visitor-apply))

(provide 'idee-visitors)
;;;  idee-visitors.el ends here
