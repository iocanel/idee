;;; idee-counsel.el --- Counsel Utilities -*- lexical-binding: t -*-

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

;; Package-Requires: ((emacs "25.1") (counsel "0.13.0"))

;;; Commentary:

;;; Code:

(require 'counsel)                     

(defun idee-error-filter (list)
  "Stip dublicates from the list.
   Credits: https://stackoverflow.com/questions/3815467/stripping-duplicate-elements-in-a-list-of-strings-in-elisp."
  (let ((new-list nil))
    (while list
      (let  ((current (car list)))
        (when (and current
                   (not (member current new-list))
                   (string-match-p (regexp-quote "ERROR") current))
          (setq new-list (cons current new-list))))
        (setq list (cdr list)))
    (nreverse new-list)))

(defun idee-shell-show-errors ()
  (interactive)
  "Show the compilation errors as they appear on the shell."
  (when (require 'counsel nil t)
    (idee-with-project-shell
        (counsel-compilation-errors))))

(when (require 'counsel nil t) (advice-add 'counsel-compilation-errors-cands :filter-return #'idee-error-filter))

(provide 'idee-counsel)
;;; idee-counsel.el ends here
