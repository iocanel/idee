;;; idee-arch.el --- Arch -*- lexical-binding: t -*-

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

;; The concept of archetypes if borrowed from the maven world, but its not exactly the same.
;; In the context of this tool an archetype is an elsip function that somehow modifies your project.
;; In most cases it should be a function that applies multiple templates to the project. 

;;; Code:
(require 'idee-vars)

(cl-defstruct idee/archetype
  name
  description
  func)

(defvar idee/archetype-list nil "A list of all the available archetypes.")

(defun idee/archetype-expand-path (path)
  "Expand the specified PATH."
  (with-temp-buffer
    (insert path)
    (goto-char (point-min))
    (while (re-search-forward "__\\([a-zA-Z0-9_-]+\\)__" nil t)
      (let* ((key (match-string 1))
             (value (if (boundp (intern key))
                        (symbol-value (intern key))
                      nil))
             (end (point))
             (start (- end 4 (length key))))
       (replace-region-contents  start end (lambda () (format "%s" value)))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun idee/archetype-register (archetype)
  "Register a ARCHETYPE."
  (setq idee/archetype-list (delq archetype idee/archetype-list))
  (setq idee/archetype-list (add-to-list  'idee/archetype-list archetype)))

(defun idee/archetype-select()
  "Select an archetype from the list of registered archetype."
  (let ((archetype (completing-read "Select archetype:"
                                             (mapcar 'idee/archetype-entry idee/archetype-list))))
    (car (seq-filter
          (lambda (a)
            (equal archetype (idee/archetype-entry a))) idee/archetype-list))))

(defun idee/archetype-entry (f)
  "Create an entry for the specified project factory F."
  (concat (idee/archetype-name f) " - " (idee/archetype-description f)))

;;;###autoload 
(defun idee/archetype-run()
  "Run archetype."
  (interactive)
  (let* ((archetype (idee/archetype-select))
         (func (idee/archetype-func archetype)))
    (when archetype (funcall func))))

(provide 'idee-arch)
;;; idee-arch.el ends here
