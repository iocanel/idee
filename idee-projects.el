;;; idee-projects.el --- Project Factories

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

;; Package-Requires: ((emacs "25.1"))

;; Commentary:

;;; Code:

(defun idee-new-project-function()
  "Create a new project"
  (interactive)
  (let ((factory (idee--select-project-factory)))
    (funcall (idee-project-factory-func factory))
    )
  )

(defun idee--select-new-project-dir()
  "Select a new project directory."
  (interactive)
    (ido-file-internal 'dired 'dired nil "Select project directory:" 'dir)
    (dired-current-directory))

(defun idee--select-project-factory()
  "Select a project factory from the list of registered factories"
  (let ((factory (projectile-completing-read "Select project type:"
                                             (mapcar 'idee--project-factory-entry idee-project-factories-list))))

    (car (seq-filter
          (lambda (f)
            (equal (idee-project-factory-name f) (car (split-string factory " ")))) idee-project-factories-list))
    )
  ) 

(defun idee--project-factory-entry (f)
  "Create an entry for the specified project factory F."
  (concat (idee-project-factory-name f) " - " (idee-project-factory-description f))
  )

(provide 'idee-projects)
;;; idee-projects.el ends here.
