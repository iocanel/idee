;;; idee-spring.el --- Description

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

;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(require 'idee-utils)

(defvar idee-spring-languages '("java" "groovy" "kotlin"))
(defvar idee-spring-project-types '("maven-project" "gradle-project"))
(defvar idee-spring-dependencies '("web" "security" "jpa" "actuator"))

(defvar idee-spring-extract-dir nil)
(defvar idee-spring-extract-dir-buffer nil)

(defun idee-new-spring-starter-project ()
  "Create a new project from https://start.spring.io."
  (interactive)
  (let ((language) (project-type) (dependencies))
    (setq language (completing-read "Select language: " idee-spring-languages))
    (setq project-type (completing-read "Select build tool: " idee-spring-project-types))
    (setq dependencies (completing-read-multiple "Select dependencies: " idee-spring-dependencies))
    (setq idee-spring-extract-dir (idee--select-new-project-dir))
    (setq idee-spring-extract-dir-buffer (current-buffer))
    (idee-http-post "https://start.spring.io/starter.zip" `(
                                                            ("language" . ,language)
                                                            ("type" . ,project-type)
                                                            ("dependencies" . ,(mapconcat 'identity dependencies ",")))
                    'idee-spring-starter-download-callback
    ) 
    )
  )

(defconst idee-spring-starter-project-factory
  (make-idee-project-factory
   :name "Spring"
   :description "New Spring project created using https://start.spring.io"
   :func 'idee-new-spring-starter-project))

(add-to-list 'idee-project-factories-list idee-spring-starter-project-factory)

(defun idee-spring-starter-download-callback (s)
  "Download callback for spring starter http request."
  (let ((zip (concat temporary-file-directory "spring-" (format "%06x-%06x" (random (expt 16 6)) (random (expt 16 6))) ".zip"))) 
  (write-region (point-min) (point-max) zip)
  (call-process-shell-command (format "unzip %s -d %s" zip idee-spring-extract-dir))
  (set-buffer idee-spring-extract-dir-buffer)
  (revert-buffer)
  )
)

(provide 'idee-spring)
;;; idee-spring.el ends here
