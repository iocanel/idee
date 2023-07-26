;; idee-spring.el --- Description

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

;;; Commentary:

;;; Code:

(require 'idee-utils)
(require 'idee-views)
(require 'idee-projects)

(defvar idee/spring-languages '("java" "groovy" "kotlin"))
(defvar idee/spring-project-types '("maven-project" "gradle-project"))
(defvar idee/spring-dependencies '("web" "security" "data-jpa" "actuator" "postgresql" "data-rest"))

(defvar idee/spring-extract-dir nil)
(defvar idee/spring-extract-dir-buffer nil)

(defcustom idee/spring-init-group-id "org.acme" "The initial value for group-id in the spring project factory." :group 'idee/spring :type 'string)

;;;###autoload
(defun idee/new-spring-starter-project (&optional create-function)
  "Create a new project from https://start.spring.io.
The command supports accepting an external CREATE-FUNCTION or defaults to idee/project-create-with-shell."
  (interactive)
  (let* ((language (completing-read "Select language: " idee/spring-languages))
         (project-type (completing-read "Select build tool: " idee/spring-project-types))
         (dependencies (completing-read-multiple "Select dependencies: " idee/spring-dependencies))
         (group-id (read-string "Group Id:" idee/spring-init-group-id))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:" "0.1-SNAPSHOT"))
         (target-dir (idee/project-dir-select))
         (generate-command (format "spring init -g %s -a %s -v %s -d%s %s" group-id artifact-id version (mapconcat 'identity dependencies ",") artifact-id))
         (cleanup-command (format "mv %s/* . && rm -r %s" artifact-id artifact-id)))
    (funcall (or create-function 'idee/project-create-with-shell) target-dir generate-command cleanup-command)
    (idee/project-name-set artifact-id)
    (idee/project-version-set version)))

(defun idee/new-spring-starter-project-internal ()
  "Create a new project from https://start.spring.io."
  (interactive)
  (let ((language) (project-type) (dependencies))
    (setq language (completing-read "Select language: " idee/spring-languages))
    (setq project-type (completing-read "Select build tool: " idee/spring-project-types))
    (setq dependencies (completing-read-multiple "Select dependencies: " idee/spring-dependencies))
    (setq idee/spring-extract-dir (idee/project-dir-select))
    (setq idee/spring-extract-dir-buffer (current-buffer))
    (idee/http-post "https://start.spring.io/starter.zip" `(
                                                            ("language" . ,language)
                                                            ("type" . ,project-type)
                                                            ("dependencies" . ,(mapconcat 'identity dependencies ",")))
                    'idee/spring-starter-download-callback)))

(defun idee/spring-starter-download-callback (s)
  "Download callback for spring starter http request."
  (let ((project-name) (zip (concat temporary-file-directory "spring-" (format "%06x-%06x" (random (expt 16 6)) (random (expt 16 6))) ".zip"))) 
  (setq project-name (file-name-nondirectory (directory-file-name (file-name-directory idee/spring-extract-dir))))
  (write-region (point-min) (point-max) zip)
  (call-process-shell-command (format "unzip %s -d %s" zip idee/spring-extract-dir))
  (set-buffer idee/spring-extract-dir-buffer)
  (setq default-directory (file-name-as-directory idee/spring-extract-dir))
  (message (format "default dir: %s" default-directory))
  (call-process-shell-command "git init")
  (write-region "" nil (concat (file-name-as-directory idee/spring-extract-dir) ".project"))


  (add-to-list 'project-list path)
  (project-switch-project path)
  (revert-buffer)
  (dired idee/spring-extract-dir)))

(provide 'idee-spring)
;;; idee-spring.el ends here
