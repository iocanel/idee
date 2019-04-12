;;; idee-maven.el --- Maven support for IDEE -*- lexical-binding: t -*-

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
(require 'idee-projects)

(defconst pom-xml "pom.xml")
(defvar idee-maven-profiles ())

(defun idee-maven-module-root-dir-p (f)
  "Return non-nil if F is a maven module directory."
  (seq-filter (lambda (x)
                (equal pom-xml x))
              (directory-files f)))

(defun idee-maven-module-root-dir (&optional f)
  "Find the directory of the maven module that owns the source file F."
  (let ((current-dir (f-full (if f f (file-name-directory (directory-file-name (buffer-file-name (current-buffer))))))))
    (while (not (idee-maven-module-root-dir-p current-dir))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    current-dir))

(defun idee-maven-pom-artifact-id (pom)
  "Get the artifactId of the specified POM."
  (if (file-exists-p pom)
      (with-temp-buffer
        (insert-file-contents pom)
        (let* ((xml (libxml-parse-xml-region (point-min) (point-max)))
               (p (assoc 'project xml))
               (project (if p p (cdr xml))))
          (car (cdr (cdr (assoc 'artifactId project))))))
    nil))

(defun idee-maven-build-project ()
  "Build the current maven project."
  (interactive)

  (idee-with-project-settings "maven.el" idee-maven-profiles
  (let* ((module-dir (idee-maven-module-root-dir))
         (module-pom (concat module-dir pom-xml))
         (profiles-opt (idee--maven-profiles-option)))
    (idee-with-project-shell 
      (insert (format " mvn clean install %s\n" profiles-opt))))))

(defun idee-maven-build-module ()
  "Build the current maven module."
  (interactive)
  (idee-with-project-settings "maven.el" idee-maven-profiles
  (let* ((module-dir (idee-maven-module-root-dir))
         (module-pom (concat module-dir pom-xml))
         (artifact-id (idee-maven-pom-artifact-id module-pom))
         (profiles-opt (idee--maven-profiles-option)))

    (idee-with-project-shell 
      (insert (format " mvn clean install -pl :%s %s\n" artifact-id profiles-opt))))))

;;; Utilities
(defun idee--maven-profiles-option ()
  "Create the profiles option to be appended to any maven command.
   Returns something like: -Pprofile1,profile2 if profiles are enabled, 
   or empty string other wise."
  (if idee-maven-profiles
    (concat "-P" (string-join idee-maven-profiles ","))
    ""))

;;; Project Factory
(defun idee-new-maven-from-archetype-project ()
  "Create a new project from maven."
  (interactive)
  (let* ((group-id (read-string "Group Id:"))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:"))
         (recomended-dir (concat (file-name-as-directory default-directory) artifact-id))
         (temp-dir (concat temporary-file-directory "mvn-archetype-" (format "%06x-%06x" (random (expt 16 6)) (random (expt 16 6)))))
         (generated-dir (concat (file-name-as-directory temp-dir) artifact-id))
         (target-dir (idee--select-new-project-dir))
         (parent-dir (file-name-directory (directory-file-name target-dir)))
         (dir-name (substring target-dir (length parent-dir))))

    (make-directory temp-dir t)
    (setq default-directory temp-dir)
    (shell-command (format "mvn archetype:generate -DgroupId=%s -DartifactId=%s -Dversion=%s -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false" group-id artifact-id version))
    (message (format "moving generated project from: %s to %s." generated-dir target-dir))
    (shell-command (format "mv %s/* %s" generated-dir target-dir))
    (write-region "" nil (concat (file-name-as-directory target-dir) ".projectile"))
    (projectile-add-known-project target-dir)
    (setq projectile-project-root target-dir)
    (projectile-switch-project-by-name target-dir)
    (revert-buffer)
    (dired target-dir)
    (idee-ide-view)))

(defconst idee-maven-project-factory
  (make-idee-project-factory
   :name "Maven"
   :description "New Maven project from archetype."
   :func 'idee-new-maven-from-archetype-project))

(add-to-list 'idee-project-factories-list idee-maven-project-factory)

(provide 'idee-maven)
;;; idee-maven.el ends here
