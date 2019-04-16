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
(defvar idee-maven-offline nil)
(defvar idee-maven-skip-tests nil)
(defvar idee-maven-exec-history ())

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

(defun idee-maven-clean-project ()
  "Clean the current maven project."
  (interactive)
  (idee-maven-exec :goals "clean"))

(defun idee-maven-install-project ()
  "Install the current maven project."
  (interactive)
  (idee-maven-exec :goals "clean install"))

(defun idee-maven-debug-project ()
  "Build the current maven project."
  (interactive)
  (idee-maven-exec :goals "clean install" :debug t))

(defun idee-maven-install-module ()
  "Install the current maven module."
  (interactive)
  (idee-maven-exec :goals "clean install" :module-build t))

(defun idee-maven-also-install-module ()
  "Install the current maven module (but also make dependencies in the reactor)."
  (interactive)
  (idee-maven-exec :goals "clean install" :module-build t :also-make t))

(defun idee-maven-exec-module ()
  "Build the current maven module."
  (interactive)
  (idee-maven-exec :goals "clean install" :module-build t))
 
(defun idee-maven-debug-module ()
  "Build the current maven module."
  (interactive)
  (idee-maven-exec :goals "clean install" :debug t :module-build t))

(defun idee-maven-exec-from-history ()
  "Prompt the user to execute previous maven build from history."
  (interactive)
  (let ((maven-command (completing-read "Maven command:" idee-maven-exec-history)))
    (idee-with-project-shell (insert maven-command))))
 
(cl-defun idee-maven-exec (&key goals debug module-build also-make)
  "Build the current maven module."
  (interactive)
  (idee-with-project-settings "maven.el" idee-maven-profiles
  (let* ((module-dir (idee-maven-module-root-dir))
         (module-pom (concat module-dir pom-xml))
         (artifact-id (idee-maven-pom-artifact-id module-pom))
         (profiles-opt (idee--maven-profiles-option))
         (mvn-cmd-builder nil))
    
    (add-to-list 'mvn-cmd-builder (if debug "mvnDebug" "mvn"))
    (add-to-list 'mvn-cmd-builder goals)
    (if profiles-opt
        (add-to-list 'mvn-cmd-builder profiles-opt))
    (if (and artifact-id module-build)
        (add-to-list 'mvn-cmd-builder (format "-pl :%s" artifact-id)))
    (if also-make
        (add-to-list 'mvn-cmd-builder "-am"))

    (let ((mvn-command (string-join (reverse mvn-cmd-builder) " ")))
      (add-to-list 'idee-maven-exec-history mvn-command t)
      (idee-with-project-shell 
          (insert mvn-command))))))

;;; Toggles
(defun idee-maven-toggle-offline ()
  "Toggle offline flag for maven builds."
  (interactive)
  (if (idee-toggle idee-maven-offline)
      (message "Maven offline: Enabled!")
      (message "Maven offline: Disabled!")))

(defun idee-maven-toggle-skip-tests ()
  "Toggle offline flag for maven builds."
  (interactive)
  (if (idee-toggle idee-maven-skip-tests)
      (message "Maven test skip: Enabled!")
      (message "Maven test skip: Disabled!")))

;;; Utilities
(defun idee--maven-profiles-option ()
  "Create the profiles option to be appended to any maven command.
   Returns something like: -Pprofile1,profile2 if profiles are enabled, 
   or empty string other wise."
  (if idee-maven-profiles
    (concat "-P" (string-join idee-maven-profiles ",")) ""))

;;; Maven Hydra
(defhydra idee-maven-hydra (:hint nil :exit t)
"
 Maven: ^ Project          Module                 Toggle         Execute 
        ^^^^^^----------------------------------------------------------------------
        _pc_: clean       _mc_: clean            _to_: offline   _h_: from history
        _pi_: install     _mi_: install          _tt_: tests
        _pd_: debug       _mai_: also install
                        _md_: debug
"
  ("pc" idee-maven-clean-project)
  ("pi" idee-maven-install-project)
  ("pd" idee-maven-debug-project)

  ("mc" idee-maven-clean-module)
  ("mi" idee-maven-install-module)
  ("mai" idee-maven-also-install-module)
  ("md" idee-maven-debug-module)

  ("to" idee-maven-toggle-offline)
  ("tt" idee-maven-toggle-skip-tests)

  ("h" idee-maven-exec-from-history)
  ("q" nil "quit"))

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
