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
(defvar idee-maven-project-settings-commands ())

(defun idee-maven-module-root-dir-p (f)
  "Return non-nil if F is a maven module directory."
  (if (and f (file-directory-p f)) (seq-filter (lambda (x) (equal pom-xml x)) (directory-files f))
    nil))

(defun idee-maven-module-root-dir (&optional f)
  "Find the directory of the maven module that owns the source file F."
  (let* ((file-name (buffer-file-name (current-buffer)))
         (dir (if file-name (directory-file-name file-name) default-directory))
         (current-dir (f-full (if f f (file-name-directory dir)))))
    (while (and current-dir (not (idee-maven-module-root-dir-p current-dir)))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    current-dir))

(defun idee-maven-enclosuing-module-root-dir (&optional f)
  "Find the directory of the parent maven module that owns the source file F."
  (let* ((file-name (buffer-file-name (current-buffer)))
         (dir (if file-name (directory-file-name file-name) default-directory))
         (current-dir (f-full (if f f (file-name-directory dir))))
         (module-dir (idee-maven-module-root-dir current-dir))
         (parent-dir (idee-maven-module-root-dir (file-name-directory  (directory-file-name module-dir)))))
    (while (and parent-dir (not (idee-maven-module-root-dir-p parent-dir)))
      (setq parent-dir (file-name-directory (directory-file-name parent-dir))))
    parent-dir))

(defun idee-maven-invoker-test-dir-p (&optional f)
  "Return non-nil if the file is part of a maven-invoker integration test."
  (let* ((file-name (buffer-file-name (current-buffer)))
         (dir (if file-name (directory-file-name file-name) default-directory))
         (current-dir (f-full (if f f (file-name-directory dir)))))
    (string-match-p (regexp-quote (f-join "src" "it")) current-dir)))

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

(defun idee-maven-pom-version (pom)
  "Get the version of the specified POM."
  (if (file-exists-p pom)
      (with-temp-buffer
        (insert-file-contents pom)
        (let* ((xml (libxml-parse-xml-region (point-min) (point-max)))
               (p (assoc 'project xml))
               (project (if p p (cdr xml))))
          (car (cdr (cdr (assoc 'version project))))))
    nil))

(defun idee-maven-edit-project-pom-xml ()
  "Edit the current project pom."
  (interactive)
  (let* ((project-pom (concat (projectile-project-root) pom-xml)))
    (if (file-exists-p project-pom)
        (progn (idee-jump-to-non-ide-window)
               (find-file project-pom)
               (idee-refresh-view)))))

(defun idee-maven-clean-project ()
  "Clean the current maven project."
  (interactive)
  (idee-maven-exec :goals "clean"))

(defun idee-maven-package-project ()
  "Package the current maven project."
  (interactive)
  (idee-maven-exec :goals "clean package"))

(defun idee-maven-install-project ()
  "Install the current maven project."
  (interactive)
  (idee-maven-exec :goals "clean install"))

(defun idee-maven-debug-project ()
  "Build the current maven project."
  (interactive)
  (let* ((module-dir (idee-maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (artifact-id (idee-maven-pom-artifact-id module-pom)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 8000
              :project-name artifact-id
              :wait-for-port t)
      (append (list :program-to-start (idee-maven-cmd :goals "clean install" :debug t)))
      dap-debug)))

(defun idee-maven-surefire-debug-project ()
  "Debug the current maven project."
  (interactive)
  (let* ((module-dir (idee-maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (artifact-id (idee-maven-pom-artifact-id module-pom)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :project-name artifact-id
              :wait-for-port t)
        (append (list :program-to-start (idee-maven-cmd :goals "clean install" :surefire-debug t)))
        dap-debug)))

(defun idee-maven-failsafe-debug-project ()
  "Debug the current maven project."
  (interactive)
  (let* ((module-dir (idee-maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (artifact-id (idee-maven-pom-artifact-id module-pom)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :project-name artifact-id
              :wait-for-port t)
      (append (list :program-to-start (idee-maven-cmd :goals "clean install" :failsafe-debug t)))
      dap-debug)))

(defun idee-maven-clean-module ()
  "Clean the current maven module."
  (interactive)
  (idee-maven-exec :goals "clean" :build-scope 'module))

(defun idee-maven-edit-module-pom-xml ()
  "Edit the current maven module pom."
  (interactive)
  (let* ((module-dir (idee-maven-module-root-dir))
         (module-pom (concat module-dir pom-xml)))
    (if  (file-exists-p module-pom)
        (progn (idee-jump-to-non-ide-window)
               (find-file module-pom)
               (idee-refresh-view)))))

(defun idee-maven-package-module ()
  "Package the current maven module."
  (interactive)
  (idee-maven-exec :goals "clean package" :build-scope 'module))

(defun idee-maven-install-module ()
  "Install the current maven module."
  (interactive)
  (idee-maven-exec :goals "clean install" :build-scope 'module))

(defun idee-maven-also-install-module ()
  "Install the current maven module (but also make dependencies in the reactor)."
  (interactive)
  (idee-maven-exec :goals "clean install" :build-scope 'module :also-make t))

(defun idee-maven-resume-from-module ()
  "Install the current maven module (but also make dependencies in the reactor)."
  (interactive)
  (idee-maven-exec :goals "clean install" :build-scope 'resume))

(defun idee-maven-exec-module ()
  "Build the current maven module."
  (interactive)
  (idee-maven-exec :goals "clean install" :build-scope 'module))

(defun idee-maven-debug-module ()
  "Debug the current maven module."
  (interactive)
  (let* ((module-dir (idee-maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee-maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee-maven-enclosuing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee-maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee-maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 8000
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee-maven-cmd :goals "clean install" :debug t :build-scope 'module)))
        dap-debug)))

(defun idee-maven-surefire-debug-module ()
  "Debug the current maven module."
  (interactive)
  (let* ((module-dir (idee-maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee-maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee-maven-enclosuing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee-maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee-maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee-maven-cmd :goals "clean install" :surefire-debug t :build-scope 'module)))
        dap-debug)))

(defun idee-maven-failsafe-debug-module ()
  "Debug the current maven module."
  (interactive)
  (let* ((module-dir (idee-maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee-maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee-maven-enclosuing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee-maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee-maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee-maven-cmd :goals "clean install" :failsafe-debug t :build-scope 'module)))
        dap-debug)))

(defun idee-maven-exec-from-history ()
  "Prompt the user to execute previous maven build from history."
  (interactive)
  (let ((maven-command (completing-read "Maven command:" idee-maven-exec-history)))
    (idee-eshell-project-command-enqueue maven-command)))

(defun idee-maven-exec-from-project-settings ()
  "Prompt the user to execute previous maven build from history."
  (interactive)
  (idee-with-project-settings "maven.el" idee-maven-project-settings-commands
    (let ((maven-command (completing-read "Maven command:" idee-maven-project-settings-commands)))
      (idee-eshell-project-command-enqueue maven-command))))

(cl-defun idee-maven-cmd (&key goals debug surefire-debug failsafe-debug build-scope also-make)
  (idee-with-project-settings "maven.el" idee-maven-profiles
                              (let* ((module-dir (idee-maven-module-root-dir))
                                     (enclosuing-module-dir (idee-maven-enclosuing-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee-maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee-maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee-maven-pom-artifact-id enclosuing-module-pom))
                                     (profiles-opt (idee--maven-profiles-option))
                                     (mvn-cmd-builder nil))

                                (cond
                                 (invoker-test (add-to-list 'mvn-cmd-builder "mvn"))
                                 (debug (add-to-list 'mvn-cmd-builder "mvnDebug"))
                                 (t (add-to-list 'mvn-cmd-builder "mvn")))


                                (add-to-list 'mvn-cmd-builder goals)
                                (if profiles-opt
                                    (add-to-list 'mvn-cmd-builder profiles-opt))
                                (if artifact-id
                                    (cond
                                     (invoker-test
                                      (progn
                                        (add-to-list 'mvn-cmd-builder (format "-pl :%s -Dinvoker.test=%s" enclosing-artifact-id (file-name-nondirectory (directory-file-name module-dir))))
                                        (when debug (add-to-list 'mvn-cmd-builder "-Dinvoker.mavenExecutable=mvnDebug"))))
                                      ((equal 'resume build-scope) (add-to-list 'mvn-cmd-builder (format "-rf :%s" artifact-id)))
                                      ((equal 'module build-scope) (add-to-list 'mvn-cmd-builder (format "-pl :%s" artifact-id)))))
                                (if also-make
                                    (add-to-list 'mvn-cmd-builder "-am"))
                                (if surefire-debug
                                    (add-to-list 'mvn-cmd-builder "-Dmaven.surefire.debug"))
                                (if failsafe-debug
                                    (add-to-list 'mvn-cmd-builder "-Dmaven.failsafe.debug"))
                                (string-join (reverse mvn-cmd-builder) " "))))


(cl-defun idee-maven-exec (&key goals debug surefire-debug failsafe-debug build-scope also-make)
  "Build the current maven module."
  (interactive)
  (let ((cmd (idee-maven-cmd :goals goals :debug debug :surefire-debug surefire-debug :failsafe-debug failsafe-debug :build-scope build-scope :also-make also-make)))
    (add-to-list 'idee-maven-exec-history cmd)
    (idee-eshell-project-command-enqueue cmd)))

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
 Maven:   Project                     Module               Toggle          Execute 
        ------------------------------------------------------------------------------------
        _pc_: clean                  _mc_: clean              _to_: offline     _h_: from history
        _pp_: package                _mp_: package            _tt_: tests       _s_: from project settings 
        _pi_: install                _mi_: install   
        _po_: edit pom               _mo_: edit pom
                                  _mrf_: resume from
                                  _mai_: also install
        _pd_: debug                  _md_: debug
       _psd_: surfire debug         _msd_: surefire debug
       _pfd_: failsafe debug        _mfd_: failsafe debug

       "
  ("po" idee-maven-edit-project-pom-xml)
  ("pc" idee-maven-clean-project)
  ("pp" idee-maven-package-project)
  ("pi" idee-maven-install-project)
  ("pd" idee-maven-debug-project)
  ("psd" idee-maven-surefire-debug-project)
  ("pfd" idee-maven-failsafe-debug-project)

  ("mo" idee-maven-edit-module-pom-xml)
  ("mc" idee-maven-clean-module)
  ("mp" idee-maven-package-module)
  ("mi" idee-maven-install-module)
  ("mrf" idee-maven-resume-from-module)
  ("mai" idee-maven-also-install-module)
  ("md" idee-maven-debug-module)
  ("msd" idee-maven-surefire-debug-module)
  ("mfd" idee-maven-failsafe-debug-module)

  ("to" idee-maven-toggle-offline)
  ("tt" idee-maven-toggle-skip-tests)

  ("h" idee-maven-exec-from-history)
  ("s" idee-maven-exec-from-project-settings)
  ("q" nil "quit"))

;;; Project Factory
(defun idee-new-maven-from-archetype-project ()
  "Create a new maven from archetype project."
  (interactive)
  (let* ((group-id (read-string "Group Id:"))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:" "0.1-SNAPSHOT"))
         (target-dir (idee--select-new-project-dir))
         (generate-command (format "mvn archetype:generate -DgroupId=%s -DartifactId=%s -Dversion=%s -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false" group-id artifact-id version))
         (cleanup-command (format "mv %s/* . && rm -r %s" artifact-id artifact-id)))
    (idee-create-project-with-shell target-dir generate-command cleanup-command)))

(defconst idee-maven-project-factory
  (make-idee-project-factory
   :name "Maven"
   :description "New Maven project from archetype."
   :func 'idee-new-maven-from-archetype-project))

(add-to-list 'idee-project-factories-list idee-maven-project-factory)

;;; Project Visitor
(defun idee-maven-project-p (root)
  "Check if ROOT is the root path of a java project."
  (seq-filter (lambda (x)
                (or (equal pom-xml x)
              (directory-files root)))))

(defun idee-visitor-maven (root)
  "Check if a java project is available under the specified ROOT."
  (let ((project-pom (concat root pom-xml)))
    (when (file-exists-p project-pom)
         (setq idee-project-version (idee-maven-pom-version project-pom)))))

(add-to-list 'idee-project-visitors 'idee-visitor-maven)

(provide 'idee-maven)
;;; idee-maven.el ends here
