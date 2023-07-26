;; idee-maven.el --- Maven support for IDE -*- lexical-binding: t -*-

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

;;; Commentary:

;;; Code:

(require 'idee-java-utils)
(require 'idee-dap)
(require 'idee-utils)
(require 'idee-projects)
(require 'idee-visitors)

(require 'hydra)

(defconst pom-xml "pom.xml")
(defconst idee/dot-pom-regex "(^.)?\\.pom")

(defvar idee/maven-profiles ())
(defvar idee/maven-offline nil)
(defvar idee/maven-skip-tests nil)
(defvar idee/maven-clean t)
(defvar idee/maven-show-errors nil)
(defvar idee/maven-exec-history ())
(defvar idee/maven-project-settings-commands ())
(defvar idee/maven-known-group-ids '() "A list of known group-ids")

(defcustom idee/maven-init-group-id "org.acme" "The initial value for group-id in the maven project factory." :group 'idee/maven :type 'string)

(defun idee/maven-module-root-dir-p (f)
  "Return non-nil if F is a maven module directory."
  (if (and f (file-directory-p f)) (seq-filter (lambda (x) (equal pom-xml x)) (directory-files f))
    nil))

(defun idee/maven-module-root-dir (&optional f)
  "Find the directory of the maven module that owns the source file F."
  (let* ((file-name (buffer-file-name (current-buffer)))
         (dir (if file-name (directory-file-name file-name) default-directory))
         (current-dir (f-full (if f f (file-name-directory dir)))))
    (while (and current-dir (not (idee/filesystem-root-p current-dir)) (not (idee/maven-module-root-dir-p current-dir)))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    (if (idee/filesystem-root-p current-dir) nil current-dir)))

(defun idee/maven-enclosing-module-root-dir (&optional f)
  "Find the directory of the parent maven module that owns the source file F."
  (let* ((file-name (buffer-file-name (current-buffer)))
         (dir (if file-name (directory-file-name file-name) default-directory))
         (current-dir (f-full (if f f (file-name-directory dir))))
         (module-dir (idee/maven-module-root-dir current-dir))
         (parent-dir (idee/maven-module-root-dir (file-name-directory  (directory-file-name module-dir)))))
    (while (and parent-dir (not (idee/filesystem-root-p parent-dir)) (not (idee/maven-module-root-dir-p parent-dir)))
      (setq parent-dir (file-name-directory (directory-file-name parent-dir))))
    (if (idee/filesystem-root-p parent-dir) nil parent-dir)))

(defun idee/maven-invoker-test-dir-p (&optional f)
  "Return non-nil if the file is part of a maven-invoker integration test."
  (let* ((file-name (buffer-file-name (current-buffer)))
         (dir (if file-name (directory-file-name file-name) default-directory))
         (current-dir (f-full (if f f (file-name-directory dir)))))
    (string-match-p (regexp-quote (f-join "src" "it")) current-dir)))

(defun idee/maven-pom-group-id (pom)
  "Get the groupId of the specified POM."
  (if (file-exists-p pom)
      (with-temp-buffer
        (insert-file-contents pom)
        (let* ((xml (libxml-parse-xml-region (point-min) (point-max)))
               (p (assoc 'project xml))
               (project (if p p (cdr xml))))
          (car (cdr (cdr (assoc 'groupId project))))))
    nil))


(defun idee/maven-pom-artifact-id (pom)
  "Get the artifactId of the specified POM."
  (if (file-exists-p pom)
      (with-temp-buffer
        (insert-file-contents pom)
        (let* ((xml (libxml-parse-xml-region (point-min) (point-max)))
               (p (assoc 'project xml))
               (project (if p p (cdr xml))))
          (car (cdr (cdr (assoc 'artifactId project))))))
    nil))

(defun idee/maven-pom-version (pom)
  "Get the version of the specified POM."
  (if (file-exists-p pom)
      (with-temp-buffer
        (insert-file-contents pom)
        (let* ((xml (libxml-parse-xml-region (point-min) (point-max)))
               (p (assoc 'project xml))
               (project (if p p (cdr xml)))
               (parent (assoc 'parent project))
               (parent-version (car (cdr (cdr (assoc 'version parent)))))
               (version (car (cdr (cdr (assoc 'version project))))))
          (or version parent-version)))
    nil))

(defun idee/maven-edit-project-pom-xml ()
  "Edit the current project pom."
  (interactive)
  (let* ((project-pom (concat (project-root (project-current)) pom-xml)))
    (if (file-exists-p project-pom)
        (progn (idee/jump-to-non-idee/window)
               (find-file project-pom)))))

(defun idee/maven-clean-project ()
  "Clean the current maven project."
  (interactive)
  (idee/maven-exec :goals "clean"))

(defun idee/maven-package-project ()
  "Package the current maven project."
  (interactive)
  (idee/maven-exec :goals "package"))

(defun idee/maven-install-project ()
  "Install the current maven project."
  (interactive)
  (idee/maven-exec :goals "install"))

(defun idee/maven-debug-project ()
  "Build the current maven project."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 8000
              :project-name artifact-id
              :wait-for-port t)
      (append (list :program-to-start (idee/maven-cmd :goals "install" :debug t)))
      dap-debug)))

(defun idee/maven-surefire-debug-project ()
  "Debug the current maven project."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :project-name artifact-id
              :wait-for-port t)
        (append (list :program-to-start (idee/maven-cmd :goals "install" :surefire-debug t)))
        dap-debug)))

(defun idee/maven-failsafe-debug-project ()
  "Debug the current maven project."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :project-name artifact-id
              :wait-for-port t)
      (append (list :program-to-start (idee/maven-cmd :goals "install" :failsafe-debug t)))
      dap-debug)))

;;
;; Module 
;;

(defun idee/maven-clean-module ()
  "Clean the current maven module."
  (interactive)
  (idee/maven-exec :goals "clean" :build-scope 'module))

(defun idee/maven-edit-module-pom-xml ()
  "Edit the current maven module pom."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
         (module-pom (concat module-dir pom-xml)))
    (if  (file-exists-p module-pom)
        (progn (idee/jump-to-non-idee/window)
               (find-file module-pom)))))

(defun idee/maven-package-module ()
  "Package the current maven module."
  (interactive)
  (idee/maven-exec :goals "package" :build-scope 'module))

(defun idee/maven-install-module ()
  "Install the current maven module."
  (interactive)
  (idee/maven-exec :goals "install" :build-scope 'module))

(defun idee/maven-also-install-module ()
  "Install the current maven module (but also make dependencies in the reactor)."
  (interactive)
  (idee/maven-exec :goals "install" :build-scope 'module :also-make t))

(defun idee/maven-resume-from-module ()
  "Install the current maven module (but also make dependencies in the reactor)."
  (interactive)
  (idee/maven-exec :goals "install" :build-scope 'resume))


(defun idee/maven-exec-module ()
  "Build the current maven module."
  (interactive)
  (idee/maven-exec :goals "install" :build-scope 'module))

(defun idee/maven-debug-module ()
  "Debug the current maven module."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee/maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee/maven-enclosing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee/maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 8000
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee/maven-cmd :goals "install" :debug t :build-scope 'module)))
        dap-debug)))

(defun idee/maven-surefire-debug-module ()
  "Debug the current maven module."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee/maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee/maven-enclosing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee/maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee/maven-cmd :goals "install" :surefire-debug t :build-scope 'module)))
        dap-debug)))

(defun idee/maven-failsafe-debug-module ()
  "Debug the current maven module."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee/maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee/maven-enclosing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee/maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee/maven-cmd :goals "install" :failsafe-debug t :build-scope 'module)))
        dap-debug)))

;;
;; File 
;;

(defun idee/maven-run-file ()
  "Run the current main class using mvn exec:java."
  (interactive)
  (let* ((classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
         (packagename (idee/java-package-of default-directory))
         (fqcn (if packagename (format "%s.%s" packagename classname) classname)))

  (idee/maven-exec :goals (format "package exec:java -Dexec.mainClass=%s" fqcn) :build-scope 'module)))


(defun idee/maven-debug-file ()
  "Debug the current maven file."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee/maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee/maven-enclosing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee/maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id))
                                     (classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
                                     (packagename (idee/java-package-of default-directory))
                                     (fqcn (if packagename (format "%s.%s" packagename classname) classname)))


    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 8000
              :projectName project-name
              :wait-for-port t)
        ;; If we don't escape those arguments it will fail on shell
        (append (list :program-to-start (idee/maven-cmd :goals (format "package exec:exec -DskipTests -Dexec.executable=\"java\" -Dexec.args=\"-classpath\\ %s\\ -agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=8000\\ %s\"" "%classpath" fqcn) :build-scope 'module :goto-project-root t)))
        dap-debug)))

(defun idee/maven-surefire-test-file ()
  "Debug the current maven file."
  (interactive)
  (let* ((classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
    (idee/maven-exec :goals (format "test -Dtest=%s" classname) :build-scope 'module)))


(defun idee/maven-surefire-debug-file ()
  "Debug the current maven file."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee/maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee/maven-enclosing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee/maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id))
                                     (classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee/maven-cmd :goals (format "test -Dtest=%s" classname) :surefire-debug t :build-scope 'module :goto-project-root t)))
        dap-debug)))

(defun idee/maven-surefire-test-file-method ()
  "Test the current maven file method."
  (interactive)
  (let* ((classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
         (method-name (idee/java-method-name-at-point)))
    (if method-name
        (idee/maven-exec :goals (format "test -Dtest=%s#%s" classname method-name) :build-scope 'module)
      (idee/maven-exec :goals (format "test -Dtest=%s" classname) :build-scope 'module))))

(defun idee/maven-surefire-debug-file-method ()
  "Debug the current maven file method."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee/maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee/maven-enclosing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee/maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id))
                                     (classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
                                     (method-name (idee/java-method-name-at-point)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee/maven-cmd :goals (if method-name (format "test -Dtest=%s#%s" classname method-name) (format "test -Dtest=%s" classname)) :surefire-debug t :build-scope 'module :goto-project-root t)))
        dap-debug)))

(defun idee/maven-failsafe-test-file ()
  "Test with failsafe the current file."
  (interactive)
  (let* ((classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
         (method-name (idee/java-method-name-at-point)))
    (idee/maven-exec :goals (if method-name
                                (format "verify -Dit.test=%s#%s" classname method-name)
                              (format "verify -Dit.test=%s" classname))
                     :build-scope 'module)))

(defun idee/maven-failsafe-debug-file ()
  "Debug with failsafe the current file."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee/maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee/maven-enclosing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee/maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id))
                                     (classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))
                                     (method-name (idee/java-method-name-at-point)))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee/maven-cmd :goals
                                                        (if method-name
                                                            (format "verify -Dit.test=%s#%s" classname method-name)
                                                          (format "verify -Dit.test=%s" classname))
                                                        :failsafe-debug t :build-scope 'module :goto-project-root t)))
        dap-debug)))


(defun idee/maven-failsafe-test-file-method ()
  "Test with failsafe the current file."
  (interactive)
  (let* ((classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
    (idee/maven-exec :goals (format "verify -Dit.test=%s" classname) :build-scope 'module)))

(defun idee/maven-failsafe-debug-file-method ()
  "Debug with failsafe the current file."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee/maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-dir (idee/maven-enclosing-module-root-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee/maven-pom-artifact-id enclosuing-module-pom))
                                     (project-name (if invoker-test enclosing-artifact-id artifact-id))
                                     (classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
    (-> (list :type "java"
              :request "attach"
              :hostName "localhost"
              :port 5005
              :projectName project-name
              :wait-for-port t)
        (append (list :program-to-start (idee/maven-cmd :goals (format "verify -Dit.test=%s" classname) :failsafe-debug t :build-scope 'module :goto-project-root t)))
        dap-debug)))


(defun idee/maven-exec-from-history ()
  "Prompt the user to execute previous maven build from history."
  (interactive)
  (let ((maven-command (completing-read "Maven command:" (delete-dups idee/maven-exec-history))))
    (idee/shell-command-execute-in-project maven-command t)))


(defun idee/maven-exec-from-project-settings ()
  "Prompt the user to execute previous maven build from history."
  (interactive)
  (idee/with-project-settings "maven.el" idee/maven-project-settings-commands
                              (let* ((maven-command nil)
                                     (mvn-cmd-builder nil))

                                (push (completing-read "Maven command:" idee/maven-project-settings-commands) mvn-cmd-builder)
                                
                                (if idee/maven-skip-tests
                                    (push "-DskipTests" mvn-cmd-builder))
                                (if idee/maven-offline
                                    (push "-o" mvn-cmd-builder))
                                (if idee/maven-show-errors
                                    (push "-e" mvn-cmd-builder))
                                (setq maven-command (string-trim (string-join (reverse mvn-cmd-builder) " ")))
                                (idee/shell-command-execute-in-project maven-command t))))

(cl-defun idee/maven-cmd (&key goals debug surefire-debug failsafe-debug build-scope also-make goto-project-root)
  (idee/with-project-settings "maven.el" idee/maven-profiles
                              (let* ((module-dir (idee/maven-module-root-dir))
                                     (enclosuing-module-dir (if module-dir (idee/maven-enclosing-module-root-dir) nil))
                                     (module-pom (concat module-dir pom-xml))
                                     (invoker-test (idee/maven-invoker-test-dir-p module-dir))
                                     (enclosuing-module-pom (concat enclosuing-module-dir pom-xml))
                                     (artifact-id (idee/maven-pom-artifact-id module-pom))
                                     (enclosing-artifact-id (idee/maven-pom-artifact-id enclosuing-module-pom))
                                     (profiles-opt (idee/maven-profiles-option))
                                     (mvn-cmd-builder nil))

                                (when goto-project-root (push (format "cd %s && " (idee/project-root-dir)) mvn-cmd-builder))

                                (cond
                                 (invoker-test (push "mvn" mvn-cmd-builder))
                                 (debug (push "mvnDebug" mvn-cmd-builder))
                                 (t (push "mvn" mvn-cmd-builder)))

                                (when (and idee/maven-clean (not (idee/contains-string goals "clean")))
                                  (push "clean" mvn-cmd-builder))
                                
                                (push goals mvn-cmd-builder)

                                (if profiles-opt
                                    (push profiles-opt mvn-cmd-builder))
                                (if artifact-id
                                    (cond
                                     (invoker-test
                                      (progn
                                        (push (format "-pl :%s -Dinvoker.test=%s" enclosing-artifact-id (file-name-nondirectory (directory-file-name module-dir))) mvn-cmd-builder)
                                        (when debug (push "-Dinvoker.mavenExecutable=mvnDebug" mvn-cmd-builder))))
                                      ((equal 'resume build-scope) (push (format "-rf :%s" artifact-id) mvn-cmd-builder))
                                      ((equal 'module build-scope) (push (format "-pl :%s" artifact-id) mvn-cmd-builder))))
                                (if idee/maven-skip-tests
                                    (push "-DskipTests" mvn-cmd-builder))
                                (if idee/maven-offline
                                    (push "-o" mvn-cmd-builder))
                                (if idee/maven-show-errors
                                    (push "-e" mvn-cmd-builder))
                                (if also-make
                                    (push "-am" mvn-cmd-builder))
                                (if surefire-debug
                                    (if invoker-test 
                                        (push "-Dinvoker.mavenOpts=\"-Dmaven.surefire.debug\"" mvn-cmd-builder)
                                      (push "-Dmaven.surefire.debug" mvn-cmd-builder)))
                                (if failsafe-debug
                                    (push "-Dmaven.failsafe.debug" mvn-cmd-builder))
                                (string-trim (string-join (reverse mvn-cmd-builder) " ")))))


(cl-defun idee/maven-exec (&key goals debug surefire-debug failsafe-debug build-scope also-make)
  "Build the current maven module."
  (interactive)
  (let ((cmd (idee/maven-cmd :goals goals :debug debug :surefire-debug surefire-debug :failsafe-debug failsafe-debug :build-scope build-scope :also-make also-make)))
    (when (not (member cmd idee/maven-exec-history))
      (push cmd idee/maven-exec-history))
    (idee/shell-command-execute-in-project cmd t)))

;;; Toggles
(defun idee/maven-toggle-clean ()
  "Toggle clean flag for maven builds."
  (interactive)
  (if (idee/toggle idee/maven-clean)
      (message "Maven clean: Enabled!")
    (message "Maven clean: Disabled!")))

(defun idee/maven-toggle-offline ()
  "Toggle offline flag for maven builds."
  (interactive)
  (if (idee/toggle idee/maven-offline)
      (message "Maven offline: Enabled!")
    (message "Maven offline: Disabled!")))

(defun idee/maven-toggle-skip-tests ()
  "Toggle offline flag for maven builds."
  (interactive)
  (if (idee/toggle idee/maven-skip-tests)
      (message "Maven test skip: Enabled!")
    (message "Maven test skip: Disabled!")))

(defun idee/maven-toggle-show-errors ()
  "Toggle offline flag for maven builds."
  (interactive)
  (if (idee/toggle idee/maven-show-errors)
      (message "Maven show errors Enabled!")
    (message "Maven show errors Disabled!")))

;;
;; Coordinates
;;

(defun idee/maven-local-group-ids  ()
  "Retrieve all group ids found in the local maven repository."
  (interactive)
  (if idee/maven-known-group-ids idee/maven-known-group-ids
    (let* ((repo-path "~/.m2/repository/")
           (metadata-list
            (directory-files-recursively repo-path (regexp-quote "maven-metadata-local.xml"))))
                                   (directory-files-recursively repo-path idee/dot-pom-regex)
      (delq nil (delete-dups (mapcar 'idee/maven-pom-group-id metadata-list))))))

;;;###autoload
(defun idee/maven-add-known-group-ids (group-ids)
  "Add GROUP-IDS to the list of know group ids."
  (let (value)
    (dolist (id group-ids value)
            (idee/maven-add-known-group-id id))))

;;;###autoload
(defun idee/maven-add-known-group-id (group-id)
  "Add GROUP-ID to the list of know group ids."
  (add-to-list 'idee/maven-known-group-ids group-id))

;;;###autoload
(defun idee/maven-remove-known-group-id (group-id)
  "Remove GROUP-ID to the list of know group ids."
  (setq idee/maven-known-group-ids (remove group-id idee/maven-knwon-group-ids)))

(defun idee/maven-local-artifact-ids  (group-id)
  "Retrieve all artifact ids found in the local maven repository, under GROUP-ID."
  (interactive)
  (let* ((group-path (format "~/.m2/repository/%s" (replace-regexp-in-string (regexp-quote ".") "/" group-id))))
    (cdr (cdr (mapcar 'file-name-nondirectory (seq-filter 'file-directory-p (directory-files group-path t)))))))

(defun idee/maven-local-versions  (group-id artifact-id)
  "Retrieve all versions found in the local maven repository, under GROUP-ID and ARTIFACT-ID."
  (interactive)
  (let* ((artifact-path (format "~/.m2/repository/%s/%s" (replace-regexp-in-string (regexp-quote ".") "/" group-id) artifact-id)))
    (cdr (cdr (mapcar 'file-name-nondirectory (seq-filter 'file-directory-p (directory-files artifact-path t)))))))

;;; Utilities
(defun idee/maven-profiles-option ()
  "Create the profiles option to be appended to any maven command.
Returns something like: -Pprofile1,profile2 if profiles are enabled, 
or empty string other wise."
  (if idee/maven-profiles
      (concat "-P" (string-join idee/maven-profiles ",")) ""))

(defun idee/maven-select-profiles ()
  (interactive)
  "Select profiles"
  (idee/project-settings-set "maven.el" "idee/maven-profiles" (idee/as-code (completing-read-multiple "Maven profiles:" (idee/maven--all-profiles)))))

(defun idee/maven--all-profiles ()
  "List all available profiles"
  (split-string (shell-command-to-string (format "cd %s && mvn help:all-profiles | grep \"Profile Id:\" | cut -d\" \" -f5 | sort | uniq" (idee/project-root-dir))) "\n" ))

(defun idee/maven-version-set ()
  (interactive)
  "Set the version to project."
  (let* ((module-dir (idee/maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (version (idee/maven-pom-version module-pom))
                                     (version-parts (if version (split-string version "\\.") '("1" "0" "0")))
                                     (major (string-to-number (replace-regexp-in-string "[^0-9]*" "" (or (nth 0 version-parts) "1"))))
                                     (minor (string-to-number (replace-regexp-in-string "[^0-9]*" "" (or (nth 1 version-parts) "0"))))
                                     (micro (string-to-number (replace-regexp-in-string "[^0-9]*" "" (or (nth 2 version-parts) "0"))))
                                     (candidates `(,(format "%s.%s-SNAPSHOT" major minor)
                                                   ,(format "%s.%s-SNAPSHOT" major (+ 1 minor))
                                                   ,(format "%s.%s-SNAPSHOT" (+ major 1) 0)
                                                   ,(format "%s.%s.%s" major minor (+ micro 1))
                                                   ,(format "%s.%s.%s" major (+ minor 1) 0)
                                                   ,(format "%s.%s.%s" (+ major 1) 0 0)))
                                     (new-version (completing-read "Version:" candidates nil nil))
                                     (goals (format "versions:set -DnewVersion=%s" new-version)))
    (idee/maven-exec :goals goals)))

;; Hydra helpers
(defun idee/maven--selected-profiles ()
  "List all selected profiles"
  (idee/with-project-settings "maven.el" idee/maven-profiles
      (mapcar 'intern idee/maven-profiles)))

(defun idee/maven--project-name ()
    (intern (or (idee/project-name-get) "unknown")))

(defun idee/maven--module-name ()
  (let* ((module-dir (idee/maven-module-root-dir))
         (module-pom (concat module-dir pom-xml))
         (artifact-id (idee/maven-pom-artifact-id module-pom)))
    (intern (or artifact-id "unknown"))))

(defun idee/maven--file-name ()
  (let* ((file-name (if (buffer-file-name) (file-name-nondirectory (buffer-file-name)) nil)))
    (if file-name file-name "<none>")))

;;
;; Maven Hydra
;;

;;;###autoload (autoload 'idee/maven-hydra/body "idee-maven")
(defhydra idee/maven-hydra (:hint none :exit t)
  "
        Project^              ^Module^                 ^File^                             ^Execute^                    ^Toggle^ 
        ?P? 
    --------------------------------------------------------------------------------------------------------------------------------------   
    _pc_: clean            _mc_: clean              _fr_: run                          _h_: from history            _tc_: ?tc? clean
    _pp_: package          _mp_: package          _fstc_: surefire test                _s_: from project settings   _tt_: ?tt? skip tests
    _pi_: install          _mi_: install          _fftc_: failsafe test                _v_: version set             _te_: ?te? errors 
    _po_: edit pom         _mo_: edit pom         _fstm_: surefire test method                                    ^^_to_: ?to? offline
                        ^^_mrf_: resume from      _fftm_: failsafe test method                                    ^^_tp_: profiles %(idee/maven--selected-profiles)   
                        ^^_mai_: also install
   _pd_: debug             _md_: debug              _fd_: debug file
  _psd_: surfire debug    _msd_: surefire debug   _fsdc_: debug surefire test class
  _pfd_: failsafe debug   _mfd_: failsafe debug   _ffdc_: debug failsafe test class
                                              ^^^^_fsdm_: debug surefire test method
                                              ^^^^_ffdm_: debug failsafe test method
  [_q_]: quit
       "
  ("P" nil (format "%-20S %-22S %s" (idee/maven--project-name) (idee/maven--module-name) (idee/maven--file-name)))
  ("po" idee/maven-edit-project-pom-xml)
  ("po" idee/maven-edit-project-pom-xml)
  ("pc" idee/maven-clean-project)
  ("pp" idee/maven-package-project)
  ("pi" idee/maven-install-project)
  ("pd" idee/maven-debug-project)
  ("psd" idee/maven-surefire-debug-project)
  ("pfd" idee/maven-failsafe-debug-project)

  ("mo" idee/maven-edit-module-pom-xml)
  ("mc" idee/maven-clean-module)
  ("mp" idee/maven-package-module)
  ("mi" idee/maven-install-module)
  ("mrf" idee/maven-resume-from-module)
  ("mai" idee/maven-also-install-module)
  ("md" idee/maven-debug-module)
  ("msd" idee/maven-surefire-debug-module)
  ("mfd" idee/maven-failsafe-debug-module)

  ("fr" idee/maven-run-file)
  ("fstc" idee/maven-surefire-test-file)
  ("fftc" idee/maven-failsafe-test-file)

  ("fstm" idee/maven-surefire-test-file-method)
  ("fftm" idee/maven-failsafe-test-file-method)
 
  ("fd" idee/maven-debug-file)
  ("fsdc" idee/maven-surefire-debug-file)
  ("ffdc" idee/maven-failsafe-debug-file)
  ("fsdm" idee/maven-surefire-debug-file-method)
  ("ffdm" idee/maven-failsafe-debug-file-method)

  ("tc" idee/maven-toggle-clean (if idee/maven-clean "[*]" "[ ]") :exit nil)
  ("to" idee/maven-toggle-offline (if idee/maven-offline "[*]" "[ ]") :exit nil)
  ("tt" idee/maven-toggle-skip-tests (if idee/maven-skip-tests "[*]" "[ ]") :exit nil)
  ("te" idee/maven-toggle-show-errors (if idee/maven-show-errors "[*]" "[ ]") :exit nil)
  ("tp" idee/maven-select-profiles :exit nil)

  ("h" idee/maven-exec-from-history)
  ("s" idee/maven-exec-from-project-settings)
  ("v" idee/maven-version-set)
  ("q" nil "quit"))

;;; Project Factory
(defun idee/new-maven-from-archetype-project (&optional create-function)
  "Create a new maven from archetype project.
The command supports accepting an external CREATE-FUNCTION or defaults to idee/project-create-with-shell."
  (interactive)
  (let* ((group-id (read-string "Group Id:" idee/maven-init-group-id))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:" "0.1-SNAPSHOT"))
         (target-dir (idee/project-dir-select))
         (generate-command (format "mvn archetype:generate -DgroupId=%s -DartifactId=%s -Dversion=%s -DarchetypeArtifactId=maven-archetype-quickstart -DinteractiveMode=false" group-id artifact-id version))
         (cleanup-command (format "mv %s/* . && rm -r %s" artifact-id artifact-id)))
    (funcall (or create-function 'idee/project-create-with-shell) target-dir generate-command cleanup-command)
    (idee/project-name-set artifact-id)
    (idee/project-version-set version)))

;;; Project Visitor
(defun idee/maven-project-p (root)
  "Check if ROOT is the root path of a java project."
  (seq-filter (lambda (x)
                (or (equal pom-xml x)))
              (directory-files root)))

;;;###autoload
(defun idee/maven-visitor (root)
  "Check if a java project is available under the specified ROOT."
  (let ((project-pom (concat root pom-xml)))
    (when (file-exists-p project-pom)
      (idee/project-version-set (idee/maven-pom-version project-pom))
      (idee/project-name-set (idee/maven-pom-artifact-id project-pom)))))

(provide 'idee-maven)
;;; idee-maven.el ends here
