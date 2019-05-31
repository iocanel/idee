;;; idee-quarkus.el --- Quarkus Project Factory -*- lexical-binding: t -*-

;; Copyright (C) 2019 Ioannis Canellos 
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

;;; Code:

(require 'idee-utils)
(require 'idee-projects)
(require 'idee-eshell)

(defcustom idee-quarkus-version "0.15.0" "The quarkus version." :group 'idee-quarkus :type 'string)
(defcustom idee-quarkus-remote-dev-url nil "The remote dev url." :group 'idee-quarkus :type 'string)
(defcustom idee-quarkus-init-group-id "org.acme" "The initial value for group-id in the quarkus project factory." :group 'idee-quarkus :type 'string)

(defconst idee-quarkus-extensions-list '("agroal"
                                         "amazon-lambda"
                                         "arc"
                                         "camel-core"
                                         "camel-infinispan"
                                         "camel-netty4-http"
                                         "camel-salesforce"
                                         "elytron-security"
                                         "flyway"
                                         "hibernate-orm"
                                         "hibernate-orm-panache"
                                         "hibernate-validator"
                                         "infinispan-client"
                                         "jaxb"
                                         "jdbc-h2"
                                         "jdbc-mariadb"
                                         "jdbc-mssql"
                                         "jdbc-postgresql"
                                         "jsonb"
                                         "jsonp"
                                         "keycloak"
                                         "kotlin"
                                         "kubernetes"
                                         "narayana-jta"
                                         "netty"
                                         "resteasy"
                                         "resteasy-jsonb"
                                         "scheduler"
                                         "smallrye-fault-tolerance"
                                         "smallrye-health"
                                         "smallrye-jwt"
                                         "smallrye-metrics"
                                         "smallrye-openapi"
                                         "smallrye-opentracing"
                                         "smallrye-reactive-messaging"
                                         "smallrye-reactive-messaging-kafka"
                                         "smallrye-reactive-streams-operators"
                                         "smallrye-rest-client"
                                         "spring-di"
                                         "undertow"
                                         "undertow-websockets"
                                         "vertx"))

(defconst idee-quarkus-rest-project-factory
  (make-idee-project-factory
   :name "Quarkus"
   :description "New Quarkus project created using the quarkus maven plugin."
   :func 'idee-new-quarkus-rest-project))

(defun idee-new-quarkus-rest-project (&optional create-function)
  "Create a new quarkus rest project.
The command supports accepting an external CREATE-FUNCTION or defaults to idee-create-project-with-shell."
  (interactive)
  (let* ((group-id (read-string "Group Id:" idee-quarkus-init-group-id))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:" "0.1-SNAPSHOT"))
         (endpoint (read-string "Endpoint:" "/hello"))
         (extensions (completing-read-multiple "Select extensions: " idee-quarkus-extensions-list))
         (target-dir (idee--select-new-project-dir))
         (generate-command (format "mvn io.quarkus:quarkus-maven-plugin:%s:create -DprojectGroupId=%s -DprojectArtifactId=%s -DprojectVersion=%s -DclassName=%s.Endpoint -Dendpoint=%s" idee-quarkus-version group-id artifact-id version group-id endpoint))
         (cleanup-command (format "mv %s/* . && mv %s/.[^.]* . && rm -r %s" artifact-id artifact-id artifact-id))
         (add-extension-command (if (not extensions) nil
                                    (format "mvn quarkus:add-extension -Dextensions=\"io.quarkus:quarkus-%s\"" (mapconcat 'identity (mapc (lambda (e) (format "io.quarkus:quarkus-%s" e)) extensions) ",")))))

    (if (not add-extension-command)
        (funcall (or create-function 'idee-create-project-with-shell) target-dir generate-command cleanup-command add-extension-command)
      (funcall (or create-function 'idee-create-project-with-shell) target-dir generate-command cleanup-command))

      (idee-quarkus-init-maven-project-settings)))

(defun idee-new-quarkus-remote-dev-project (&optional create-function)
  "Create a new quarkus rest project.
The command supports accepting an external CREATE-FUNCTION or defaults to idee-create-project-with-shell."
  (interactive)
  (let* ((group-id (read-string "Group Id:" idee-quarkus-init-group-id))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:" "0.1-SNAPSHOT"))
         (endpoint (read-string "Endpoint:" "/hello"))
         (target-dir (idee--select-new-project-dir))
         (generate-command (format "mvn io.quarkus:quarkus-maven-plugin:%s:create -DprojectGroupId=%s -DprojectArtifactId=%s -DprojectVersion=%s -DclassName=%s.Endpoint -Dendpoint=%s" idee-quarkus-version group-id artifact-id version group-id endpoint))
         (cleanup-command (format "mv %s/* . && mv %s/.* . && rm -r %s" artifact-id artifact-id artifact-id))
         (tune-application-properties-command "echo quarkus.live-reload.password=secret >> src/main/resources/application.properties")
         (add-extension-command "mvn quarkus:add-extension -Dextensions=\"kubernetes,smallrye-fault-tolerance,smallrye-rest-client,undertow-websockets\""))
         (funcall (or create-function 'idee-create-project-with-shell) target-dir generate-command cleanup-command add-extension-command tune-application-properties-command)
      (idee-quarkus-init-maven-project-settings)))


(defun idee-quarkus-add-extension ()
  "Add a quarkus extension to the project."
  (interactive)
  (let ((extension (completing-read "Extension:" idee-quarkus-extensions-list)))
    (idee-eshell-project-command-enqueue (format "mvn quarkus:add-extension -Dextensions=\"%s\"" extension))))

(defun idee-quarkus-init-maven-project-settings ()
  "Initialize project with project settings."
  (interactive)
  (let* ((settings-dir (f-join default-directory ".idee"))
         (maven-settings-file (f-join settings-dir "maven.el")))
    (if (not (file-exists-p settings-dir))
        (mkdir settings-dir))
    (with-temp-buffer
      ;; Add to settings popular commands for quarkus
      (insert (format "(setq idee-maven-project-settings-commands nil)"))
      ;; Native image build
      (insert (format "(push \"%s\" idee-maven-project-settings-commands)" "mvn clean package -Dnative=true -Dnative-image.docker-build=true"))
      ;; Dev mode
      (insert (format "(push \"%s\" idee-maven-project-settings-commands)" "mvn quarkus:dev"))
      ;; Remove Dev mode
      (when idee-quarkus-remote-dev-url
        (insert (format "(push \"%s\" idee-maven-project-settings-commands)" (format "mvn quarkus:remote-dev -Dquarkus.live-reload.url=%s" idee-quarkus-remote-dev-url))))
      (write-file maven-settings-file))))

(defun idee-quarkus-aws-lambda-deploy ()
  "Deploy the application to aws lambda."
  (interactive)
  (let* ((module-dir (idee-maven-module-root-dir))
                                     (module-pom (concat module-dir pom-xml))
                                     (artifact-id (idee-maven-pom-artifact-id module-pom)))
    (idee-eshell-project-command-enqueue `("mkdir -p target/function"
                                         "cp target/wiring-classes/bootstrap target/*-runner target/function"
                                         "chmod 755 target/function/bootstrap"
                                         "pushd target/function"
                                         "zip -q function.zip bootstrap *-runner*"
                                         "popd"
                                         ,(format "aws lambda delete-function --function-name %s" artifact-id)
                                         ,(format "aws lambda create-function --function-name %s --timeout 10 --zip-file fileb://target/function/function.zip --handler bootstrap --runtime provided" artifact-id)))))

;;
;; Output filters
;;
(defun idee-quarkus-highlight-time ()
  "Highlight time."
  (idee-quarkus-highlight-time-on-region eshell-last-output-start
                                         eshell-last-output-end))

(defconst idee-time-regexp "[0-9]+([\.][0-9]+)?m?s")

(defun idee-quarkus-highlight-time-on-region (start end)
  "Highlight time in region between START and END."
  (let ((start-marker (copy-marker start))
        (end-marker (copy-marker end)))
    (save-excursion
      (goto-char start-marker)
      (while (re-search-forward idee-time-regexp end-marker t))
        (message (buffer-substring (match-beginning 0) (match-end 0))))))

; TODO: Implement filter correctly
;(add-to-list 'eshell-output-filter-functions 'idee-quarkus-highlight-time)

(add-to-list 'idee-project-factories-list idee-quarkus-rest-project-factory)

(provide 'idee-quarkus)
;;; idee-quarkus.el ends here
