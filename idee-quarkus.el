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

;;; Commentary:

;;; Code:
(require 'idee-maven)
(require 'idee-projects)
(require 'idee-eshell)
(require 'idee-visitors)

(defcustom idee-quarkus-version "1.0.1.Final" "The quarkus version." :group 'idee-quarkus :type 'string)
(defcustom idee-quarkus-remote-dev-url nil "The remote dev url." :group 'idee-quarkus :type 'string)
(defcustom idee-quarkus-init-group-id "org.acme" "The initial value for group-id in the quarkus project factory." :group 'idee-quarkus :type 'string)

(defconst idee-quarkus-maven-plugin "quarkus-maven-plugin")

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
                                         "kubernetes-client"
                                         "narayana-jta"
                                         "netty"
                                         "reactive-pg-client"
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
                                         "rest-client"
                                         "spring-di"
                                         "spring-web"
                                         "spring-data-jpa"
                                         "undertow"
                                         "undertow-websockets"
                                         "vertx"))

(defconst idee-quarkus-rest-project-factory
  (make-ide-project-factory
   :name "Quarkus"
   :description "New Quarkus project created using the quarkus maven plugin."
   :func 'idee-new-quarkus-rest-project))

(defun idee-new-quarkus-rest-project (&optional create-function)
  "Create a new quarkus rest project.
The command supports accepting an external CREATE-FUNCTION or defaults to ide-project-create-with-shell."
  (interactive)
  (let* ((group-id (read-string "Group Id:" idee-quarkus-init-group-id))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:" "0.1-SNAPSHOT"))
         (endpoint (read-string "Endpoint:" "/hello"))
         (extensions (completing-read-multiple "Select extensions: " idee-quarkus-extensions-list))
         (extension-names (mapcar 'idee-quarkus-extension-qualified-name extensions))
         (target-dir (ide-project-dir-select))
         (generate-command (format "mvn io.quarkus:quarkus-maven-plugin:%s:create -DprojectGroupId=%s -DprojectArtifactId=%s -DprojectVersion=%s -DclassName=%s.Endpoint -Dendpoint=%s" idee-quarkus-version group-id artifact-id version group-id endpoint))
         (cleanup-command (format "mv %s/* . && mv %s/.[^.]* . && rm -r %s" artifact-id artifact-id artifact-id))
         (add-extension-command (if (not extension-names) nil
                                    (format "mvn quarkus:add-extension -Dextensions=\"%s\"" (mapconcat 'identity extension-names ",")))))

    (if add-extension-command
        (funcall (or create-function 'ide-project-create-with-shell) target-dir generate-command cleanup-command add-extension-command)
      (funcall (or create-function 'ide-project-create-with-shell) target-dir generate-command cleanup-command))

    (ide-project-name-set artifact-id)
    (ide-project-version-set version)
    (idee-quarkus-init-maven-project-settings)))

(defun idee-new-quarkus-remote-dev-project (&optional create-function)
  "Create a new quarkus rest project.
The command supports accepting an external CREATE-FUNCTION or defaults to ide-project-create-with-shell."
  (interactive)
  (let* ((group-id (read-string "Group Id:" idee-quarkus-init-group-id))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:" "0.1-SNAPSHOT"))
         (endpoint (read-string "Endpoint:" "/hello"))
         (target-dir (ide-project-dir-select))
         (generate-command (format "mvn io.quarkus:quarkus-maven-plugin:%s:create -DprojectGroupId=%s -DprojectArtifactId=%s -DprojectVersion=%s -DclassName=%s.Endpoint -Dendpoint=%s" idee-quarkus-version group-id artifact-id version group-id endpoint))
         (cleanup-command (format "mv %s/* . && mv %s/.* . && rm -r %s" artifact-id artifact-id artifact-id))
         (tune-application-properties-command "echo quarkus.live-reload.password=secret >> src/main/resources/application.properties")
         (add-extension-command "mvn quarkus:add-extension -Dextensions=\"kubernetes,smallrye-fault-tolerance,smallrye-rest-client,undertow-websockets\""))
         (funcall (or create-function 'ide-project-create-with-shell) target-dir generate-command cleanup-command add-extension-command tune-application-properties-command)
      (idee-quarkus-init-maven-project-settings)))


(defun idee-quarkus-add-extension ()
  "Add a quarkus extension to the project."
  (interactive)
  (let ((extension (completing-read "Extension:" idee-quarkus-extensions-list)))
    (idee-eshell-project-command-enqueue (format "mvn quarkus:add-extension -Dextensions=\"io.quarkus:quarkus-%s\"" extension))))

(defun idee-quarkus-init-maven-project-settings ()
  "Initialize project with project settings."
  (interactive)
  (let* ((settings-dir (f-join default-directory ".idee"))
         (maven-settings-file (f-join settings-dir "maven.el")))
    (if (not (file-exists-p settings-dir))
        (mkdir settings-dir))
    (if (not (file-exists-p maven-settings-file))
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
          (write-file maven-settings-file)))))

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
;; Shortcuts
;;
(defun idee-quarkus-build ()
  "Run the quarkus build."
  (interactive)
  (idee-eshell-project-command-enqueue "mvn clean install"))

(defun idee-quarkus-dev ()
  "Run the quarkus dev mode."
  (interactive)
  (idee-eshell-project-command-enqueue "mvn clean compile quarkus:dev")
  (rename-buffer "**quarkus:dev**"))

(defun idee-quarkus-remote-dev ()
  "Run the quarkus remote dev mode."
  (interactive)
  (when idee-quarkus-remote-dev-url (idee-eshell-project-command-enqueue (format "mvn quarkus:remote-dev -Dquarkus.live-reload.url=%s" idee-quarkus-remote-dev-url))))

(defun idee-quarkus-native-build ()
  "Run the quarkus native build."
  (interactive)
  (idee-eshell-project-command-enqueue "mvn clean package -Dnative=true -Dnative-image.docker-build=true"))

;;
;; Utils
;;

(defun idee-quarkus-extension-qualified-name (extension)
  "Return the qualified name of the EXTENSION."
  (format "io.quarkus:quarkus-%s" extension))

;;; Project Visitor
(defun idee-quarkus-project-p (root)
  "Check if ROOT is the root path of a quarkus project."
  (if (idee-maven-project-p root)
    (let* ((pom (f-join root pom-xml))
           (content (idee-read-file pom)))
      (string-match-p (regexp-quote idee-quarkus-maven-plugin) content))
    nil))

(defun ide-visitor-quarkus (root)
  "Check if a java project is available under the specified ROOT."
  (let ((project-pom (concat root pom-xml)))
    (when (idee-quarkus-project-p root)
      (ide-project-version-set (idee-maven-pom-version project-pom))
      (ide-project-name-set (idee-maven-pom-artifact-id project-pom))
      (idee-quarkus-init-maven-project-settings))))

;;;###autoload
(defun idee--quarkus-init ()
  (ide-visitor-register 'ide-visitor-quarkus) 
  (ide-project-factory-register idee-quarkus-rest-project-factory))

(provide 'idee-quarkus)
;;; idee-quarkus.el ends here
