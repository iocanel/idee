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

(defcustom idee-quarkus-version "0.15.0" "The quarkus version." :group 'idee :type 'string)

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

(defun idee-new-quarkus-rest-project ()
  "Create a new quarkus rest project."
  (interactive)
  (let* ((group-id (read-string "Group Id:"))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:" "0.1-SNAPSHOT"))
         (endpoint (read-string "Endpoint:" "/hello"))
         (extensions (completing-read-multiple "Select extensions: " idee-quarkus-extensions-list))
         (target-dir (idee--select-new-project-dir))
         (generate-command (format "mvn io.quarkus:quarkus-maven-plugin:%s:create -DprojectGroupId=%s -DprojectArtifactId=%s -DprojectVersion=%s -DclassName=%s.Endpoint -Dendpoint=%s" idee-quarkus-version group-id artifact-id version group-id endpoint))
         (cleanup-command (format "mv %s/* . && rm -r %s" artifact-id artifact-id))
         (add-extension-command (format "mvn quarkus:add-extension -Dextensions=\"io.quarkus:quarkus-%s\"" (mapconcat 'identity (mapc (lambda (e) (format "io.quarkus:quarkus-%s" e)) extensions) ","))))

    (idee-create-project-with-shell target-dir generate-command cleanup-command add-extension-command)
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
      (insert (format "(setq idee-maven-project-settings-commands '(\"%s\"))" "mvn quarkus:dev"))
      (write-file maven-settings-file))))


(add-to-list 'idee-project-factories-list idee-quarkus-rest-project-factory)

(provide 'idee-quarkus)
;;; idee-quarkus.el ends here
