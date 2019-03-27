;;; idee-quarkus.el --- Quarkus Project Factory

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

(defcustom idee-quarkus-version "0.12.0" "The quarkus version." :group 'idee :type 'string)

(defun idee-new-quarkus-rest-project ()
  "Create a new quarkus rest project."
  (interactive)
  (let* ((group-id (read-string "Group Id:"))
         (artifact-id (read-string "Artifact Id:"))
         (version (read-string "Version:" "0.1-SNAPSHOT"))
         (endpoint (read-string "Endpoint:" "/hello"))
         (recomended-dir (concat (file-name-as-directory default-directory) artifact-id))
         (temp-dir (concat temporary-file-directory "quarkus-" (format "%06x-%06x" (random (expt 16 6)) (random (expt 16 6)))))
         (generated-dir (concat (file-name-as-directory temp-dir) artifact-id))
         (target-dir (idee--select-new-project-dir))
         (parent-dir (file-name-directory (directory-file-name target-dir)))
         (dir-name (substring target-dir (length parent-dir)))
         (generate-command (format "mvn io.quarkus:quarkus-maven-plugin:%s:create -DprojectGroupId=%s -DprojectArtifactId=%s -DprojectVersion=%s -DclassName=%s.Endpoint -Dendpoint=%s" idee-quarkus-version group-id artifact-id version group-id
                           endpoint))
         )

    (make-directory temp-dir t)
    (setq default-directory temp-dir)
    (shell-command generate-command)
    (shell-command (format "mv %s/* %s" temp-dir target-dir))
    (write-region "" nil (concat (file-name-as-directory target-dir) ".projectile"))
    (projectile-add-known-project target-dir)
    (setq projectile-project-root target-dir)
    (projectile-switch-project-by-name target-dir)
    (revert-buffer)
    (dired target-dir)
    (idee-ide-view)
    )
  )

(defconst idee-quarkus-rest-project-factory
  (make-idee-project-factory
   :name "Quarkus"
   :description "New Quarkus project created using the quarkus maven plugin."
   :func 'idee-new-quarkus-rest-project))

(add-to-list 'idee-project-factories-list idee-quarkus-rest-project-factory)

(provide 'idee-quarkus)
;;; idee-quarkus.el ends here.
