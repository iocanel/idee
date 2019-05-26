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
(require 'dockerfile-mode)
(require 'idee-projects)

(defvar idee-dockerfile-provider-list nil)
(defvar idee-docker-last-edited-dockerfile nil)
(defvar idee-docker-registry nil)

(defconst idee-last-visited-dockerfile "last-visited-dockerfile" "The property key that marks the last visited Dockerfile.")

(defun idee-docker-get-dockerfiles-from-providers()
  "Call all registered dockerfile providers."
  (interactive)
  (seq-filter 'file-exists-p (mapcar (lambda (p) (funcall p)) idee-dockerfile-provider-list)))

(defun idee-docker-build ()
  "Perform a docker build."
  (interactive)
  (let ((dockerfile (idee-docker-find-dockerfile))
        (dockerfile-relative-path (file-relative-name dockerfile (projectile-project-root)))
        (docker-image (idee-docker-get-image-name)))
    (idee-eshell-project-command-enqueue (format "docker build -f %s -t %s ." dockerfile-relative-path docker-image))))

(defun idee-docker-run-dockerfile ()
  "Perform a docker run using a know dockerfile."
  (interactive)
  (let* ((dockerfile (idee-docker-find-dockerfile))
        (docker-image (idee-docker-get-image-name))
        (ports (idee-docker-get-exposed-ports dockerfile))
        (cmd-builder nil))

    (add-to-list 'cmd-builder "docker run -it" t)
    (if ports
        (progn
          (add-to-list 'cmd-builder "-p" t)
          (dolist (p ports)
                  (add-to-list 'cmd-builder (format "%s:%s" p p) t))))
    (add-to-list 'cmd-builder docker-image t)
    (idee-eshell-project-command-enqueue (string-trim (string-join cmd-builder " ")))))

(defun idee-docker-get-image-name ()
  "Get the docker image name from settings or project parameters."
  (idee-with-project-settings "docker.el" idee-docker-image
                              (let  ((org (user-login-name))
                                     (name (idee-project-get-name))
                                     (version (idee-project-get-version))
                                     (docker-image idee-docker-image))

                                ;;
                                ;; When using project factories, it's possible that the visitors
                                ;; where called before the pom.xml, package.json etc was created.
                                ;; So, try to call them again or fallback to the latest
                                ;;
                                (when (not version)
                                  (idee-project-visit)
                                  (setq version (or (idee-project-get-version) "latest")))

                                (or  docker-image
                                     (if idee-docker-registry
                                         (format "%s/%s/%s:%s" idee-docker-registry org name version)
                                       (format "%s/%s:%s" org name version))))))

(defun idee-docker-get-exposed-ports(dockerfile)
  "Get the port numbers that are exposed in the DOCKERFILE as list."
  (let ((abs-dockerfile
         (if (file-name-absolute-p dockerfile) dockerfile (f-join (projectile-project-root) dockerfile))))
    (with-temp-buffer
      (insert-file abs-dockerfile)
      (let* ((begin (point-min))
             (end (point-max))
             (content (buffer-substring begin end))
             (match-list (idee-string-match-as-list "EXPOSE \\([0-9 ]+\\)" content))
             (ports-string (if match-list (car (cdr match-list)) nil))
             (port-list (if (stringp ports-string) (split-string ports-string) nil)))
        port-list))))

(defun idee-docker-find-dockerfile()
  "Find the dockerfile.
The criteria are the following:

1. If current file is a Dockerfile return that.
2. Call all dockerfile providers and return the first existing Dockerfile
3. Check the last edited Dockerfile, or return nil."
  (let* ((buffer (buffer-file-name))
         (path (if buffer (buffer-file-name) path nil))
         (provider-dockerfile (car (idee-docker-get-dockerfiles-from-providers)))
         (last-visited-dockerfile (idee-project-get-property idee-last-visited-dockerfile))
         (dockerfile))

    (setq dockerfile (cond
     ((equal "dockerfile-mode" major-mode) path)
     (provider-dockerfile provider-dockerfile)
     ((file-exists-p last-visited-dockerfile) last-visited-dockerfile)
     (t nil)))
    dockerfile))
    

(defun idee-docker-dockerfile-from-project-root ()
  "Return the path of Dockerfile at the project root."
  (f-join (projectile-project-root) "Dockerfile"))         

(add-to-list 'idee-dockerfile-provider-list 'idee-docker-dockerfile-from-project-root)
(add-hook 'dockerfile-mode-hook (lambda () (idee-project-set-property idee-last-visited-dockerfile (buffer-file-name))))

(define-key dockerfile-mode-map (kbd" C-c C-b") 'idee-docker-build)
(define-key dockerfile-mode-map (kbd" C-c C-r") 'idee-docker-run-dockerfile)

(provide 'idee-docker)
;;; idee-docker.el ends here
