;;; idee-docker.el --- Docker integration -*- lexical-binding: t -*-

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

(defvar idee-dockerfile-provider-list nil)
(defvar idee-docker-last-edited-dockerfile nil)
(defvar idee-docker-registry nil)

(defun idee-docker-get-dockerfiles-from-providers()
  "Call all registered dockerfile providers."
  (interactive)
  (seq-filter 'file-exists-p (mapcar (lambda (p) (funcall p)) idee-dockerfile-provider-list)))

(defun idee-docker-build ()
  "Perform a docker build."
  (interactive)
  (let ((dockerfile (idee-docker-find-dockerfile))
        (docker-image (idee-docker-get-image-name)))
    (idee-eshell-project-command-enqueue (format "docker build -f %s -t %s ." dockerfile docker-image))))

(defun idee-docker-get-image-name ()
  "Get the docker image name from settings or project parameters."
  (idee-with-project-settings "docker.el" idee-docker-image
                              (let  ((docker-image idee-docker-image))
                                (or  docker-image
                                     (if idee-docker-registry
                                         (format "%s/%s/%s:%s" idee-docker-registry (user-login-name) idee-project-name idee-project-version)
                                       (format "%s/%s:%s" (user-login-name) idee-project-name idee-project-version))))))

(defun idee-docker-find-dockerfile()
  "Find the dockerfile.
The criteria are the following:

1. If current file is a Dockerfile return that.
2. Call all dockerfile providers and return the first existing Dockerfile
3. Check the last edited Dockerfile, or return nil."
  (let* ((buffer (buffer-file-name))
         (path default-directory)
         (buffer (buffer-file-name))
         (relative-path (if buffer (file-relative-name (buffer-file-name) path) nil))
         (provider-dockerfile (car (idee-docker-get-dockerfiles-from-providers))))
    (cond
     ((equal "dockerfile-mode" major-mode) relative-path)
     (provider-dockerfile provider-dockerfile)
     ((file-exists-p idee-docker-last-edited-dockerfile) idee-docker-last-edited-dockerfile)
     (t nil))))

(defun idee-docker-dockerfile-from-project-root ()
  "Return the path of Dockerfile at the project root."
  (f-join (projectile-project-root) "Dockerfile"))         

(add-to-list 'idee-dockerfile-provider-list 'idee-docker-dockerfile-from-project-root)
(add-hook 'dockerfile-mode-hook (lambda () (setq idee-docker-last-edited-dockerfile (buffer-file-name))))

(define-key dockerfile-mode-map (kbd" C-c C-b") 'idee-docker-build)

(provide 'idee-docker)
;;; idee-docker.el ends here
