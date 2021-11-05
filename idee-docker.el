;; idee-docker.el --- Docker integration.  -*- lexical-binding: t -*-


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:
(require 'idee-visitors)
(require 'dockerfile-mode)
(require 'projectile)

(defvar idee/dockerfile-provider-list nil)
(defvar idee/docker-last-edited-dockerfile nil)
(defvar idee/docker-registry nil)
(defvar idee/docker-image nil)

(defconst idee/last-visited-dockerfile "last-visited-dockerfile" "The property key that marks the last visited Dockerfile.")

(defun idee/docker-get-dockerfiles-from-providers()
  "Call all registered dockerfile providers."
  (interactive)
  (seq-filter 'file-exists-p (mapcar (lambda (p) (funcall p)) idee/dockerfile-provider-list)))

(defun idee/docker-build ()
  "Perform a docker build."
  (interactive)
  (let* ((dockerfile (idee/docker-find-dockerfile))
        (dockerfile-relative-path (file-relative-name dockerfile (projectile-project-root)))
        (docker-image (idee/docker-get-image-name)))
    (idee/shell-command-execute-in-project (format "docker build -f %s -t %s ." dockerfile-relative-path docker-image))))

(defun idee/docker-kill ()
  "Perform a docker kill for all containers using the applications docker image."
  (interactive)
  (let* ((docker-image (idee/docker-get-image-name)))
    (shell-command (format "docker ps | grep %s | cut -f1 -d \" \" | while read id; do docker kill $id; done" docker-image))))

(defun idee/docker-run-dockerfile ()
  "Perform a docker run using a know dockerfile."
  (interactive)
  (let* ((dockerfile (idee/docker-find-dockerfile))
        (docker-image (idee/docker-get-image-name))
        (ports (idee/docker-get-exposed-ports dockerfile))
        (cmd-builder nil))

    (push "docker run -it" cmd-builder)
    (if ports
        (progn
          (push "-p" cmd-builder)
          (dolist (p ports)
                  (push (format "%s:%s" p p) cmd-builder))))
    (push docker-image cmd-builder)
    (idee/shell-command-execute-in-project (string-trim (string-join cmd-builder " ")))))

(defun idee/docker-push-dockerfile ()
  "Perform a docker push using a known docker-image."
  (interactive)
  (let* ((docker-image (idee/docker-get-image-name)))
    (idee/shell-command-execute-in-project (format "docker push %s" docker-image))))


(defun idee/docker-get-image-name ()
  "Get the docker image name from settings or project parameters."
  (idee/with-project-settings "docker.el" idee/docker-image
                              (let  ((org (user-login-name))
                                     (name (idee/project-name-get))
                                     (version (idee/project-version-get))
                                     (docker-image idee/docker-image))

                                ;;
                                ;; When using project factories, it's possible that the visitors
                                ;; where called before the pom.xml, package.json etc was created.
                                ;; So, try to call them again or fallback to the latest
                                ;;
                                (when (not version)
                                  (idee/apply-visitor)
                                  (setq version (or (idee/project-version-get) "latest")))

                                (or  docker-image
                                     (if idee/docker-registry
                                         (format "%s/%s/%s:%s" idee/docker-registry org name version)
                                       (format "%s/%s:%s" org name version))))))

(defun idee/docker-get-exposed-ports(dockerfile)
  "Get the port numbers that are exposed in the DOCKERFILE as list."
  (let ((abs-dockerfile
         (if (not (file-name-absolute-p dockerfile)) dockerfile (f-join (projectile-project-root) dockerfile))))
    (with-temp-buffer
      (insert-file-contents abs-dockerfile)
      (let* ((begin (point-min))
             (end (point-max))
             (content (buffer-substring begin end))
             (match-list (idee/string-match-as-list "EXPOSE \\([0-9 ]+\\)" content))
             (match-ports (cl-remove-if (lambda (s) (string-match "[^0-9 ]+" s)) match-list))
             (ports-string (if match-ports (car match-ports) nil))
             (port-list (if (stringp ports-string) (split-string ports-string) nil)))
        port-list))))

(defun idee/docker-find-dockerfile()
  "Find the dockerfile.
The criteria are the following:

1. If current file is a Dockerfile return that.
2. Call all dockerfile providers and return the first existing Dockerfile
3. Check the last edited Dockerfile, or return nil."
  (let* ((buffer (buffer-file-name))
         (path (if buffer (buffer-file-name) nil))
         (provider-dockerfile (car (idee/docker-get-dockerfiles-from-providers)))
         (last-visited-dockerfile (idee/project-property-get idee/last-visited-dockerfile))
         (dockerfile))

    (setq dockerfile
          (cond
           ((equal 'dockerfile-mode major-mode) path)
           (provider-dockerfile provider-dockerfile)
           ((and last-visited-dockerfile (file-exists-p last-visited-dockerfile)) last-visited-dockerfile)
           (t nil)))
    dockerfile))
    

(defun idee/docker-dockerfile-from-project-root ()
  "Return the path of Dockerfile at the project root."
  (f-join (projectile-project-root) "Dockerfile"))

(defun idee/docker-init ()
  "Initialize idee/docker."
  (interactive)
  (add-to-list 'idee/dockerfile-provider-list 'idee/docker-dockerfile-from-project-root)
  (add-hook 'dockerfile-mode-hook (lambda () (idee/project-property-set idee/last-visited-dockerfile (buffer-file-name)))))

(provide 'idee-docker)
;; idee-docker.el ends here
