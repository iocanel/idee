;;; idee-kubernetes.el --- Kubernetes integration

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

(require 'idee-eshell)

(defcustom idee-kubernetes-kubectl-binary "kubectl" "The kubectl binary to use (e.g kubectl. oc microk8s.kubectl)." :group 'idee-kubernetes :type 'string)

(defun idee-kubernetes-create-from-region(start end)
  "Pass the selected region to kubectl/oc create."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (clipboard-kill-ring-save (region-beginning) (region-end))
  (ide-shell-command-execute-in-project (format "cat /dev/clip | %s create -f -" idee-kubernetes-kubectl-binary)))

(defun idee-kubernetes-create-from-buffer()
  "Pass the current to kubectl/oc create."
  (interactive)
  (let ((file-name buffer-file-name))
    (ide-eshell-inin-project
        (let* ((path default-directory)
               (relative-path (file-relative-name file-name path)))
          
          (ide-shell-command-execute-in-project (format "%s create -f %s" idee-kubernetes-kubectl-binary  relative-path))))))

(defun idee-kubernetes-create-dwim (&optional start end)
  "Pass the selected region or currnent buffer (if region not active) to kubectl/oc create."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (region-active-p)
      (idee-kubernetes-create-from-region (region-beginning) (region-end))
    (idee-kubernetes-create-from-buffer)))

(defun idee-kubernetes-delete-from-buffer()
  "Pass the current to kubectl/oc delete."
  (interactive)
  (let ((file-name buffer-file-name))
    (ide-eshell-inin-project
        (let* ((path default-directory)
               (relative-path (file-relative-name file-name path)))
          (ide-shell-command-execute-in-project (format "%s delete -f %s" idee-kubernetes-kubectl-binary  relative-path))))))

(defun idee-kubernetes-delete-from-region(start end)
  "Pass the selected region to kubectl/oc delete"
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (clipboard-kill-ring-save (point-min) (point-max))
  (ide-shell-command-execute-in-project (format "cat /dev/clip | %s delete -f -" idee-kubernetes-kubectl-binary)))

(defun idee-kubernetes-delete-dwim(&optional start end)
  "Pass the selected region or currnent buffer (if region not active) to kubectl/oc delete."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (region-active-p)
      (idee-kubernetes-delete-from-region (region-beginning) (region-end))
    (idee-kubernetes-delete-from-buffer)))

(defun idee-kubernetes-replace-from-buffer()
  "Pass the current buffer to kubectl/oc create."
  (interactive)
  (let ((file-name buffer-file-name))
    (ide-eshell-inin-project
        (let* ((path default-directory)
               (relative-path (file-relative-name file-name path)))
          (ide-shell-command-execute-in-project (format "%s replace -f %s" idee-kubernetes-kubectl-binary  relative-path))))))

(defun idee-kubernetes-replace-from-region(start end)
  "Pass the selected region to kubectl/oc create."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (clipboard-kill-ring-save (point-min) (point-max))
  (ide-shell-command-execute-in-project (format "cat /dev/clip | %s replace -f -" idee-kubernetes-kubectl-binary)))

(defun idee-kubernetes-replace-dwim(&optional start end)
  "Pass the selected region or currnent buffer (if region not active) to kubectl/oc replace."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (region-active-p)
      (idee-kubernetes-replace-from-region (region-beginning) (region-end))
    (idee-kubernetes-replace-from-buffer)))

(defun idee-kubernetes--tmp-resource-name()
  "Create a temporary idee-kubernetes resource file."
  (concat temporary-file-directory "kubernetes-" (format "%06x-%06x" (random (expt 16 6)) (random (expt 16 6)))))

(provide 'idee-kubernetes)
;;; idee-kubernetes.el ends here
