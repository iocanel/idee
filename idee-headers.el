;;; idee-headers.el --- Emacs IDE Header support.

;; Copyright (C) 2018 Ioannis Canellos

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;       http://www.apache.org/licenses/LICENSE-2.0

;;Unless required by applicable law or agreed to in writing, software
;;distributed under the License is distributed on an "AS IS" BASIS,
;;WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;See the License for the specific language governing permissions and
;;limitations under the License.

;; Author: Ioannis Canellos

;;; Commentary:

;;; Code:

(require 'idee-projects)
(require 'idee-vars)
(require 'idee-comments)
(require 'idee-vars)

;;
;; Customization
;;
(defconst idee-emacs-headers-dir (concat (file-name-as-directory idee-resources-dir) "headers") "The directory where header files are stored.")

(defvar idee-header-selected-kind nil "The kind of header currently selcted.")

;;
;; Functions
;; 
(defun idee--read-project-header ()
  "Read the header from header.txt."
  (interactive)
  (let ((root-dir-header (concat (idee-project-root-dir (buffer-file-name)) "header.txt"))
        (idee-dir-header (concat (idee-project-root-dir (buffer-file-name)) (file-name-as-directory idee-project-conf-dir) "header.txt")))

       (cond ((file-exists-p root-dir-header) (idee-read-and-eval-template root-dir-header))
             ((file-exists-p idee-dir-header) (idee-read-and-eval-template idee-dir-header))
             (t nil))))

(defun idee--set-header ()
  "Set the header value, if exists."
  (let ((h (idee--read-project-header)))
    (if h
        (setq idee--current-header h))))

(defun idee-header ()
  "Return the header commented for the current buffer style."
    (idee--set-header)
    (ide-comment idee--current-header (file-name-extension (buffer-file-name (current-buffer)))))

;;;###autoload
(defun idee-select-project-header ()
  "Select a header for the project from the existing selection of headers."
  (interactive)
  (let* ((headers (directory-files idee-emacs-headers-dir))
        (kind (projectile-completing-read "Select header:" headers)))
    (setq idee--current-header (idee-read-and-eval-template (concat (file-name-as-directory idee-emacs-headers-dir) kind)))))

;;;###autoload
(defun idee-apply-buffer-header ()
  "Apply the selected header to the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ide-comment-remove-at-point)
    (insert (idee-header))))

;;;###autoload
(defun idee-apply-header-to-file (f)
  "Apply the selected header to the specified file F."
  (find-file f)
  (idee-apply-buffer-header)
  (write-file f))

;;;###autoload
(defun idee-apply-header-to-project-files ()
  "Recursively visit all project files nad apply the selected header."
  (interactive)
  (idee-visit-project-files 'idee-apply-header-to-file))


;;;###autoload
(defun idee--headers-init ()
  "Initialize idee headers."
  (advice-add 'projectile-switch-project :after 'idee--set-header))

(provide 'idee-headers)
;;; idee-headers.el ends here
