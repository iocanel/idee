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

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(require 'idee-utils)
(require 'idee-comments)
(require 'idee-projects)
(require 'idee-vars)

;;
;; Customization
;;
(defcustom idee-emacs-headers-dir "~/.emacs.d/headers" "The directory where header files are stored." :group 'idee :type 'string)

;;
;; State
;;
(defvar idee--current-header nil)

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
             (t nil))
       )
  )

(defun idee--set-header ()
  "Set the header value, if exists."
  (let ((h (idee--read-project-header)))
    (if h
        (setq idee--current-header h)
      )
    )
  )

(defun idee-header ()
  "Return the header commented for the current buffer style."
    (idee--comment idee--current-header (file-name-extension (buffer-file-name (current-buffer))))
 )

(defun idee-select-project-header ()
  "Select a header for the project from the existing selection of headers."
  (interactive)
  (let ( (headers) (header) )
    (setq headers (directory-files idee-emacs-headers-dir))
    (setq header (projectile-completing-read "Select header:" headers))
    (setq idee--current-header (idee-read-and-eval-template (concat (file-name-as-directory idee-emacs-headers-dir) header)))
    )
  )

(defun idee-apply-buffer-header ()
  "Apply the selected header to the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (idee-remove-comment-at-point)
    (insert (idee-header))
    )
  )

(advice-add 'projectile-switch-project :after 'idee--set-header)

(provide 'idee-headers)
;;; idee-headers.el ends here
