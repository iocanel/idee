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

(defun idee--read-project-header()
  "Read the header from header.txt."
  (interactive)
  (idee-read-and-eval-template (concat projectile-project-root "header.txt")))


(defun idee--comment (content extension)
  "Apply comments to CONTENT for file EXTENSION."
  (let ((s (cdr (assoc extension idee-type-comment-styles-alist))))
    (if content
        (concat (idee-comment-style-above s)
                (mapconcat 'identity (mapcar
                                      (lambda (l) (concat (idee-comment-style-prefix s) l "\n"))
                                      (split-string content "\n")) "")
                (idee-comment-style-below s))
      nil)
    )
  )

(defun idee--set-header ()
  "Set the header value, if exists."
  (setq idee--current-header (idee--read-project-header))
  )

(defun idee-header()
  "Return the header commented for the current buffer style."
  (let ( (extension) (file-name-extension (buffer-file-name (current-buffer))))
    (idee--comment idee--current-header (file-name-extension (buffer-file-name (current-buffer))))
    )
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

(advice-add 'projectile-switch-project :after 'idee--set-header)

(provide 'idee-headers)
;;; idee-headers.el ends here
