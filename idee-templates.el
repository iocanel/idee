;;; idee-templates.el --- Emacs IDE - File templates.

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

(require 'yasnippet)

(defun idee-new-file-function()
  "Just creates an empty buffer"
  (interactive)
  (let ((path) (extension) (mode) (filetypes) (filetype))
    (setq path (ido-find-file))
    (setq extension (file-name-extension (buffer-file-name path)))
    (setq mode (cdr (assoc extension idee-type-modes-alist)))
    (setq filetypes (directory-files (concat (file-name-as-directory idee-emacs-templates-dir) mode)))
    (setq filetype (projectile-completing-read "Select type of file:" filetypes))
    
    (with-silent-modifications (write-file (buffer-file-name path)))
    (switch-to-buffer path)
    (insert filetype)
    (funcall (intern mode))
    (yas-expand)
    )
  )

(defun idee-select-project-header-function ()
  "Select a header for the project from the existing selection of headers."
  (interactive)
  (let ( (headers) (header) (content) )
    (setq headers (directory-files idee-emacs-headers-dir))
    (setq header (projectile-completing-read "Select header:" headers))
    (setq content (idee-read-and-eval-template (concat (file-name-as-directory idee-emacs-headers-dir) header)))
    (setq idee--current-header content)
    )
  )
(provide 'idee-templates)
;;; idee-templates.el ends here
