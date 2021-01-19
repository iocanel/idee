;;; idee.el --- A unified way to perform IDE-like tasks across mutiple languages and frameworks.

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

;; Package-Requires: ((emacs "25.1") (projectile "2.0.0") (helm-projectile "0.14.0") (helm-ag "0.58") (treemacs "2.6") (yasnippet "0.13.0") (hydra "0.15.0") (magit "2.90.1") (company "0.9.10" ) (lsp-mode "6.2") (lsp-ui "6.0") (dap-mode "0.3") (company-lsp "2.1.0"))

;;; Commentary:

;;; Code:
(require 'idee-vars)
(require 'idee-projects)
(require 'idee-templates)

(defcustom idee-repo-url "git@github.com:iocanel/idee.git" "The repository url of the idee project." :group 'idee :type 'string)

(defun idee-resources-init ()
  (interactive)
  "Initialize idee resources."
  (idee-git-checkout idee-repo-url idee-resources-dir '("headers" "templates" "snippets")))

;;;###autoload
(defun idee-init ()
  (interactive)
  "Initialize idee"
  (idee--projects-init)
  (idee--templates-init)
  (idee--headers-init)
  (idee--views-init)
  (idee--evil-init))

(provide 'idee)
;;; idee.el ends here
