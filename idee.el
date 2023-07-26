;;; idee.el --- A unified way to perform IDEE/like tasks across mutiple languages and frameworks.

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

;; Package-Requires: ((emacs "25.1") (swiper "0.13.0") (treemacs "2.6") (yasnippet "0.13.0") (hydra "0.15.0") (magit "2.90.1") (company "0.9.10" ) (lsp-mode "6.2") (lsp-ui "6.0") (dap-mode "0.3"))

;;; Commentary:

;;; Code:

(require 'idee-utils)
(require 'idee-vars)

(defcustom idee/repo-url "git@github.com:iocanel/idee.git" "The repository url of the ide project." :group 'ide :type 'string)
(defvar idee/initialized nil)

(defun idee/resources-install ()
  "Initialize ide resources."
  (interactive)
  (idee/git-checkout idee/repo-url idee/resources-dir '("headers" "templates" "snippets")))

;;;###autoload
(defun idee/init ()
  "Initialize idee."
  (interactive)

  (idee/only-once idee/initialized
    (idee/when-idle
     ;; Initialize Project
     (advice-add 'project-switch-project :after 'idee/project-initialize)

     ;; Intialize templates
     (advice-add 'project-switch-project :after 'idee/template-load-from-project)

     (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
     (when (not (file-exists-p idee/resources-dir)) (mkdir idee/resources-dir t))

     ;; Copy and compile templates if not there
     (when (not (file-exists-p idee/emacs-templates-dir))
       (copy-directory (idee/template-source-dir) idee/emacs-templates-dir)
       (yas-compile-directory idee/emacs-templates-dir))

     (yas-load-directory idee/emacs-templates-dir)

     ;; Copy and compile snippets if not there
     (when (not (file-exists-p idee/emacs-snippets-dir))
       (copy-directory (idee/snippet-source-dir) idee/emacs-snippets-dir)
       (yas-compile-directory idee/emacs-snippets-dir))

     (yas-load-directory idee/emacs-snippets-dir)

     (when (not (file-exists-p idee/emacs-headers-dir)) (copy-directory (idee/header-source-dir) idee/emacs-headers-dir))

     (add-to-list 'yas-snippet-dirs idee/emacs-templates-dir)
     (add-to-list 'yas-snippet-dirs idee/emacs-snippets-dir)

     ;; Intialize visitors
     (advice-add 'project-switch-project :after 'idee/apply-visitor)

     ;; Initialize Headers
     (advice-add 'project-switch-project :after 'idee/header-detect)

     ;; Initialize vterm
     (setq idee/function-alist (delq (assoc 'idee/shell-command-execute-in-project-function idee/function-alist) idee/function-alist))
     (setq idee/function-alist (delq (assoc 'idee/shell-visible-window-function idee/function-alist) idee/function-alist))
     (setq idee/function-alist (delq (assoc 'idee/shell-open-in-project-function idee/function-alist) idee/function-alist))

     ;; Register vterm functions
     (add-to-list 'idee/function-alist '(idee/shell-command-execute-in-project-function . idee/vterm-command-execute-in-project))
     (add-to-list 'idee/function-alist '(idee/shell-visible-window-function . idee/vterm-visible-window))
     (add-to-list 'idee/function-alist '(idee/shell-open-in-project-function . idee/vterm-open-in-project)))))

  (provide 'idee)
;;; idee.el ends here
