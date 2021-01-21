;;; idee-vars.el ---  Custom and Variables

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
(defcustom idee-resources-dir (locate-user-emacs-file "idee") "The directory where idee files are stored." :group 'idee :type 'string)
(defcustom idee-project-conf-dir ".idee" "The directory where idee configuration files are stored." :group 'idee :type 'string)

;; Tabs and indentation
(defvar idee-tab-width 2)
(defvar idee-use-tabs nil)

(defvar idee-source-file-extensions-list '(java kt groovy scala clojure xml go py js el))

;;;###autoload
(defun idee-source-file-extensions ()
  "Returns the IDEE source extension list."
  idee-source-file-extensions-list)

;;
;; Projects
;;
(cl-defstruct idee-project-factory
  name
  description
  func)

(cl-defstruct idee-project-info
  name
  version
  path
  properties)

(defconst idee-cask-project-factory (make-idee-project-factory
  :name "Cask"
  :description "Create an elisp project based on Cask."
  :func ()))

(defconst idee-project-root-markers '(".idee" ".projectile" ".git") "Files that indicate the root of a project")
(defconst idee-module-root-markers '() "Files that indicate the root of a module")
(defvar idee-project-factories-list `(,idee-cask-project-factory))

(defvar idee-project-info-alist nil)

(defvar idee-project-get-name nil)
(defvar idee-project-version nil)

;;
;; Comments
;;
;;;###autoload (autoload 'make-idee-comment-style "idee-comments")
(cl-defstruct idee-comment-style
  ;Language (used for comment detection.)
  block-beginning
  line-prefix
  block-ending
  ;Custom (used for styling)
  custom-block-beginning
  custom-line-prefix
  custom-block-ending)

(defvar idee-project-visitors ())

;;
;; Headers
;;
(defvar idee--current-header nil)
;;
;; Comments
;;

;; General purpose comment styles
(defcustom idee-comment-elisp-custom-line-prefix ";; " "Custom line comment prefix to use when commenting elisp code." :group 'idee-elisp :type 'string)
(defcustom idee-comment-shell-custom-line-prefix "# " "Custom line comment prefix to use when commenting shell code." :group 'idee-shell :type 'string)

(defcustom idee-comment-xml-custom-block-beginning "<!--\n" "Custom block comment beginning to use when commenting xml code." :group 'idee-xml :type 'string)
(defcustom idee-comment-xml-custom-block-ending "-->\n" "Custom block comment ending to use when commenting xml code." :group 'idee-xml :type 'string)
(defcustom idee-comment-xml-custom-line-prefix "~ " "Custom line prefix to use when commenting xml code." :group 'idee-xml :type 'string)

(defconst elisp-comment-style (make-idee-comment-style :line-prefix ";" :custom-line-prefix idee-comment-elisp-custom-line-prefix))
(defconst shell-comment-style (make-idee-comment-style :line-prefix "#" :custom-line-prefix idee-comment-shell-custom-line-prefix))
(defconst xml-comment-style (make-idee-comment-style :block-beginning "<!-" :block-ending "-->"
                                                     :custom-block-beginning idee-comment-xml-custom-block-beginning :custom-block-ending idee-comment-xml-custom-block-ending))

(defvar idee-type-comment-styles-alist `(
                                         ("el" . ,elisp-comment-style)
                                         ("html" . ,shell-comment-style)
                                         ("sh" . ,shell-comment-style)
                                         ("xml" . ,xml-comment-style)))

(defvar idee-current-comment-style elisp-comment-style)

;; Functions
(defvar idee-function-alist '((idee-open-function . projectile-switch-project)
                              (idee-new-project-function . idee-new-project-function)
                              (idee-new-file-function . idee-new-file-function)
                              (idee-recent-function . projectile-recentf)
                              (idee-save-all-function . projectile-save-project-buffers)
                              (idee-close-function . idee-close-project-buffers)
                              (idee-build-function . projectile-compile-project)
                              (idee-run-or-eval-function . projectile-run-project)
                              (idee-vcs-function . magit-status)
                              (idee-optimizie-imports-function . nil)
                              (idee-indent-function . evil-indent)
                              (idee-indent-region-function . nil)
                              (idee-select-project-header-function . idee-select-project-header-function)
                              (idee-apply-code-actions-function . idee-execute-code-actions)
                              (idee-references-function . nil)
                              (idee-implementation-function . nil)
                              (idee-declaration-function . nil)
                              (idee-back-function . nil)
                              (idee-grep-function . idee-project-grep)
                              (idee-find-file-function . projectile-find-file-dwim)
                              (idee-find-variable-function . find-variable)
                              (idee-test-function . nil)
                              (idee-repl-view-function . nil)
                              (idee-mode-tab-width-function . idee-global-set-tab-width-function)
                              (idee-mode-hydra-function . nil)))

;; On Event Command Association List
(defvar idee-on-event-command-alist '())


;;; File Types

(provide 'idee-vars)
;;; idee-vars.el ends here
