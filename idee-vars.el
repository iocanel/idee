;; idee-vars.el ---  Custom and Variables

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

(defcustom idee/resources-dir (locate-user-emacs-file "idee") "The directory where ide files are stored." :group 'ide :type 'string)
(defcustom idee/project-conf-dir ".idee" "The directory where ide configuration files are stored." :group 'ide :type 'string)

(defconst idee/emacs-templates-dir (concat (file-name-as-directory idee/resources-dir) "templates") "The directory where template files are stored.")
(defconst idee/emacs-snippets-dir (concat (file-name-as-directory idee/resources-dir) "snippets") "The directory where snippet files are stored.")
(defconst idee/emacs-headers-dir (concat (file-name-as-directory idee/resources-dir) "headers") "The directory where header files are stored.")

(defvar idee/source-dir nil "The source directory of the ide project.")

;; Tabs and indentation
(defvar idee/tab-width 2)
(defvar idee/use-tabs nil)

(defvar idee/source-file-extensions-list '(java kt groovy scala clojure xml go py js el))

(defvar idee/type-modes-alist '(("el" . "emacs-lisp-mode")
                                ("org" . "org-mode")
                                ("md" . "markdown-mode")
                                ("java" . "java-mode")
                                ("json" . "json-mode")
                                ("js" . "js-mode")
                                ("ts" . "typescript-mode")
                                ("py" . "python-mode")
                                ("go" . "go-mode")
                                ("cl" . "clojure-mode")
                                ("kt" . "kotlin-mode")
                                ("groovy" . "groovy-mode")
                                ("html" . "html-mode")
                                ("yml" . "yaml-mode")
                                ("yaml" . "yaml-mode")
                                ("sql" . "sql-mode")) "Association list for extension to mode.")



;;;###autoload
(defun idee/source-file-extensions ()
  "Returns the IDE source extension list."
  idee/source-file-extensions-list)

;;
;; Projects
;;
(cl-defstruct idee/project-factory
  name
  description
  func)

(cl-defstruct idee/project-info
  name
  version
  path
  properties)

(defconst idee/cask-project-factory (make-idee/project-factory
  :name "Cask"
  :description "Create an elisp project based on Cask."
  :func ()))

(defconst idee/project-root-markers '(".idee" ".git") "Files that indicate the root of a project")
(defconst idee/module-root-markers '() "Files that indicate the root of a module")
(defvar idee/project-factories-list `(,idee/cask-project-factory))

(defvar idee/project-info-alist nil)

(defvar idee/project-name-get nil)
(defvar idee/project-version nil)

;;
;; Comments
;;
;;;###autoload (autoload 'make-idee/comment-style "idee-comments")
(cl-defstruct idee/comment-style
  ;Language (used for comment detection.)
  block-beginning
  line-prefix
  block-ending
  ;Custom (used for styling)
  custom-block-beginning
  custom-line-prefix
  custom-block-ending)

(defvar idee/list-visitor () "List of registered visitors.")

;;
;; Headers
;;
(defvar idee/header-current nil)
;;
;; Comments
;;

;; General purpose comment styles
(defcustom idee/comment-elisp-custom-line-prefix ";; " "Custom line comment prefix to use when commenting elisp code." :group 'idee/elisp :type 'string)
(defcustom idee/comment-shell-custom-line-prefix "# " "Custom line comment prefix to use when commenting shell code." :group 'idee/shell :type 'string)

;; XML
(defcustom idee/comment-xml-custom-block-beginning "<!--\n" "Custom block comment beginning to use when commenting xml code." :group 'idee/xml :type 'string)
(defcustom idee/comment-xml-custom-block-ending "-->\n" "Custom block comment ending to use when commenting xml code." :group 'idee/xml :type 'string)
(defcustom idee/comment-xml-custom-line-prefix "~ " "Custom line prefix to use when commenting xml code." :group 'idee/xml :type 'string)

;; YML
(defcustom idee/comment-yml-custom-block-beginning nil "Custom block comment beginning to use when commenting yml code." :group 'idee/yml :type 'string)
(defcustom idee/comment-yml-custom-block-ending nil "Custom block comment ending to use when commenting yml code." :group 'idee/yml :type 'string)
(defcustom idee/comment-yml-custom-line-prefix "# " "Custom line prefix to use when commenting yml code." :group 'idee/yml :type 'string)


;; Comment styles
(defconst elisp-comment-style (make-idee/comment-style :line-prefix ";" :custom-line-prefix idee/comment-elisp-custom-line-prefix))
(defconst shell-comment-style (make-idee/comment-style :line-prefix "#" :custom-line-prefix idee/comment-shell-custom-line-prefix))
(defconst xml-comment-style (make-idee/comment-style :block-beginning "<!-" :block-ending "-->"
                                                     :custom-block-beginning idee/comment-xml-custom-block-beginning :custom-block-ending idee/comment-xml-custom-block-ending))
(defconst yml-comment-style (make-idee/comment-style :line-prefix " #"
                                                      :custom-block-beginning idee/comment-yml-custom-block-beginning :custom-line-prefix idee/comment-yml-custom-line-prefix :custom-block-ending idee/comment-yml-custom-block-ending))

(defvar idee/type-comment-styles-alist `(
                                         ("el" . ,elisp-comment-style)
                                         ("html" . ,shell-comment-style)
                                         ("sh" . ,shell-comment-style)
                                         ("xml" . ,xml-comment-style)
                                         ("yml" . ,yml-comment-style)
                                         ("yaml" . ,yml-comment-style)))

(defvar idee/current-comment-style elisp-comment-style)

;; Functions
(defvar idee/function-alist '((idee/open-function . project-switch-project)
                              (idee/new-project-function . idee/project-new)
                              (idee/new-file-function . idee/file-new)
                              (idee/recent-function . recentf)
                              (idee/save-all-function . idee/project-save-buffers)
                              (idee/close-function . idee/project-close-buffers)
                              (idee/build-function . project-compile)
                              (idee/run-or-eval-function . nil)
                              (idee/vcs-function . magit-status)
                              (idee/optimizie-imports-function . nil)
                              (idee/indent-function . evil-indent)
                              (idee/indent-region-function . nil)
                              (idee/header-select-function . idee/header-select)
                              (idee/apply-code-actions-function . idee/execute-code-actions)
                              (idee/references-function . nil)
                              (idee/implementation-function . nil)
                              (idee/declaration-function . nil)
                              (idee/back-function . nil)
                              (idee/grep-function . project-find-regexp)
                              (idee/find-file-function . project-find-fil)
                              (idee/find-variable-function . find-variable)
                              (idee/test-function . nil)
                              (idee/repl-view-function . nil)
                              (idee/mode-tab-width-function . idee/global-set-tab-width-function)
                              (idee/mode-hydra-function . nil)
                              (idee/shell-command-execute-in-project-function . idee/vterm-command-execute-in-project)
                              (idee/shell-visible-window-function . idee/vterm-visible-window)
                              (idee/shell-open-in-project-function . idee/vterm-open-in-project)
                              ))

;; On Event Command Association List
(defvar idee/on-event-command-alist '())


(provide 'idee-vars)
;;; idee-vars.el ends here
