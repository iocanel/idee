;;; idee-java.el --- Java IDE

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

(require 'idee-vars)
(require 'idee-headers)
(require 'projectile)

(require 'cc-vars)
(require 'cc-cmds)

(defconst source-main-prefix "src/main/java")
(defconst source-test-prefix "src/test/java")
(defconst source-prefix "src")
(defconst java-prefix "java")
(defconst test-prefix "test")

(defconst source-directory-list `(,source-main-prefix ,source-test-prefix ,java-prefix ,source-prefix ,test-prefix))

(defconst pom-xml "pom.xml")
(defconst build-gradle "build.gradle")
(defconst meghanada-conf ".meghanada.conf")

(defconst idee-java-project-file-list `(,pom-xml ,build-gradle ,meghanada-conf))


(defun idee-java-enable()
  "Enables java bindings"
  (interactive)

  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-mode-tab-width-function idee-function-alist) idee-function-alist))

  ;; Set functions
  (add-to-list 'idee-function-alist '(idee-mode-tab-width-function . idee-java-tab-width))

  (add-to-list 'idee-type-modes-alist '("java" . "java-mode"))

  ;; Define comment structure
  (defconst java-comment-style (make-idee-comment-style :above "/**\n" :prefix "  * " :below "**/\n"))
  (add-to-list 'idee-type-comment-styles-alist `("java" . ,java-comment-style))
  )


;;; Formatting
(defun idee-java-tab-width ()
  "Replace the hook that set the tab width."
  (remove-hook 'java-mode-hook 'idee-java-update-tab-width)
  (add-hook 'java-mode-hook 'idee-java-update-tab-width)
  (java-mode)
  (java-mode)
  )

(defun idee-java-update-tab-width()
  "Update the tab width for java hook."
  (setq c-basic-offset idee-tab-width)
  )

;;; Language Functions
(defun idee-java-package-line()
  "Return the full package line for the current directory."
  (let ((pkg (idee-java-package-of default-directory)))
    (if pkg
        (concat "package " pkg ";"))
    )
  )

(defun idee-java-package-of (f)
  "Return the package of the specified file F."
  (let ((relative-path) (module-dir) (source-dir))
    (setq module-dir (idee-java-find-module-dir f))
    (setq source-dir (car (seq-filter (lambda (s) (file-exists-p (concat module-dir s))) source-directory-list)))
    (setq relative-path (substring f (+ (length module-dir) (length source-dir))))
    (replace-regexp-in-string "^." ""
                              (replace-regexp-in-string ".$" ""
                                                        (replace-regexp-in-string "\/" "." relative-path)))
    )
  )

(defun idee-java-find-module-dir (f)
  "Find the directory of the module that owns the source file F."
  (let ((current-dir f))
    (while (not (idee-java-module-dir-p current-dir))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    current-dir
    )
  )

(defun idee-java-module-dir-p (f)
  "Return true if F is a module directory."
  (if (seq-filter 'file-exists-p (seq-map (lambda (p) (concat f p)) idee-java-project-file-list))
      t
    nil)
  )

(defun idee-java-test-dwim()
  "Run unit tests 'Do what I mean'."
  (interactive)
  (let ((method (which-function)))
    (message method)
    )
  )

(defun idee--java-in-method-p()
  "Return true if point is inside a method."
  (interactive)
  
  (let ((thing (thing-at-point 'defun))))
  )

(defun idee-java-method-name(thing)
  "Return t if THING is a method"
  (interactive)
  (with-temp-buffer
    (save-excursion
      (insert thing))
      (c-end-of-defun)
      )
  )

(defun idee-java-block-name()
  (interactive)
  (message (format "%s" (c-defun-name)))
  )

;;; Visitor
(defun idee-visitor-java (root)
  "Check if a java project is available under the specified ROOT."
  (when (seq-filter (lambda (x)
                      (or (equal pom-xml x)
                          (equal build-gradle x)
                          (equal meghanada-conf x))
                      (directory-files root))
                    (idee-java-enable))
    )
  )

(add-to-list 'idee-project-visitors 'idee-visitor-java)

;;; Hook
(add-hook 'java-mode-hook 'idee-java-enable)

(provide 'idee-java)
;;; idee-java.el ends here
