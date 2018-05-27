;;; idee-meghanada.el --- Meghanada IDE

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
(require 'meghanada)

(require 'cc-vars)

(defconst source-main-prefix "src/main/java")
(defconst source-test-prefix "src/test/java")
(defconst source-prefix "src")
(defconst java-prefix "java")
(defconst test-prefix "tesrt")

(defconst source-directory-list `(,source-main-prefix ,source-test-prefix ,java-prefix ,source-prefix ,test-prefix))

(defconst pom-xml "pom.xml")
(defconst build-gradle "build.gradle")
(defconst meghanada-conf ".meghanada.conf")

(defconst idee-meghanada-project-file-list `(,pom-xml ,build-gradle ,meghanada-conf))


(defun idee-meghanada-enable()
  "Enables java bindings"
  (interactive)
  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-refernces-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-declaration-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-optimize-imports-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-indent-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-mode-tab-width-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-mode-hydra-function idee-function-alist) idee-function-alist))

  ;; Set functions
  (add-to-list 'idee-function-alist '(idee-references-function . meghanada-reference))
  (add-to-list 'idee-function-alist '(idee-declaration-function . meghanada-jump-declaration))
  (add-to-list 'idee-function-alist '(idee-optimize-imports-function . meghanada-optimize-import))
  (add-to-list 'idee-function-alist '(idee-mode-tab-width-function . idee-meghanada-tab-width))
  (add-to-list 'idee-function-alist '(idee-mode-hydra-function . meghanada-hydra/body))

  (add-to-list 'idee-type-modes-alist '("java" . "java-mode"))

  ;; Define comment structure
  (defconst java-comment-style (make-idee-comment-style :above "/**\n" :prefix "  * " :below "**/"))
  (add-to-list 'idee-type-comment-styles-alist `("java" . ,java-comment-style))
  )


;;; Formatting
(defun idee-meghanada-tab-width ()
  "Replace the hook that set the tab width."
  (remove-hook 'java-mode-hook 'idee-meghanada-update-tab-width)
  (add-hook 'java-mode-hook 'idee-meghanada-update-tab-width)
  (java-mode)
  (java-mode)
  )

(defun idee-meghanada-update-tab-width()
  "Update the tab width for java hook"
  (setq c-basic-offset idee-tab-width)
  )

;;; Language Functions
(defun idee-meghanada-package-line()
  "Return the full package line for the current directory."
  (let ((pkg (idee-meghanada-package-of default-directory)))
    (if pkg
        (concat "package " pkg ";"))
    )
  )

(defun idee-meghanada-package-of (f)
  "Return the package of the specified file F."
  (let ((relative-path) (module-dir) (source-dir))
    (setq module-dir (idee-meghanada-find-module-dir f))
    (setq source-dir (car (seq-filter (lambda (s) (file-exists-p (concat module-dir s))) source-directory-list)))
    (setq relative-path (substring f (+ (length module-dir) (length source-dir))))
    (replace-regexp-in-string "^." ""
                              (replace-regexp-in-string ".$" ""
                                                        (replace-regexp-in-string "\/" "." relative-path)))
    )
  )

(defun idee-meghanada-find-module-dir (f)
  "Find the directory of the module that owns the source file F."
  (let ((current-dir f))
    (while (not (idee-meghanada-module-dir-p current-dir))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    current-dir
    )
  )

(defun idee-meghanada-module-dir-p (f)
  "Return true if F is a module directory."
  (if (seq-filter 'file-exists-p (seq-map (lambda (p) (concat f p)) idee-meghanada-project-file-list))
      t
    nil)
  )

;;; Visitor
(defun idee-visitor-meghanada (root)
  "Check if a meghanada project is available under the specified ROOT."
  (when (seq-filter (lambda (x)
                      (or (equal "pom.xml" x)
                          (equal "build.gradle" x)
                          (equal ".meghanada.conf" x))
                      (directory-files root))
                    (idee-meghanada-enable))
    )
  )

  (add-to-list 'idee-project-visitors 'idee-visitor-meghanada)

;;; Hook
  (add-hook 'meghanada-mode-hook 'idee-meghanada-enable)

  (provide 'idee-meghanada)
;;; idee-meghanada.el ends here
