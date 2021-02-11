;;; idee-java.el --- Java support for IDEE -*- lexical-binding: t -*-

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

;; Package-Requires: ((emacs "25.1") (lsp-java "2.2") (dap-mode "0.3"))

;;; Code:

(require 'idee-arch)
(require 'idee-vars)
(require 'idee-templates)
(require 'idee-java-utils)
(require 'idee-visitors)
(require 'idee-quarkus)
(require 'idee-spring)
(require 'idee-lsp-java)

(require 'projectile)

(require 'cc-vars)
(require 'cc-cmds)

(require 'term)
(require 'dap-mode)
(require 'dap-hydra)
(require 'dap-ui)

(defcustom idee-comment-java-custom-block-beginning "/**\n" "Custom block comment beginning to use when commenting java code." :group 'idee-java :type 'string)
(defcustom idee-comment-java-custom-block-ending "**/\n" "Custom block comment ending to use when commenting java code." :group 'idee-java :type 'string)
(defcustom idee-comment-java-custom-line-prefix " * " "Custom line prefix to use when commenting java code." :group 'idee-java :type 'string)

(defconst java-comment-style (make-idee-comment-style :block-beginning "/*" :block-ending "*/"
                                                      :custom-block-beginning idee-comment-java-custom-block-beginning :custom-line-prefix idee-comment-java-custom-line-prefix :custom-block-ending idee-comment-java-custom-block-ending))
(defvar idee-java-last-visited-class nil)


(defun idee-java-enable()
  "Enable java bindings."
  (interactive)

  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-mode-tab-width-function idee-function-alist) idee-function-alist))

  (setq idee-function-alist (delq (assoc 'idee-repl-view-function idee-function-alist) idee-function-alist))
  ;; Set functions
  (add-to-list 'idee-function-alist '(idee-mode-tab-width-function . idee-java-set-tab-width))

  (add-to-list 'idee-function-alist '(idee-repl-view-function . idee-java-repl))
  (setq idee-repl-kind "jshell")
  (setq idee-repl-buffer-prefix "*jshell")
  (setq idee-repl-buffer-prompt "jshell>")
  (add-to-list 'idee-type-modes-alist '("java" . "java-mode"))
  (add-to-list 'idee-type-comment-styles-alist `("java" . ,java-comment-style)))

(defun idee-java-repl ()
  "Start the java repl."
  (interactive)
  (let* ((module-dir (idee-maven-module-root-dir))
         (module-pom (concat module-dir pom-xml))
         (jshell-buffer-name (format "jshell %s" (if module-dir module-dir (idee-project-root-dir))))
         (jshell-process-name (format "*jshell %s" (if module-dir module-dir (idee-project-root-dir))))
         (existing-buffer (car (seq-filter  (lambda (b) (idee-starts-with (buffer-name b) jshell-process-name)) (buffer-list)))))
    (if existing-buffer
          (switch-to-buffer existing-buffer)
      (let* ((new-jshell-process-name (format "*%s*" jshell-buffer-name)))
        (ansi-term "/bin/sh" jshell-buffer-name)
        (term-line-mode)
        (if (file-exists-p module-pom)
            (comint-send-string new-jshell-process-name (format "cd %s && mvn compile com.github.johnpoth:jshell-maven-plugin:1.3:run\n" module-dir))
          (comint-send-string new-jshell-process-name "jshell\n"))))))

(defun idee-java-visit-file(&optional f)
  "Enable java bindings."
  (when (idee-java-source-p (or f (buffer-file-name)))
    (let* ((package (idee-java-package-of default-directory))
           (classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
      (setq idee-java-last-visited-class (format "%s.%s" package classname)))))


;;; Archetypes

(defun idee-java-archetype-create-class (fqcn archetype)
  "Create a new java class with FQCN in the current module from the specified ARCHETYPE."
    (with-temp-buffer
      (let* ((package (idee-java-package-of-fqcn fqcn))
             (class-name (substring fqcn (+ 1 (length package))))
             (package-path (replace-in-string "." "/" package))
             (factory-class (concat (idee-module-root-dir) (format "src/main/java/%s/%s.java" package-path class-name))))
        (set-visited-file-name factory-class)
        (insert archetype)
        (java-mode)
        (yas-expand)
        (write-file factory-class))))

(defun idee-java-register-spi (interface impl)
  "Create a new java class with FQCN in the current module from the specified ARCHETYPE."
  (with-temp-buffer
    (let* ((spi-file-name (concat (idee-module-root-dir) (format "src/main/resources/META-INF/services/%s" interface))))
      (when (file-exists-p spi-file-name)
        (set-visited-file-name spi-file-name)
        (insert-file spi-file-name))

      (if (not (idee-buffer-contains-string impl))
          (progn
            (goto-char (point-max))
            (insert impl)
            (write-file spi-file-name))))))

(defun idee-java-archetype-abstract-factory ()
  "A simple java factory archetype."
  (interactive)
  (let ((fqcn (read-string "Fully qualified class name:")))
    (idee-java-archetype-create fqcn "abstract-factory")))

;;; Visitor
(defun idee-java-project-p (root)
  "Check if ROOT is the root path of a java project."
  (seq-filter (lambda (x)
                (or (equal pom-xml x)
                    (equal build-gradle x)))
              (directory-files root)))

(defun idee-visitor-java (root)
  "Check if a java project is available under the specified ROOT."
  (if (idee-java-project-p root)
      (idee-java-enable)))

;;;###autoload
(defun idee-java-init ()
  (interactive)
  "Initialize java."
  ;; Dependencies
  (idee--lsp-java-init)
  (idee--maven-init)
  (idee--quarkus-init)
  (idee--spring-init)

  (idee-register-archetype 'idee-java-archetype-abstract-factory)
  (idee-register-visitor 'idee-visitor-java)

  ;;; Hook
  (add-hook 'java-mode-hook 'idee-java-enable)
  (add-hook 'java-mode-hook 'idee-java-visit-file)

  (advice-add 'projectile-switch-to-buffer :after #'idee-java-visit-file))

(provide 'idee-java)
;;; idee-java.el ends here
