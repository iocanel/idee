;; idee-java.el --- Java support for IDE -*- lexical-binding: t -*-

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

(defcustom idee/comment-java-custom-block-beginning "/**\n" "Custom block comment beginning to use when commenting java code." :group 'idee/java :type 'string)
(defcustom idee/comment-java-custom-block-ending "**/\n" "Custom block comment ending to use when commenting java code." :group 'idee/java :type 'string)
(defcustom idee/comment-java-custom-line-prefix " * " "Custom line prefix to use when commenting java code." :group 'idee/java :type 'string)

(defconst java-comment-style (make-idee/comment-style :block-beginning "/*" :block-ending "*/"
                                                      :custom-block-beginning idee/comment-java-custom-block-beginning :custom-line-prefix idee/comment-java-custom-line-prefix :custom-block-ending idee/comment-java-custom-block-ending))
(defvar idee/java-last-visited-class nil)
(defvar idee/java-initialized nil)


(defun idee/java-enable()
  "Enable java bindings."
  (interactive)

  ;; Clear functions
  (setq idee/function-alist (delq (assoc 'idee/mode-tab-width-function idee/function-alist) idee/function-alist))

  (setq idee/function-alist (delq (assoc 'idee/repl-view-function idee/function-alist) idee/function-alist))
  ;; Set functions
  (add-to-list 'idee/function-alist '(idee/mode-tab-width-function . idee/java-set-tab-width))

  (add-to-list 'idee/function-alist '(idee/repl-view-function . idee/java-repl))
  (setq idee/repl-kind "jshell")
  (setq idee/repl-buffer-prefix "*jshell")
  (setq idee/repl-buffer-prompt "jshell>")
  (add-to-list 'idee/type-modes-alist '("java" . "java-mode"))
  (add-to-list 'idee/type-comment-styles-alist `("java" . ,java-comment-style)))

(defun idee/java-repl ()
  "Start the java repl."
  (interactive)
  (let* ((module-dir (idee/maven-module-root-dir))
         (module-pom (concat module-dir pom-xml))
         (jshell-buffer-name (format "jshell %s" (if module-dir module-dir (idee/project-root-dir))))
         (jshell-process-name (format "*jshell %s" (if module-dir module-dir (idee/project-root-dir))))
         (existing-buffer (car (seq-filter  (lambda (b) (idee/starts-with (buffer-name b) jshell-process-name)) (buffer-list)))))
    (if existing-buffer
        (switch-to-buffer existing-buffer)
      (let* ((new-jshell-process-name (format "*%s*" jshell-buffer-name)))
        (ansi-term "/bin/sh" jshell-buffer-name)
        (term-line-mode)
        (if (file-exists-p module-pom)
            (comint-send-string new-jshell-process-name (format "cd %s && mvn compile com.github.johnpoth:jshell-maven-plugin:1.3:run\n" module-dir))
          (comint-send-string new-jshell-process-name "jshell\n"))))))

(defun idee/java-visit-file(&optional f)
  "Enable java bindings for file F."
  (when (idee/java-source-p (or f (buffer-file-name)))
    (let* ((package (idee/java-package-of default-directory))
           (classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
      (setq idee/java-last-visited-class (format "%s.%s" package classname)))))


;;; Archetypes

(defun idee/java-archetype-create-class (fqcn archetype &optional src-path base-path)
  "Create a new java class with FQCN from the specified ARCHETYPE.
The new class will be created under SRC-PATH or `src/main/java` if the src path is ommitted.
The target module will be the current, unless BASE-PATH has been specified, in which case it will be used."
  (with-temp-buffer
    (let* ((base-path (file-name-as-directory (or base-path (idee/module-root-dir))))
           (src-path (file-name-as-directory (or src-path "src/main/java")))
           (package (idee/java-package-of-fqcn fqcn))
           (class-name (substring fqcn (+ 1 (length package))))
           (package-path (replace-regexp-in-string (regexp-quote ".") "/" package))
           (class (concat base-path (format "%s/%s/%s.java" src-path package-path class-name))))
      (set-visited-file-name class)
      (insert archetype)
      (java-mode)
      (yas-expand)
      (write-file class))))

(defun idee/java-archetype-create-resource (name archetype &optional src-path base-path)
  "Create a new java class with FQCN in the current module from the specified ARCHETYPE."
  (with-temp-buffer
    (let* ((base-path (file-name-as-directory (or base-path (idee/module-root-dir))))
           (src-path (file-name-as-directory (or src-path "src/main/resources")))
           (resource (concat base-path (format "%s/%s" src-path name)))
           (extension (file-name-extension resource))
           (mode (cdr (assoc extension idee/type-modes-alist))))
      (set-visited-file-name resource)
      (when archetype
        (insert archetype)
        (when mode (funcall mode))
        (yas-expand))
      (write-file resource))))

(defun idee/java-register-spi (interface impl)
  "Create a new java class with FQCN in the current module from the specified ARCHETYPE."
  (with-temp-buffer
    (let* ((spi-file-name (concat (idee/module-root-dir) (format "src/main/resources/META-INF/services/%s" interface))))
      (when (file-exists-p spi-file-name)
        (set-visited-file-name spi-file-name)
        (insert-file-contents spi-file-name))

      (if (not (idee/buffer-contains-string impl))
          (progn
            (goto-char (point-max))
            (insert impl)
            (write-file spi-file-name))))))

(defun idee/java-archetype-abstract-factory ()
  "A simple java factory archetype."
  (interactive)
  (let ((fqcn (read-string "Fully qualified class name:")))
    (idee/java-archetype-create-class fqcn "abstract-factory")))

(defun idee/java-archetype-annotation-processor ()
  "A simple java annotation and annotation processor archetype."
  (interactive)
  (let* ((current-fqcn (idee/java-fqcn-of (buffer-file-name)))
         (current-pkg (idee/java-package-of (buffer-file-name)))
         (fqcn (read-string "Annotation processor qualified class name:" current-pkg)))
    (idee/java-archetype-create-class fqcn "annotation")
    (idee/java-archetype-create-class (concat fqcn "Processor") "apt")
    (idee/java-register-spi "javax.annotation.processing.Processor" fqcn)))

;;
;; Snippets
;;

(defun idee/java-create-template (source-file name key &optional templates-dir)
  "Create snippet from file."
  (with-temp-buffer
    (let* ((root-dir (idee/project-root-dir (buffer-file-name)))
           (conf-dir (concat (file-name-as-directory root-dir) idee/project-conf-dir))
           (project-templates-dir (concat (file-name-as-directory conf-dir) "templates"))
           (templates-dir (or templates-dir project-templates-dir))
           (body (idee/java-snippet-body-from source-file))
           (target (concat (file-name-as-directory templates-dir) (format "java-mode/%s" key)))
           (parent (file-name-directory target)))
      (set-visited-file-name target)
      (insert (format "# name: %s" name))
      (newline)
      (insert (format "# key: %s" key))
      (newline)
      (insert "# --")
      (newline)
      (insert body)
      (write-file target)
      (yas-load-directory parent)
      (yas-compile-directory parent))))

(defun idee/java-snippet-body-from (f)
  "Create snippet from file F."
  (with-temp-buffer
    (save-excursion
      (insert-file-contents f)
      (goto-char (point-min))
      (idee/comment-remove-at-point java-comment-style)
      (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
             (class-name (idee/java-class-name-of f))
             (updated (concat
                       "`idee/header`\n" (replace-regexp-in-string "package[ ]+[a-zA-Z0-9\.-]+;" "`idee/java-package-line`"
                                                                   (replace-regexp-in-string (regexp-quote class-name) idee/java-snippet-class-name content t) t))))
        updated))))

(defun idee/filename ()
  "Return the name for the current file."
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

;;; Visitor
(defun idee/java-project-p (root)
  "Check if ROOT is the root path of a java project."
  (seq-filter (lambda (x)
                (or (equal pom-xml x)
                    (equal build-gradle x)))
              (directory-files root)))

;;;###autoload
(defun idee/java-visitor (root)
  "Check if a java project is available under the specified ROOT."
  (when (idee/java-project-p root)
    (idee/lsp-java-init)
    (idee/java-enable)))

;;;###autoload
(defun idee/java-init ()
  "Initialize java."
  (interactive)
  ;; Dependencies
  (idee/only-once idee/java-initialized
    ;; LSP
    (idee/lsp-java-init)

    ;; Maven
    (idee/visitor-register 'idee/maven-visitor)
    (idee/project-factory-register (make-idee/project-factory
                                    :name "Maven"
                                    :description "New Maven project from archetype."
                                    :func 'idee/new-maven-from-archetype-project))
    ;; Quarkus
    (idee/visitor-register 'idee/quarkus-visitor)
    (idee/project-factory-register (make-idee/project-factory
                                    :name "Quarkus"
                                    :description "New Quarkus project created using the quarkus maven plugin."
                                    :func 'idee/new-quarkus-rest-project))

    ;; Spring
    (idee/project-factory-register (make-idee/project-factory
                                    :name "Spring"
                                    :description "New Spring project created using https://start.spring.io"
                                    :func 'idee/new-spring-starter-project))

    (idee/template-factory-register (make-idee/template-factory
                                     :mode 'java-mode
                                     :description "A java temlate factory"
                                     :func 'idee/java-create-template))
    (idee/archetype-register
     (make-idee/archetype
      :name "Java Abstract Factory"
      :description "A simple java abstract factory"
      :func 'idee/java-archetype-abstract-factory))

    (idee/archetype-register
     (make-idee/archetype
      :name "Java Annotation and Processor"
      :description "A java annotation and a java annotation processor"
      :func 'idee/java-archetype-annotation-processor))

  ;;; Hook
    (add-hook 'java-mode-hook 'idee/java-enable)
    (add-hook 'java-mode-hook 'idee/java-visit-file)

    (advice-add 'projectile-switch-to-buffer :after #'idee/java-visit-file))))

(provide 'idee-java)
;;; idee-java.el ends here
