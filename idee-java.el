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

;; Package-Requires: ((emacs "25.1") (lsp-java "2.2") (dap-mode "0.3"))

;;; Code:

(require 'idee-vars)
(require 'idee-visitors)
(require 'idee-quarkus)

(require 'projectile)

(require 'cc-vars)
(require 'cc-cmds)

(defconst pom-xml "pom.xml")

(defconst source-main-prefix "src/main/java")
(defconst source-test-prefix "src/test/java")
(defconst source-prefix "src")
(defconst java-prefix "java")
(defconst test-prefix "test")

(defconst source-directory-list `(,source-main-prefix ,source-test-prefix ,java-prefix ,source-prefix ,test-prefix))

(defconst build-gradle "build.gradle")

(defconst idee-java-project-file-list `(,pom-xml ,build-gradle))


(defcustom idee-comment-java-custom-block-beginning "/**\n" "Custom block comment beginning to use when commenting java code." :group 'idee-java :type 'string)
(defcustom idee-comment-java-custom-block-ending "**/\n" "Custom block comment ending to use when commenting java code." :group 'idee-java :type 'string)
(defcustom idee-comment-java-custom-line-prefix " * " "Custom line prefix to use when commenting java code." :group 'idee-java :type 'string)



(defconst java-comment-style (make-idee-comment-style :block-beginning "/*" :block-ending "*/"
                                                      :custom-block-beginning idee-comment-java-custom-block-beginning :custom-line-prefix idee-comment-java-custom-line-prefix :custom-block-ending idee-comment-java-custom-block-ending))
(defvar idee--java-symbols)
(defvar idee-java-last-visited-class nil)

(defun idee-java-enable()
  "Enable java bindings."
  (interactive)

  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-mode-tab-width-function idee-function-alist) idee-function-alist))

  ;; Set functions
  (add-to-list 'idee-function-alist '(idee-mode-tab-width-function . idee-java-set-tab-width))

  (add-to-list 'idee-type-modes-alist '("java" . "java-mode"))
  (add-to-list 'idee-type-comment-styles-alist `("java" . ,java-comment-style)))

(defun idee-java-visit-file(&optional f)
  "Enable java bindings."
  (interactive)
  (when (idee-java-source-p (or f (buffer-file-name)))
    (let* ((package (idee-java-package-of default-directory))
           (classname (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
      (setq idee-java-last-visited-class (format "%s.%s" package classname)))))

;;; Formatting
(defun idee-java-set-tab-width (&optional w)
  "Replace the hook that set the tab width to W or idee-tab-width."
  (when w (setq idee-tab-width w))
  
  (remove-hook 'java-mode-hook 'idee-java-update-tab-width-callback)
  (add-hook 'java-mode-hook 'idee-java-update-tab-width-callback)
  (java-mode)
  (java-mode))

(defun idee-java-update-tab-width-callback()
  "Update the tab width for java hook."
  (setq c-basic-offset idee-tab-width))

;;; Language Functions
;;;###autoload
(defun idee-java-package-line()
  "Return the full package line for the current directory."
  (let ((pkg (idee-java-package-of default-directory)))
    (if pkg
        (concat "package " pkg ";"))))

(defun idee-java-package-of (f)
  "Return the package of the specified file F."
  (let* ((module-dir (idee-java-find-module-dir f))
         (source-dir (car (seq-filter (lambda (s) (file-exists-p (concat module-dir s))) source-directory-list)))
         (relative-path (substring f (+ (length module-dir) (length source-dir)))))
    (replace-regexp-in-string "^." ""
                              (replace-regexp-in-string ".$" ""
                                                        (replace-regexp-in-string "\/" "." relative-path)))))

(defun idee-java-find-module-dir (&optional f)
  "Find the directory of the java module that owns the source file F."
  (let* ((file-name (buffer-file-name (current-buffer)))
         (dir (if file-name (directory-file-name file-name) default-directory))
         (current-dir (f-full (if f f (file-name-directory dir)))))
    (while (and current-dir (not (idee-filesystem-root-p current-dir)) (not (idee-java-module-dir-p current-dir)))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    (if (idee-filesystem-root-p current-dir) nil current-dir)))

(defun idee-java-module-dir-p (f)
  "Return non-nil if F is a module directory."
  (if (seq-filter 'file-exists-p (seq-map (lambda (p) (concat f p)) idee-java-project-file-list))
      t
    nil))

(defun idee-java-find-src-dir (f)
  "Return the source root of F."
  (let* ((module-dir (idee-java-find-module-dir f))
         (source-dir  (concat (file-name-as-directory module-dir) source-main-prefix)))

    (if (file-exists-p source-dir)
        source-dir
      nil)))

(defun idee-java-source-p (f)
  "Return non-nil if F is a source file."
  (if (string-suffix-p ".java" f)
      (progn
        (let ((source-dir (idee-java-find-src-dir f)))
          (and source-dir (string-prefix-p source-dir f))))))

(defun idee-java-find-test-dir (f)
  "Return the test root of F."
  (let* ((module-dir (idee-java-find-module-dir f))
         (test-dir (concat (file-name-as-directory module-dir) source-test-prefix)))

    (if (file-exists-p test-dir)
        test-dir
      nil)))

(defun idee-java-test-p (f)
  "Return non-nil if F is a test file."
  (let ((test-dir (idee-java-find-test-dir f)))
    (message (format "file %s test dir %s." f test-dir))
    (and test-dir (string-prefix-p test-dir f))))

(defun idee--java-in-method-p()
  "Return non-nil if point is inside a method."
  (interactive)
  
  (let ((thing (thing-at-point 'defun)))))

(defun idee-java-method-name(thing)
  "Return non-nil if THING is a method."
  (interactive)
  (with-temp-buffer
    (save-excursion
      (insert thing))
    (c-end-of-defun)))

(defun idee-java-block-name()
  (interactive)
  (message (format "%s" (c-defun-name))))

(defun idee-java-method-name-at-point ()
  "Return the name of the method at point or nil if cursors is outside of a method.
The method obtains the whole block at point and strips everything found after '{'.
Finally it returns the last word before '('."
  (let* ((declaration-bounds (c-declaration-limits nil))
         (start (car declaration-bounds))
         (end (cdr declaration-bounds))
         (str  (buffer-substring start end))
         (declaration (substring str 0 (string-match "{" str))))
    (if (string-match "\\([^ ]+\\)[ ]*(" declaration) (match-string 1 declaration) nil)))

(defun idee--java-fqcn-matches-p (c)
  "Predicate that matches c against idee--java-symbols."
  (let* ((matches (seq-filter (lambda (i) (idee--java-fqcn-matches c i)) idee--java-symbols)))
    (message (format "%s -> %s" c matches))
    matches))

(defun idee--java-fqcn-matches (c f)
  "Match a candidate C to the fully qualified class name F.
- Example accepted matches for java.util.List:
   - Li
   - List
   - util.Li
   - util.List
   - java.util.List
   - j.u.List"
  (let* ((candidate-list (split-string c "\\([\\.]\\)"))
         (fqcn-list (split-string f "\\([\\.]\\)"))
         (reverse-candidate-list (reverse candidate-list))
         (reverse-fqcn-list (reverse fqcn-list))
         (canidate-class-name (car reverse-candidate-list))
         (fqcn-class-name (car reverse-fqcn-list))
         (candidate-package (reverse (cdr reverse-candidate-list)))
         (fqcn-package (reverse (cdr reverse-fqcn-list))))

    (and
     (cl-every (lambda (x y) (s-prefix-p x y)) (idee--java-camelcase-split canidate-class-name) (idee--java-camelcase-split fqcn-class-name))
     (cl-every (lambda (x y) (s-prefix-p x y)) candidate-package fqcn-package))))

(defun idee--java-camelcase-split (s)
  "Splits a camel case S into a list of strings."
  (let ((case-fold-search nil))
    (seq-filter
     (lambda (x) (not (equal "" x))) ;;remove all empty strings (its usually the first)
     (split-string (replace-regexp-in-string "\\([A-Z]\\)" " \\1" s) " "))))



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

  (idee-register-visitor 'idee-visitor-java)
  ;;; Hook
  (add-hook 'java-mode-hook 'idee-java-enable)
  (add-hook 'java-mode-hook 'idee-java-visit-file)

  (advice-add 'projectile-switch-to-buffer :after #'idee-java-visit-file))

(provide 'idee-java)
;;; idee-java.el ends here
