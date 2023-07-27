;; idee-java-utils.el --- Java utils -*- lexical-binding: t -*-
;; Copyright (C) 2021 Ioannis Canellos 
;;     
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;         http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "28.0"))

;;; Commentary:

;;; Code:

(require 'idee-vars)
(require 'cc-vars)
(require 'cc-cmds)

(defconst pom-xml "pom.xml")

(defconst source-main-prefix "src/main/java")
(defconst source-test-prefix "src/test/java")
(defconst source-prefix "src")
(defconst java-prefix "java")
(defconst test-prefix "test")

(defconst idee/java-snippet-class-name "`(idee/java-class)`" "The snippet code to use to replace the actual class name")
(defconst idee/java-source-directory-list `(,source-main-prefix ,source-test-prefix ,java-prefix ,source-prefix ,test-prefix))
(defconst build-gradle "build.gradle")

(defconst idee/java-project-file-list `(,pom-xml ,build-gradle))

(defvar idee/java-symbols)

;;
;; Formatting
;;
(defun idee/java-set-tab-width (&optional w)
  "Replace the hook that set the tab width to W or idee/tab-width."
  (when w (setq idee/tab-width w))
  (remove-hook 'java-mode-hook 'idee/java-update-tab-width-callback)
  (add-hook 'java-mode-hook 'idee/java-update-tab-width-callback)
  (java-mode)
  (java-mode))

(defun idee/java-update-tab-width-callback()
  "Update the tab width for java hook."
  (setq c-basic-offset idee/tab-width))

;;
;; Language Functions
;;

;;;###autoload

(defun idee/java-package()
  "Return the package line for the current file."
  (idee/java-package-of default-directory))

;;;###autoload
(defun idee/java-package-line()
  "Return the full package line for the current directory."
  (let ((pkg (idee/java-package-of default-directory)))
    (when (and pkg (not (idee/string-blank pkg)))
        (concat "package " pkg ";"))))

;;;###autoload
(defun idee/java-class()
  "Return the class name for the current file."
  (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))

;;;###autoload
(defun idee/java-class-sans-suffix (suffix &optional class-name)
  "Return the class name for the current file."
  (let ((class-name (or class-name (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))))
    (replace-regexp-in-string (format "%s$" suffix) "" class-name)))

(defun idee/java-fqcn-of (f)
  "Return the fully qualified name of class F."
  (if (idee/ends-with f ".java")
      (let ((name (file-name-nondirectory (file-name-sans-extension  f)))
            (pkg (idee/java-package-of f)))
        (if pkg (concat pkg "." name) name))
    nil))

(defun idee/java-package-of (f)
  "Return the package of the specified file F."
  (if f
      (let* ((module-dir (idee/java-find-module-dir f))
             (source-dir (car (seq-filter (lambda (s) (file-exists-p (concat module-dir s))) idee/java-source-directory-list)))
             (relative-path (substring (file-name-directory (file-name-sans-extension f)) (+ (length module-dir) (length source-dir)))))
        (replace-regexp-in-string "^." ""
                                  (replace-regexp-in-string ".$" ""
                                                            (replace-regexp-in-string "\/" "." relative-path))))
    nil))

(defun idee/java-class-name-of (f)
  "Return the class name of the specified file F."
  (if f
      (let* ((fqcn (idee/java-fqcn-of f))
             (pkg (idee/java-package-of f)))
        (if pkg (substring fqcn (+ 1 (length pkg))) fqcn))

    nil))

(defun idee/java-package-of-fqcn (fqcn)
  "Return the package of the specified FQCN."
  (let* ((parts (split-string fqcn "\\."))
         (class-name (nth (- (length parts) 1) parts))
         (package (substring fqcn 0 (- (length fqcn) (length class-name) 1))))
    package))

(defun idee/java-find-module-dir (&optional f)
  "Find the directory of the java module that owns the source file F."
  (let* ((file-name (buffer-file-name (current-buffer)))
         (dir (if file-name (directory-file-name file-name) default-directory))
         (current-dir (f-full (if f f (file-name-directory dir)))))
    (while (and current-dir (not (idee/filesystem-root-p current-dir)) (not (idee/java-module-dir-p current-dir)))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    (if (idee/filesystem-root-p current-dir) nil current-dir)))

(defun idee/java-module-dir-p (f)
  "Return non-nil if F is a module directory."
  (if (seq-filter 'file-exists-p (seq-map (lambda (p) (concat f p)) idee/java-project-file-list))
      t
    nil))

(defun idee/java-find-src-dir (f)
  "Return the source root of F."
  (let* ((module-dir (idee/java-find-module-dir f))
         (source-dir  (if module-dir
                          (concat (file-name-as-directory module-dir) source-main-prefix)
                        nil)))
    (if (and source-dir (file-exists-p source-dir))
        source-dir
      nil)))

(defun idee/java-source-p (f)
  "Return non-nil if F is a source file."
  (if (string-suffix-p ".java" f)
      (progn
        (let ((source-dir (idee/java-find-src-dir f)))
          (and source-dir (string-prefix-p source-dir f))))))

(defun idee/java-find-test-dir (f)
  "Return the test root of F."
  (let* ((module-dir (idee/java-find-module-dir f))
         (test-dir (concat (file-name-as-directory module-dir) source-test-prefix)))

    (if (file-exists-p test-dir)
        test-dir
      nil)))

(defun idee/java-test-p (f)
  "Return non-nil if F is a test file."
  (let ((test-dir (idee/java-find-test-dir f)))
    (and test-dir (string-prefix-p test-dir f))))

(defun idee/java-in-method-p()
  "Return non-nil if point is inside a method."
  (interactive)
  (let ((thing (thing-at-point 'defun)))
    thing))

(defun idee/java-method-name(thing)
  "Return non-nil if THING is a method."
  (interactive)
  (with-temp-buffer
    (save-excursion
      (insert thing))
    (c-end-of-defun)))

(defun idee/java-class-name-at-point ()
  "Return the name of the class/interface/enum at point or nil if cursors is outside of a class.
The function obtains the whole block at point and strips everything found after '{'.
Finally it returns the first word before 'class', 'interface', 'enum'."
  (let* ((declaration-bounds (c-declaration-limits nil))
         (start (car declaration-bounds))
         (end (cdr declaration-bounds))
         (str  (if (and start end) (buffer-substring start end) nil))
         (declaration (if str (substring str 0 (string-match "{" str)) nil)))
    (if (and declaration (string-match ".*\\(class\\|interface|enum|@interface\\)[ ]*\\([a-zA-Z0-9_]+\\)" declaration)) (match-string 2 declaration) nil)))

(defun idee/java-method-name-at-point ()
  "Return the name of the method at point or nil if cursors is outside of a method.
The method obtains the whole block at point and strips everything found after '{'.
Finally it returns the last word before '('."
  (let* ((declaration-bounds (c-declaration-limits nil))
         (start (car declaration-bounds))
         (end (cdr declaration-bounds))
         (str  (buffer-substring start end))
         (declaration (substring str 0 (string-match "{" str))))
    (if (string-match "\\([^ ]+\\)[ ]*(" declaration) (match-string 1 declaration) nil)))

(defun idee/java-imports-section-at-point-p ()
  "Returns non-nil if the cursors is in the imports section (between package and class)."
  (if (not (idee/java-class-name-at-point))
      (progn
        (save-excursion
          (re-search-backward "^[ ]*package[ ]+[a-zA-Z0-9_\\.]+;" nil t)))
    nil))
(defun idee/java-jump-to-imports-section ()
  "Returns non-nil if the cursors is in the imports section (between package and class)."
  (interactive)
  (goto-char (point-max))
  (re-search-backward "^[ ]*import[ ]+\\(static[ ]+\\)?[a-zA-Z0-9_\\.]+;" nil t)
  (end-of-line))


;;
;; Other utils
;;
(defun idee/java-fqcn-matches-p (c)
  "Predicate that matches c against idee/java-symbols."
  (let* ((matches (seq-filter (lambda (i) (idee/java-fqcn-matches c i)) idee/java-symbols)))
    (message (format "%s -> %s" c matches))
    matches))

(defun idee/java-fqcn-matches (c f)
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
     (cl-every (lambda (x y) (s-prefix-p x y)) (idee/java-camelcase-split canidate-class-name) (idee/java-camelcase-split fqcn-class-name))
     (cl-every (lambda (x y) (s-prefix-p x y)) candidate-package fqcn-package))))

(defun idee/java-camelcase-split (s)
  "Splits a camel case S into a list of strings."
  (let ((case-fold-search nil))
    (seq-filter
     (lambda (x) (not (equal "" x))) ;;remove all empty strings (its usually the first)
     (split-string (replace-regexp-in-string "\\([A-Z]\\)" " \\1" s) " "))))


(provide 'idee-java-utils)
;; idee-java-utils.el ends here
