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

(require 'projectile)
(require 'flycheck)
(require 'use-package)
(require 'meghanada)
(require 'company-meghanada)


(require 'cc-vars)

(require 'idee-java)

(defconst source-main-prefix "src/main/java")
(defconst source-test-prefix "src/test/java")
(defconst source-prefix "src")
(defconst java-prefix "java")
(defconst test-prefix "test")

(defconst source-directory-list `(,source-main-prefix ,source-test-prefix ,java-prefix ,source-prefix ,test-prefix))

(defconst pom-xml "pom.xml")
(defconst build-gradle "build.gradle")
(defconst meghanada-conf ".meghanada.conf")

(defconst idee-meghanada-project-file-list `(,pom-xml ,build-gradle ,meghanada-conf))

(defcustom idee-meghanada-enabled t "Meghanada Feature Toggle" :group 'idee :type 'boolean)
(defcustom idee-meghanada-completion-enabled t "Meghanada Completion Feature Toggle" :group 'idee :type 'boolean)

(defun idee-meghanada-enable()
  "Enable meghanada, add hooks, visitors etc."
  (interactive)
  (use-package meghanada
    :ensure t
    :init
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")
    (setq company-meghanada-prefix-length 2)
    (setq meghanada-server-jvm-option "-ea -server -XX:+UseConcMarkSweepGC -XX:SoftRefLRUPolicyMSPerMB=50 -Xverify:none -Xms512m -Dfile.encoding=UTF-8"))

  (if idee-meghanada-completion-enabled
      (add-to-list 'company-backends 'company-meghanada)
    (setq company-backends (delete 'company-meghanada company-backends))))

(defun idee-meghanada-disable()
  "Disable meghanada, Remove hooks, visitors etc."
  (interactive)
  (setq company-backends (delete 'company-meghanada company-backends)))

(defun idee-meghanada-hook()
  "Set meghanada bindings."
  (idee-java-enable)
  (meghanada-mode t)
  (flycheck-mode +1)
  
  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-refernces-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-declaration-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-optimize-imports-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-indent-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-mode-hydra-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-run-or-eval-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-test idee-function-alist) idee-function-alist))

  ;; Set functions
  (add-to-list 'idee-function-alist '(idee-references-function . meghanada-reference))
  (add-to-list 'idee-function-alist '(idee-declaration-function . meghanada-jump-declaration))
  (add-to-list 'idee-function-alist '(idee-optimize-imports-function . meghanada-optimize-import))
  (add-to-list 'idee-function-alist '(idee-run-or-eval-function . meghanada-exec-main))
  (add-to-list 'idee-function-alist '(idee-test-function . idee-meghanada-test-dwim))
  (add-to-list 'idee-function-alist '(idee-mode-hydra-function . meghanada-hydra/body)))


(defun idee-meghanada-test-dwim()
  "Run unit test 'Do what I mean'."
  (interactive)
  (let ((method (which-function))
        (f (buffer-file-name (current-buffer))))

    (if (idee-java-source-p f)
        (message (format "%s is a source file" f))
      (if (idee-java-test-p f)
          (if method
              (meghanada-run-junit-test-case)
            (meghanada-run-junit-class))))))

;;; Visitor
(defun idee-meghanada-project-p (root)
  "Checks if meghanada mode is applicable to the project."
  (seq-filter (lambda (x)
                (or
                 (equal "pom.xml" x)
                 (equal "build.gradle" x)
                 (equal ".meghanada.conf" x)))
              (directory-files root)))

(defun idee-visitor-meghanada (root)
  "Check if a meghanada project is available under the specified ROOT."
  (if (and idee-meghanada-enabled (idee-meghanada-project-p root))
      (idee-meghanada-enable)))

(defun idee--meghanada-init ()
  (idee-register-visitor 'idee-visitor-meghanada)
  (add-hook 'java-mode-hook 'idee-meghanada-hook))

(provide 'idee-meghanada)
;;; idee-meghanada.el ends here
