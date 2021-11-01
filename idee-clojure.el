;;; idee-clojure.el --- Clojure IDE settings  -*- lexical-binding: t -*-

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

;; Package-Requires: ((emacs "25.1") (cider "0.26.1"))

;;; Commentary:

;;; Code:

(require 'projectile)
(require 'cider)

(require 'idee-templates)
(require 'idee-vars)
(require 'idee-views)
(require 'idee-visitors)

(defconst clojure-comment-style (make-ide-comment-style :line-prefix ";;"))

(defun idee-clojure-enable()
  "Enable clojure bindings."
  (interactive)
  ;; Clear functions
  (setq idee-function-alist (delq (assoc 'idee-mode-tab-width-function idee-function-alist) idee-function-alist))
  (setq idee-function-alist (delq (assoc 'idee-repl-view-function idee-function-alist) idee-function-alist))
  ;; Set functions
  (add-to-list 'idee-function-alist '(idee-mode-tab-width-function . idee-java-set-tab-width))
  (add-to-list 'idee-function-alist '(idee-repl-view-function . idee-clojure-repl))
  (setq idee-function-alist (delq (assoc 'idee-run-or-eval-function idee-function-alist) idee-function-alist))
  (add-to-list 'idee-function-alist '(idee-run-or-eval-function . idee-clojure-run-project))
  (setq idee-repl-buffer-prefix "*cider-repl")
  (add-to-list 'idee-type-modes-alist '("clj" . "clojure-mode"))
  (add-to-list 'idee-type-comment-styles-alist `("clj" . ,clojure-comment-style)))

(defun idee-clojure-repl ()
  "Start the clojure repl."
  (interactive)
  (let ((repl-buffer (get-buffer (format "*cider-repl %s*" (projectile-project-name)))))
    (if repl-buffer
        (switch-to-buffer repl-buffer)
      (idee-clojure-jack-in-and-switch))))

(defun idee-clojure-jack-in-and-switch ()
  "Jack in cider REPL and switch to the current projects REPL buffer."
  (interactive)
  (add-hook 'cider-connected-hook 'idee-clojure--cider-on-connected)
  (cider-jack-in nil))

(defun idee-clojure--cider-on-connected()
  (remove-hook 'cider-connected-hook 'idee-cider-on-connected)
  (switch-to-buffer (get-buffer (format "*cider-repl %s*" (projectile-project-name))))
  (mapcar (lambda (a) (eval a))
          (alist-get 'on-repl-connected idee-on-event-command-alist)))

(defun idee-clojure-run-project ()
  (async-shell-command "lein run"))

(defun idee-visitor-clojure (root)
  (when (seq-filter (lambda (x) (equal "project.clj" x)) (directory-files root))
    (idee-clojure-enable))) 

(defun idee--clojure-init ()
  (add-hook 'clojure-mode-hook 'idee-clojure-enable)
  (idee-register-visitor 'idee-visitor-clojure))

(provide 'idee-clojure)
;;; idee-clojure.el ends here
