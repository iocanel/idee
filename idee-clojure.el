;;; idee-clojure.el --- Clojure IDE settings

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
(require 'cider)

(require 'idee-vars)

(defun cider-jack-in-and-switch ()
  "Jack in cider REPL and switch to the current projects REPL buffer."
  (interactive)
  (add-hook 'cider-connected-hook 'idee-cider-on-connected)
  (cider-jack-in))

(defun idee-cider-on-connected()
  (remove-hook 'cider-connected-hook 'idee-cider-on-connected)
  (switch-to-buffer (get-buffer (format "*cider-repl %s*" (projectile-project-name))))
  (mapcar (lambda (a) (eval a))
          (alist-get 'on-repl-connected idee-on-event-command-alist)))

(defun idee-run-clojure-project ()
  (async-shell-command "lein run"))

(defun idee-clojure-hook ()
  "Clojure hook."
  (interactive)
  (setq idee-function-alist (delq (assoc 'idee-repl-view-function idee-function-alist) idee-function-alist))
  (add-to-list 'idee-function-alist '(idee-repl-view-function . cider-jack-in-and-switch))

  (setq idee-function-alist (delq (assoc 'idee-run-or-eval-function idee-function-alist) idee-function-alist))
  (add-to-list 'idee-function-alist '(idee-run-or-eval-function . idee-run-clojure-project)))

(defun idee-visitor-clojure (root)
  (when (seq-filter (lambda (x) (equal "project.clj" x)) (directory-files root))
    (clojure-ide))) 

(defun idee--clojure-init ()
(add-hook 'clojure-mode-hook 'idee-clojure-hook)
(idee-register-visitor 'idee-visitor-clojure))

(provide 'idee-clojure)
;;; idee-clojure.el ends here
