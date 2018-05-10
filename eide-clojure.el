;;; eide-clojure.el --- Clojure IDE settings

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

(require 'eide-vars)
(require 'eide-visitors)

(defun cider-jack-in-and-switch ()
  "Jack in cider REPL and switch to the current projects REPL buffer."
  (interactive)
  (add-hook 'cider-connected-hook 'eide-cider-on-connected)
  (cider-jack-in)
  )

(defun eide-cider-on-connected()
  (remove-hook 'cider-connected-hook 'eide-cider-on-connected)
  (switch-to-buffer (get-buffer (format "*cider-repl %s*" (projectile-project-name))))
  (mapcar (lambda (a) (eval a))
          (alist-get 'on-repl-connected eide-on-event-command-alist))
  )

(defun eide-run-clojure-project ()
  (async-shell-command "lein run")
  )

(defun clojure-ide ()
  (interactive)
  (setq eide-function-alist (delq (assoc 'eide-repl-view-function eide-function-alist) eide-function-alist))
  (add-to-list 'eide-function-alist '(eide-repl-view-function . cider-jack-in-and-switch))

  (setq eide-function-alist (delq (assoc 'eide-run-or-eval-function eide-function-alist) eide-function-alist))
  (add-to-list 'eide-function-alist '(eide-run-or-eval-function . eide-run-clojure-project))
  )
(add-hook 'clojure-mode-hook 'clojure-ide)

(defun eide-visitor-clojure (root)
  (when (seq-filter (lambda (x) (equal "project.clj" x)) (directory-files root))
    (clojure-ide))
  ) 

(add-to-list 'eide-project-visitors 'eide-visitor-clojure)

(provide 'eide-clojure)
;;; eide-clojure.el ends here
