;; idee-git.el --- Git utilities -*- lexical-binding: t -*-

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

;;; Commentary:

;;; Code:

(require 'magit)

(defun idee/git-lines (&rest args)
  "Execute Git with ARGS, returning its output as a list of lines.
Empty lines anywhere in the output are omitted.

If Git exits with a non-zero exit status, then report show a
message and add a section in the respective process buffer."
  (magit--with-temp-process-buffer
    (apply #'magit-git-insert args)
    (mapcar (lambda (s) (if (not (idee/starts-with s "commit")) (format "commit%s" s) s)) (split-string (buffer-string) "\ncommit" t))))

(defun idee/git-log-entry-sha (entry)
 (nth 0 (idee/string-match-as-list "commit \\([a-z0-9]+\\)\n" entry)))

(defun idee/git-log-entry-author (entry)
  "Get the author of the specified log ENTRY."
  (string-trim (nth 0 (idee/string-match-as-list "^Author: \\([a-z0-9A-Z ]+\\)" entry))))

(defun idee/git-log-entry-message (entry)
  "Get the author of the specified log ENTRY."
  (let ((lines (split-string entry "\n" t)))
    (string-trim (nth (- (length lines) 1) lines))))


(defun idee/git-compact-commit-log-entry (s)
   "Make the commit log entry more compact."
   (let ((sha (substring (idee/git-log-entry-sha s) 0 10))
         (author (idee/git-log-entry-author s))
         (msg (idee/git-log-entry-message s)))
     (format "%s %s %s" sha author msg)))


(defun idee/git-get-new-file-names (commit)
  "Select all the files names of the files added by the commit."
  (magit--with-temp-process-buffer
    (apply #'magit-git-insert `("show" ,commit))
    (mapcar (lambda (n)
              (substring (car (split-string n "\n")) 6))
            (seq-filter
             (lambda (s) (idee/starts-with s "+++ b/"))
             (split-string (buffer-string) "--- /dev/null\n" t)))))

(defun idee/git-get-file-from-commit (commit file-name)
  "Select all the files names of the files added by the commit."
  (magit--with-temp-process-buffer
    (apply #'magit-git-insert `("show" ,commit))
    (goto-char (point-min))
    (let* ((start nil)
           (end nil))
      (search-forward (format "+++ b/%s" file-name))
      (re-search-forward "^+")
      (setq start (point))
      (re-search-forward "^diff")
      (setq end (point))
      (replace-regexp-in-string "^+" "" (buffer-substring start (- end 5))))))
    

;;
;; Command
;;
(defun idee/git-select-commit ()
  "Select a commit."
  (interactive)
  (let* ((lines (idee/git-lines "log"))
         (compact-lines (mapcar #'idee/git-compact-commit-log-entry lines))
         (selected (completing-read "Select commit: " compact-lines)))
    (car (split-string selected " " t))))


(defun idee/git-test ()
  (interactive)
  "Do shit."
  (let* ((commit (idee/git-select-commit))
         (new-files (idee/git-get-new-file-names commit))
         (first-file (car new-files)))
       (message "%s" (idee/git-get-file-from-commit commit first-file))))

;;
;; Test
;;

(idee/git-log-entry-sha "commit 12345\nAuthor: Ioannis Canellos <iocanel@gmail.com>\nDate:")
(idee/git-log-entry-author "commit 12345\nAuthor: Ioannis Canellos <iocanel@gmail.com>\nDate:")
(idee/git-log-entry-message "commit 12345\nAuthor: Ioannis Canellos <iocanel@gmail.com>\nDate: Tue\n    feat: this is a test.")
          
(provide 'idee-git)
;; idee-git.el ends here
