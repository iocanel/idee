;;; idee-headers.el --- Emacs IDE Header support.

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

(require 'idee-utils)

(defun idee--read-project-header()
  "Read the header from header.txt."
  (interactive)
  (idee-read-and-eval-template (concat projectile-project-root "header.txt")))


(defun idee--buffer-comment-style()
  "Return the buffer comment style."
  (let ((extension (file-name-extension (buffer-file-name (current-buffer)))))
    (cdr (assoc extension idee-type-comment-styles-alist))
    )
  )

(defun idee--comment (content extension)
  "Apply comments to CONTENT for file EXTENSION."
  (let ((s (cdr (assoc extension idee-type-comment-styles-alist))))
    (if content
        (concat (idee-comment-style-above s)
                (mapconcat 'identity (mapcar
                                      (lambda (l) (concat (idee-comment-style-prefix s) l "\n"))
                                      (split-string content "\n")) "")
                (idee-comment-style-below s))
      nil)
    )
  )

(defun idee--set-header ()
  "Set the header value, if exists."
  (setq idee--current-header (idee--read-project-header))
  )

(defun idee-header()
  "Return the header commented for the current buffer style."
  (let ( (extension) (file-name-extension (buffer-file-name (current-buffer))))
    (idee--comment idee--current-header (file-name-extension (buffer-file-name (current-buffer))))
    )
  )

(defun idee-select-project-header ()
  "Select a header for the project from the existing selection of headers."
  (interactive)
  (let ( (headers) (header) )
    (setq headers (directory-files idee-emacs-headers-dir))
    (setq header (projectile-completing-read "Select header:" headers))
    (setq idee--current-header (idee-read-and-eval-template (concat (file-name-as-directory idee-emacs-headers-dir) header)))
    )
  )

(defun idee-apply-buffer-header ()
  "Apply the selected header to the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (idee-remove-comment-at-point)
    (insert (idee-header))
    )
  )

(defun idee-remove-comment-at-point ()
  "Remove the comment at the current point."
  (interactive)
  (save-excursion
    (let* ((style (idee--buffer-comment-style))
           (above (idee-comment-style-above style))
           (prefix (idee-comment-style-prefix style))
           (below (idee-comment-style-below style))
           (begin (point-min))
           (end (point-min))
           (next (point-max))
           )
      
      (if (and above below)
          ;; Detect end and start of comment.
          (progn
            (setq current (point))
            ;; Move back enough characters so that we can read the end of the comment.
            (goto-char (- current (length below)))
            (setq end (search-forward below nil t))
            (setq begin (search-backward above nil t))
            (goto-char end)
            (setq next (search-forward above end t))
            (if (and (>= current begin) (or (not next) (< end next)))
                (delete-region begin end)
              (message "no comment detected at point.")
              )
            )
        (progn
          (if (not (equal 1 (line-number-at-pos)))
              (while (idee--line-above-commented-or-empty-p) (forward-line -1))
            )
          (while (idee--line-commented-p) (kill-whole-line))
          )
        )
      )
    )
  )

(defun idee--line-empty-p ()
  "Check if current line is empty."
    (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun idee--line-commented-p ()
  "Check if current line is commented."
  (interactive)
    (let* ((style (idee--buffer-comment-style))
           (prefix (idee-comment-style-prefix style))
           ;;(line (thing-at-point 'line t))
           (begin (idee--point-beginning-of-line))
           (end (idee--point-end-of-line))
           (line (buffer-substring begin end))
           )
      (cl-search prefix line)
      )
  )

(defun idee--line-commented-or-empty-p ()
  "Check if current line is commented."
  (interactive)
      (or (idee--line-empty-p) (idee--line-commented-p))
  )

(defun idee--line-above-commented-or-empty-p()
  "Check if the line above is commented or empty."
  (save-excursion
    (if (= (line-number-at-pos) 0)
        nil
      (progn
        (forward-line -1)
        (idee--line-commented-or-empty-p)
        )
      )
    )
  )

(advice-add 'projectile-switch-project :after 'idee--set-header)

(provide 'idee-headers)
;;; idee-headers.el ends here
