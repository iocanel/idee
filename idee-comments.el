;;; idee-comments.el --- Comment handling

;; Copyright (C) 2018 Ioannis Canellos
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

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

;;;###autoload (autoload 'make-idee-comment-style "idee-comments")
(cl-defstruct idee-comment-style
  ;Language (used for comment detection.)
  block-beginning
  line-prefix
  block-ending
  ;Custom (used for styling)
  custom-block-beginning
  custom-line-prefix
  custom-block-ending)

;;
;; Functions
;;
(defun idee--buffer-comment-style()
  "Return the buffer comment style."
  (let ((extension (file-name-extension (buffer-file-name (current-buffer)))))
    (cdr (assoc extension idee-type-comment-styles-alist))))


;;;###autoload
(defun idee-comment (content extension)
  "Apply comments to CONTENT for file EXTENSION."
  (let ((s (cdr (assoc extension idee-type-comment-styles-alist))))
    (if content
        (concat (idee-comment-style-custom-block-beginning s)
                (mapconcat 'identity (mapcar
                                      (lambda (l) (concat (idee-comment-style-custom-line-prefix s) l "\n"))
                                      (split-string content "\n")) "")
                (idee-comment-style-custom-block-ending s))
      nil)))

;;;###autoload
(defun idee-remove-comment-at-point ()
  "Remove the comment at the current point."
  (interactive)
  (save-excursion
    (let* ((style (idee--buffer-comment-style))
           (block-beginning (idee-comment-style-block-beginning style))
           (prefix (idee-comment-style-line-prefix style))
           (block-ending (idee-comment-style-block-ending style))
           (current (point))
           (begin (point-min))
           (end (point-min))
           (next (point-max)))
      
      (if (and block-beginning block-ending)
          ;; Detect end and start of comment.
          (progn
            ;; Move back enough characters so that we can read the end of the comment.
            (goto-char (- current (length block-ending)))
            (setq end (search-forward block-ending nil t))
            (setq begin (search-backward block-beginning nil t))
            (message (format "detecting region %s %s." begin end))
            (if end
                (progn
                  (goto-char end)
                  (setq next (search-forward block-beginning end t))
                  (if (and (>= current begin) (or (not next) (< end next)))
                      (delete-region begin end)
                    (message "no comment detected at point.")))))
        (progn
          (if (not (equal 1 (line-number-at-pos)))
              (while (idee--line-above-commented-or-empty-p) (forward-line -1))
            )
          (while (idee--line-commented-p) (kill-whole-line)))))))

(defun idee--line-commented-p ()
  "Check if current line is commented."
  (interactive)
    (let* ((style (idee--buffer-comment-style))
           (prefix (idee-comment-style-line-prefix style))
           ;;(line (thing-at-point 'line t))
           (begin (idee--point-beginning-of-line))
           (end (idee--point-end-of-line))
           (line (buffer-substring begin end))
           )
      (cl-search prefix line)))

(defun idee--line-empty-p ()
  "Check if current line is empty."
    (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun idee--line-commented-or-empty-p ()
  "Check if current line is commented."
  (interactive)
      (or (idee--line-empty-p) (idee--line-commented-p)))

(defun idee--line-above-commented-or-empty-p()
  "Check if the line above is commented or empty."
  (save-excursion
    (if (= (line-number-at-pos) 0)
        nil
      (progn
        (forward-line -1)
        (idee--line-commented-or-empty-p)))))

(provide 'idee-comments)
;;; idee-comments.el ends here
