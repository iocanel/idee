;;; idee-navigation.el --- Navigation Utilities

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

(require 'idee-vars)

(defun idee-back-push()
  (interactive)
  (if ignore-current-buffer
      (setq ignore-current-buffer nil)
    (setq idee-back-stack (cons (make-idee-buffer-point :buffer (current-buffer) :line (line-number-at-pos (point)) :column (current-column)) idee-back-stack))))

(defun idee-back-pop()
  (interactive)
  (let ((p (car idee-back-stack)))
    (setq idee-back-stack (cdr idee-back-stack))
    p))

(defun idee-forward-push()
  (interactive)
  (if ignore-current-buffer
      (setq ignore-current-buffer nil)
    (setq idee-forward-stack (cons (make-idee-buffer-point :buffer (current-buffer) :line (line-number-at-pos (point)) :column (current-column)) idee-back-stack))))

(defun idee-forward-pop()
  (interactive)
  (let ((p (car idee-forward-stack)))
    (setq idee-forward-stack (cdr idee-forward-stack))
    p))

(defun idee-forget-current-buffer()
  (interactive)
  (let ((c (current-buffer)) (b (car idee-back-stack)) (f (car idee-forward-stack)))
    (if (and b (equal c (idee-buffer-point-buffer b))) (idee-back-pop))
    (if (and f (equal c (idee-buffer-point-buffer f))) (idee-forward-pop))
    ) 
  (setq ignore-current-buffer t))


(defun idee-jump-back()
  (interactive)
  (let ((p (idee-back-pop)))
    (if p (progn
            (idee-forward-push)
            (switch-to-buffer (idee-buffer-point-buffer p))
            (goto-char (point-min))
            (forward-line (idee-buffer-point-line p))
            (move-to-column (idee-buffer-point-column p))
            (point))
      )
    )
  ) 

(defun idee-jump-forward()
  (interactive)
  (let ((p (idee-forward-pop)))
    (if p (progn
            (idee-back-push)
            (switch-to-buffer (idee-buffer-point-buffer p))
            (goto-char (point-min))
            (forward-line (idee-buffer-point-line p))
            (move-to-column (idee-buffer-point-column p))
            (point)
            )
      )
    )
  )

(provide 'idee-navigation)
