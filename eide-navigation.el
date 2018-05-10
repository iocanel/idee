;;; eide-navigation.el --- Navigation Utilities

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

(require 'eide-vars)

(defun eide-back-push()
  (interactive)
  (if ignore-current-buffer
      (setq ignore-current-buffer nil)
    (setq eide-back-stack (cons (make-eide-buffer-point :buffer (current-buffer) :line (line-number-at-pos (point)) :column (current-column)) eide-back-stack))))

(defun eide-back-pop()
  (interactive)
  (let ((p (car eide-back-stack)))
    (setq eide-back-stack (cdr eide-back-stack))
    p))

(defun eide-forward-push()
  (interactive)
  (if ignore-current-buffer
      (setq ignore-current-buffer nil)
    (setq eide-forward-stack (cons (make-eide-buffer-point :buffer (current-buffer) :line (line-number-at-pos (point)) :column (current-column)) eide-back-stack))))

(defun eide-forward-pop()
  (interactive)
  (let ((p (car eide-forward-stack)))
    (setq eide-forward-stack (cdr eide-forward-stack))
    p))

(defun eide-forget-current-buffer()
  (interactive)
  (let ((c (current-buffer)) (b (car eide-back-stack)) (f (car eide-forward-stack)))
    (if (and b (equal c (eide-buffer-point-buffer b))) (eide-back-pop))
    (if (and f (equal c (eide-buffer-point-buffer f))) (eide-forward-pop))
    ) 
  (setq ignore-current-buffer t))


(defun eide-jump-back()
  (interactive)
  (let ((p (eide-back-pop)))
    (if p (progn
            (eide-forward-push)
            (switch-to-buffer (eide-buffer-point-buffer p))
            (goto-char (point-min))
            (forward-line (eide-buffer-point-line p))
            (move-to-column (eide-buffer-point-column p))
            (point))
      )
    )
  ) 

(defun eide-jump-forward()
  (interactive)
  (let ((p (eide-forward-pop)))
    (if p (progn
            (eide-back-push)
            (switch-to-buffer (eide-buffer-point-buffer p))
            (goto-char (point-min))
            (forward-line (eide-buffer-point-line p))
            (move-to-column (eide-buffer-point-column p))
            (point)
            )
      )
    )
  )

(provide 'eide-navigation)
