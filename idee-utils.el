;;; idee-utils.el --- Emacs IDE Utilities.

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
(defun idee-read-file (f)
  "Read the content of file F."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))


(defun idee-read-and-eval-template (f)
  "Read the template and evaluate quotes."
  (with-temp-buffer
    (insert-file-contents f)
    (yas--restore-backquotes (yas--save-backquotes))
    (buffer-substring-no-properties
     (point-min)
     (point-max))
    )
  )


;;; String Functions
(defun idee-starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t)
                     string)
       t))

(defun idee-ends-with (string suffix)
  "Return t if STRING ends with SUFFIX."
  (and (string-match (rx-to-string `(: ,suffix eos) t)
                     string)
       t))

(provide 'idee-utils)
;;; idee-utils.el ends here.
