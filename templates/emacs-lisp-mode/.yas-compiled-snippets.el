;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
                     '(("empty" ";;; `(file-name-nondirectory (buffer-file-name))` --- ${1:Description}\n\n`(idee-header)`\n\n;; Author: `(user-full-name)`\n\n;; Version: ${2:0.0.1}\n\n;; Package-Requires: ((emacs \"25.1\"))\n\n;;; Commentary:\n\n;;; Code:\n\n$0\n\n(provide '`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`)\n;;; `(file-name-nondirectory (buffer-file-name))` ends here." "empty" nil nil nil "/home/iocanel/.config/emacs/templates/emacs-lisp-mode/empty" nil nil)))


;;; Do not edit! File generated at Mon Jun 25 11:25:17 2018
