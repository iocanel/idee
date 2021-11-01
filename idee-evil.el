;;; idee-evil.el --- IDE binding for Evil Mode.

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

;;; Commentary:

;;; Code:

(require 'evil)
(require 'idee-hydra)

(defcustom idee-helm-ag-bindings-enabled t "Toggle to enable idee manage helm-ag bindings for q and RET." :group 'idee-evil :type 'boolean)
(defcustom idee-flycheck-bindings-enabled t "Toggle to enable idee manage flycheck bindings for q and RET." :group 'idee-evil :type 'boolean)
(defcustom idee-flymake-bindings-enabled t "Toggle to enable idee manage flymake bindings for q and RET." :group 'idee-evil :type 'boolean)

;;;###autoload
(defun idee--evil-init()
  ;; Let's set some bindings so that helm-projectile-ag and projectile-grep are consistent.
  (when (and (require 'helm nil 'noerror) idee-helm-ag-bindings-enabled)
    (evil-define-key 'normal helm-major-mode-map "q" 'quit-window)
    (evil-define-key 'normal helm-major-mode-map (kbd "RET") '(lambda () (interactive) (helm-ag-mode-jump-other-window) (idee-jump-to-non-ide-window))))

  (when (and (require 'flymake nil 'noerror) idee-flymake-bindings-enabled)
    (evil-define-key 'normal flymake-diagnostics-buffer-mode-map "q" 'idee-toggle-diagnostics))

  (when (and (require 'flycheck nil 'noerror) idee-flycheck-bindings-enabled)
    (evil-define-key 'normal flycheck-error-list-mode-map "q" 'idee-toggle-errors)))

(provide 'idee-evil)
;;; idee-evil.el ends here
