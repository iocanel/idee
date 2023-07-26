;; idee-hydra.el --- Hydra for Emacs IDE.

; Copyright (C) 2018 Ioannis Canellos

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

;; Code:

(require 'idee-vars)
(require 'idee-headers)
(require 'idee-actions)
(require 'idee-navigation)

(require 'hydra)
(require 'company-yasnippet)

;;;###autoload (autoload 'idee/project-hydra/body "idee-hydra")
(defhydra idee/project-hydra (:hint nil :exit t)
"
        ^ Project      
        ^^^^^^---------
         _o_pen       
         _n_ew project 
         _s_ave all    
         _c_lose       
         _b_uild       
         _v_cs         
"
  ("o" idee/open)
  ("n" idee/new-project)
  ("s" idee/save-all)
  ("c" idee/close)
  ("b" idee/build)
  ("v" idee/vcs)
  ("q" nil "quit"))

;;;###autoload (autoload 'idee/file-hydra/body "idee-hydra")
(defhydra idee/file-hydra (:hint nil :exit t)
"
        ^ File
        ^^^^^^---------
         _o_pen       
         _n_ew 
         _r_ecent
         _s_ave 
         _c_lose       
"
  ("o" idee/find-file)
  ("n" idee/new-file)
  ("r" idee/recent)
  ("s" save-buffer)
  ("c" kill-buffer)
  ("q" nil "quit"))
 
;;;###autoload (autoload 'idee/navigation-hydra/body "idee-hydra")
(defhydra idee/navigation-hydra (:hint nil :exit t)
"
        ^ Navigation
        ^^^^^^---------
         _d_eclaration
         _r_eferenves
         _i_mplementation
         _b_ack 
         _f_orward
         _s_set mark
"
  ("d" idee/declaration)
  ("r" idee/references)
  ("i" idee/implementation)
  ("b" idee/jump-back)
  ("f" idee/jump-forward)
  ("s" idee/back-push)
  ("q" nil "quit"))
 

(defun idee/hydra--project-name ()
  (let ((name (idee/project-name-get)))
    (if name (intern name) (intern "_"))))

(defun idee/hydra--side-by-side-flag ()
  "Visual represntation of the side by side visibility."
  (if (idee/side-by-side-visible-p) (intern "*") (intern "_")))

(defun idee/hydra--diagnostics-flag ()
  "Visual represntation of the diagnostics visibility."
  (if (idee/diagnostics-visible-p) (intern "*") (intern "_")))

(defun idee/hydra--errors-flag ()
  "Visual represntation of the errors visibility."
  (if (idee/errors-visible-p) (intern "*") (intern "_")))

(defun idee/hydra--messages-flag ()
  "Visual represntation of the messages visibility."
  (if (idee/messages-visible-p) (intern "*") (intern "_")))

(defun idee/hydra--eww-flag ()
  "Visual represntation of the eww visibility."
  (if (idee/eww-visible-p) (intern "*") (intern "_")))

(defun idee/hydra--side-by-side-buffer ()
  "Visual represntation of the side by side visibility."
  (let* ((name idee/side-by-side-buffer)
         (stripped (if name (substring name 6 (- (length name) 1)) "*")))
    (intern stripped)))

(defun idee/hydra--repl-kind ()
  "Visual represntation of the side by side visibility."
    (if (and idee/repl-kind (idee/side-by-side-visible-p)) (intern idee/repl-kind) (intern " ")))

(defun idee/hydra--selected-header-kind ()
  "Visual represntation of the side by side visibility."
  (cond (idee/header-selected-kind (intern idee/header-selected-kind))
        ((file-exists-p (concat (idee/project-root-dir) "header.txt")) (intern "project"))
        (t (intern "none"))))

;; Hydra helpers
(defun idee/hydra--selected-url ()
  "List all selected profiles"
  (idee/with-project-settings "eww.el" idee/eww-url
      (or (mapcar 'intern idee/eww-url) idee/eww-url-default)))

;;;###autoload (autoload 'idee/hydra/body "idee-hydra")
(defhydra idee/hydra (:hint none :exit t)
  "
   ^Project^      ^Source^                 ^Navigate^            ^Search^              ^Task^               ^Layout^
   ?HEADER?
   -----------------------------------------------------------------------------------------------------------------   
   _o_: open       _O_: optimize imports   _?_: declaration      _g_: grep             _r_: run/eval        _t_: ?t? tree
   _p_: new        _i_: indent             _/_: references       _f_: find file        _u_: run unit test   _c_: ?c? cli
   _F_: new file   _r_: indent region      _=_: implementation   _v_: find variable                       ^^_d_: ?d? diagnostics
   _b_: by side    _T_: ?T? tab enabled    _<_: back                                                    ^^^^_e_: ?e? errors
   _R_: recent     _W_: ?W? tab width      _>_: forward                                                 ^^^^_m_: ?m? messages
   _S_: save all   _s_: insert snippet     _._: set mark                                                ^^^^_w_: ?w? eww [%(idee/hydra--selected-url)]
   _C_: close      _a_: code actions                                                                  ^^^^^^_x_: ?x? xwidget webkit [%(idee/hydra--selected-url)]       
   _B_: build      _H_: apply header (?H?)                                                       
   _V_: vcs        _h_: select headers 
                                                                                                  ^^^^^^^^^^_0_: reset
                                                                                                  ^^^^^^^^^^_1_: ?1? terminal
                                                                                                  ^^^^^^^^^^_2_: ?2?   
                                                                                                  ^^^^^^^^^^_3_: ?3? repl
   [_q_]: quit
   "
  ("HEADER" nil (idee/project-name-get))
  ("o" idee/open)
  ("b" idee/open-side-by-side)
  ("p" idee/new-project)
  ("F" idee/new-file)
  ("R" idee/recent)
  ("S" idee/save-all)
  ("C" idee/close)
  ("B" idee/build)
  ("V" idee/vcs)

  ("O" idee/optimize-imports)
  ("i" idee/indent)
  ("r" idee/indent-region)
  ("W" idee/toggle-tab-width (format "[%s]" idee/tab-width) :exit nil)
  ("T" idee/toggle-use-tabs (if idee/use-tabs "[*]" "[ ]") :exit nil)
  ("s" company-yasnippet)
  ("h" idee/header-select :exit nil)
  ("H" idee/header-apply-to-buffer (idee/hydra--selected-header-kind))
  ("a" idee/apply-code-actions)

  ("?" idee/declaration)
  ("/" idee/references)
  ("=" idee/implementation)
  (">" idee/jump-forward :exit nil)
  ("<" idee/jump-back :exit nil)
  ("." idee/back-push :exit nil)
  ("g" idee/grep)
  ("f" idee/find-file)
  ("v" idee/find-variable)

  ("r" idee/run-or-eval)
  ("u" idee/test)

  ("0" idee/reset-view) 
  ("1" idee/terminal-view (if (idee/cli-visible-p) "[*]" "[ ]"))
  ("2" idee/toggle-side-by-side (if (and idee/side-by-side-buffer (get-buffer idee/side-by-side-buffer)) (format "[*] side by side: (%s)" (idee/hydra--side-by-side-buffer)) "[ ] side by side"))
  ("3" idee/toggle-repl (if (idee/repl-visible-p) "[*]" "[ ]"))
  ("t" idee/toggle-tree (if (eq (treemacs-current-visibility) 'visible) "[*]" "[ ]"))
  ("c" idee/toggle-cli (if (idee/cli-visible-p) "[*]" "[ ]"))
  ("d" idee/toggle-diagnostics (if (idee/diagnostics-visible-p) "[*]" "[ ]"))
  ("e" idee/toggle-errors (if (idee/errors-visible-p) "[*]" "[ ]"))
  ("m" idee/toggle-messages (if (idee/messages-visible-p) "[*]" "[ ]"))
  ("w" idee/toggle-eww (if (idee/eww-visible-p) "[*]" "[ ]"))
  ("x" idee/toggle-xwidget-webkit (if (idee/xwidget-webkit-visible-p) "[*]" "[ ]"))
  ("q" nil "quit"))



(provide 'idee-hydra)
;; idee-hydra.el ends here
