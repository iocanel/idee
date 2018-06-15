;;; idee-hydra.el --- Hydra for Emacs IDE.

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

;; Code:

(require 'idee-vars)
(require 'idee-actions)
(require 'idee-navigation)
(require 'idee-views)
(require 'idee-templates)
(require 'idee-headers)

(require 'hydra)
(require 'company-yasnippet)


(defhydra idee-hydra (:hint nil :exit t)
  "
        ^ Project      ^Source^                 ^Navigate^         ^Search^             ^Task^              ^Layout^
        ^^^^^^-------------------------------------------------------------------------------------------------------
         _O_pen        _o_ptimize imports     _?_: declaration    _g_: grep             _r_: run/eval        _0_: terminal
         New _P_roject _i_ndent               _/_: references     _f_: find file        _u_: run unit test   _1_: ide
         New _F_ile    indent _r_egion        _\\_: implementation _v_: find variable                       _2_: side by side
         _R_ecent      toggle _t_ab enabled   _<_: back                                                  _3_: repl
         _S_ave all    toggle tab _w_idth     _>_: forward                                               _t_: toggle tree
         _C_lose       insert _s_nippet       _._: set mark                                              _c_: toggle cli
         _B_uild       select _h_eaders
         _V_cs      "

  ("O" idee-open)
  ("P" idee-new-project)
  ("F" idee-new-file)
  ("R" idee-recent)
  ("S" idee-save-all)
  ("C" idee-close)
  ("B" idee-build)
  ("V" idee-vcs)

  ("o" idee-optimize-imports)
  ("i" idee-indent)
  ("r" idee-indent-region)
  ("w" idee-toggle-tab-width)
  ("t" idee-toggle-use-tabs)
  ("s" company-yasnippet)
  ("h" idee-select-project-header)

  ("?" idee-declaration)
  ("/" idee-references)
  ("\\" idee-implementation)
  (">" idee-jump-forward)
  ("<" idee-jump-back)
  ("." idee-back-push)

  ("g" idee-grep)
  ("f" idee-find-file)
  ("v" idee-find-variable)

  ("r" idee-run-or-eval)
  ("u" idee-test)

  ("1" idee-ide-view)
  ("2" idee-side-by-side-view)
  ("3" idee-repl-view)
  ("0" idee-terminal-view)
  ("t" idee-toggle-tree) 
  ("c" idee-toggle-cli) 
  ("m" idee-mode-hydra "mode") 
  ("q" nil "quit"))



(provide 'idee-hydra)
;;; idee-hydra.el ends here
