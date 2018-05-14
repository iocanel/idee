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

(require 'hydra)
(require 'company-yasnippet)


(defhydra idee-hydra (:hint nil :exit t)
  "
        ^ Project      ^Source^                 ^Navigate^         ^Search^             ^Task^              ^Layout^
        ^^^^^^-------------------------------------------------------------------------------------------------------
          _O_ open      _o_ optimize imports     _?_: declaration   _g_: grep             _r_: run/eval        _0_: terminal
          _R_ recent    _i_ indent               _/_: references    _f_: find file        _u_: run unit test   _1_: ide
          _S_ save all  _r_ indent region        _<_: back          _v_: find variable                       _2_: side by side
          _C_ close     _I_ toggle tab width     _>_: forward                                              _3_: repl
          _B_ build     _T_ toggle use tabs      _._: set mark                                             _t_: toggle tree
          _V_ vcs       _s_ insert snippet                                                               _c_: toggle cli
                      _l_ license headers
          "

  ("O" idee-open)
  ("R" idee-recent)
  ("S" idee-save-all)
  ("C" idee-close)
  ("B" idee-build)
  ("V" idee-vcs)

  ("o" idee-optimize-imports)
  ("i" idee-indent)
  ("r" idee-indent-region)
  ("I" idee-toggle-tab-width)
  ("T" idee-toggle-use-tabs)
  ("s" company-yasnippet)
  ("l" idee-license-headers)

  ("?" idee-declaration)
  ("/" idee-references)
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
