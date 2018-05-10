;;; eide-hydra.el --- Hydra for Emacs IDE.

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

(require 'eide-vars)
(require 'eide-actions)
(require 'eide-navigation)
(require 'eide-views)

(require 'hydra)
(require 'yasnippet)


(defhydra eide-hydra (:hint nil :exit t)
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

  ("O" eide-open)
  ("R" eide-recent)
  ("S" eide-save-all)
  ("C" eide-close)
  ("B" eide-build)
  ("V" eide-vcs)

  ("o" eide-optimize-imports)
  ("i" eide-indent)
  ("r" eide-indent-region)
  ("I" eide-toggle-tab-width)
  ("T" eide-toggle-use-tabs)
  ("s" company-yasnippet)
  ("l" eide-license-headers)

  ("?" eide-declaration)
  ("/" eide-references)
  (">" eide-jump-forward)
  ("<" eide-jump-back)
  ("." eide-back-push)

  ("g" eide-grep)
  ("f" eide-find-file)
  ("v" eide-find-variable)

  ("r" eide-run-or-eval)
  ("u" eide-test)

  ("1" eide-ide-view)
  ("2" eide-side-by-side-view)
  ("3" eide-repl-view)
  ("0" eide-terminal-view)
  ("t" eide-toggle-tree) 
  ("c" eide-toggle-cli) 
  ("m" eide-mode-hydra "mode") 
  ("q" nil "quit"))



(provide 'eide-hydra)
;;; eide-hydra.el ends here
