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

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:

(require 'evil)
(require 'evil-leader)
(require 'idee-hydra)

(evil-leader/set-key "i" 'idee-hydra/body)
(evil-leader/set-key "p" 'idee-project-hydra/body)
(evil-leader/set-key "f" 'idee-file-hydra/body)
(evil-leader/set-key "n" 'idee-navigation-hydra/body)
(evil-leader/set-key "m" 'idee-maven-hydra/body)

(provide 'idee-evil)
;;; idee-evil.el ends here
