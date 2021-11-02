;;; idee-dap.el --- Dap integration.  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Ioannis Canellos
;;     
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;         http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;; 

;; Author: Ioannis Canellos

;;; Commentary:

;; Package-Requires: ((dap-mode "0.6"))

;;; Code:
(require 'dap-mode)
(require 'idee-eshell)

(eval-after-load "dap-mode"

  '(defun dap-start-debugging-noexpand (launch-args)
  "Start debug session with LAUNCH-ARGS.
Special arguments:

:wait-for-port - boolean defines whether the debug configuration
should be started after the :port argument is taken.

:program-to-start - when set it will be started using `compilation-start'
before starting the debug process."
  (-let* (((&plist :name :skip-debug-session :cwd :program-to-start
                   :wait-for-port :type :request :port
                   :startup-function :environment-variables :hostName host) launch-args)
          (session-name (dap--calculate-unique-name name (dap--get-sessions)))
          (default-directory (or cwd default-directory))
          (process-environment (if environment-variables
                                   (cl-copy-list process-environment)
                                 process-environment))
          program-process)
    (mapc (-lambda ((env . value)) (setenv env value t)) environment-variables)
    (plist-put launch-args :name session-name)

    (when program-to-start (ide-eshell-inin-project
                                  (mapc (-lambda ((env . value)) (setenv env value)) environment-variables)
                                  (when cwd (insert (format "cd %s\n" cwd)))
                                  (insert program-to-start)))

    (when wait-for-port
      (dap--wait-for-port host port dap-connect-retry-count dap-connect-retry-interval))

    (when startup-function (funcall startup-function launch-args))

    (unless skip-debug-session
      (let ((debug-session (dap--create-session launch-args)))
        (setf (dap--debug-session-program-proc debug-session) program-process)
        (dap--send-message
         (dap--initialize-message type)
         (dap--session-init-resp-handler
          debug-session
          (-lambda ((&hash "body" capabilities))
            (-let [debug-sessions (dap--get-sessions)]

              (ht-update! (dap--debug-session-current-capabilities debug-session) capabilities)

              (dap--set-sessions (cons debug-session debug-sessions)))
            (dap--send-message
             (dap--make-request request (-> launch-args
                                            (cl-copy-list)
                                            (dap--plist-delete :dap-compilation)
                                            (dap--plist-delete :dap-compilation-dir)
                                            (dap--plist-delete :cleanup-function)
                                            (dap--plist-delete :startup-function)
                                            (dap--plist-delete :dap-server-path)
                                            (dap--plist-delete :environment-variables)
                                            (dap--plist-delete :wait-for-port)
                                            (dap--plist-delete :skip-debug-session)
                                            (dap--plist-delete :program-to-start)))
             (dap--session-init-resp-handler debug-session)
             debug-session)))
         debug-session)

        (dap--set-cur-session debug-session)
        (push (cons session-name launch-args) dap--debug-configuration)
        (run-hook-with-args 'dap-session-created-hook debug-session))))))

(defadvice dap--go-to-stack-frame (after idee-refresh-on-stack-frame
                                      (&optional debug-session stack-frame))
  "Refresh IDE after a breakpoint has been hit."
  (idee-jump-to-non-ide-window)
  (recenter-top-bottom)
  (when (not (idee-hydra-visible-p)) (dap-hydra))
  (when (not (get-buffer-window "*dap-ui-locals*")) (dap-ui-locals))
  (dap-ui-expressions))


(ad-activate 'dap--go-to-stack-frame)

(provide 'idee-dap)
;;; idee-dap.el ends here
