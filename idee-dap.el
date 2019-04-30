;;; idee-dap.el --- Dap integration.  -*- lexical-binding: t -*-


;; Author: Ioannis Canellos

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;;; Code:
(require 'idee-utils)

(eval-after-load "dap-mode"
 '(defun dap-start-debugging (launch-args)
     "Start debug session with LAUNCH-ARGS.
Special arguments:

:wait-for-port - boolean defines whether the debug configuration
should be started after the :port argument is taken.

:program-to-start - when set it will be started using `compile' before starting the debug process."
     (-let* (((&plist :name :skip-debug-session :cwd :program-to-start
                      :wait-for-port :type :request :port
                      :environment-variables :hostName host) launch-args)
             (default-directory (or cwd default-directory)))
       (mapc (-lambda ((env . value)) (setenv env value)) environment-variables)
       
       (when program-to-start (idee-with-project-shell
                                  (when cwd (insert (format "cd %s\n" cwd)))
                                  (insert program-to-start)))

       (when wait-for-port (dap--wait-for-port host port 600 1))
       
       (unless skip-debug-session
         (let ((debug-session (dap--create-session launch-args)))
           (dap--send-message
            (dap--initialize-message type)
            (dap--session-init-resp-handler
             debug-session
             (lambda (initialize-result)
               (-let [debug-sessions (dap--get-sessions)]
                 
                 ;; update session name accordingly
                 (setf (dap--debug-session-name debug-session) (dap--calculate-unique-name
                                                                (dap--debug-session-name debug-session)
                                                                debug-sessions)
                       (dap--debug-session-initialize-result debug-session) initialize-result)
                 
                 (dap--set-sessions (cons debug-session debug-sessions)))
               (dap--send-message (dap--make-request request launch-args)
                                  (dap--session-init-resp-handler debug-session)
                                  debug-session)))
            debug-session)
           
           (dap--set-cur-session debug-session)
           (push (cons name launch-args) dap--debug-configuration)
           (run-hook-with-args 'dap-session-created-hook debug-session))
         (unless (and program-to-start dap-auto-show-output)
           (save-excursion (dap-go-to-output-buffer)))))))


(defadvice dap--go-to-stack-frame (after idee-refresh-on-stack-frame
                                      (&optional debug-session stack-frame))
  "Refresh IDE after a breakpoint has been hit."
  (idee-refresh-view)
  (idee-jump-to-non-ide-window)
  (recenter-top-bottom)
  (when (not (idee-hyda-visibile-p)) (dap-hydra))
  (when (not (get-buffer-window "*dap-ui-locals*") (dap-ui-locals))))

(ad-activate 'dap--go-to-stack-frame)

(provide 'idee-dap)
;;; idee-dap.el ends here
