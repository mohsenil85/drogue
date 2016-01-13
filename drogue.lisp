(defpackage #:drogue
  (:use #:cl
        #:cl-charms
        #:drogue-ui)
  (:import-from #:swank
                :set-package
                :create-server
                :*log-output*
                :*log-events*)
  (:export :main))

(in-package #:drogue)

(defun log-to-file (msg)
  (with-open-file
      (log "logfile.log"
           :direction :output
           :if-exists :supersede)
    (format log "~a~%" msg)))

(defun swank-init ()
  (swank:set-package 'drogue)
  (swank:create-server :port 9000 :dont-close t))

(defun handle-resize ()
  (loop while (is-resize (get-char (standard-window)))
     do
       (format *terminal-io* "NO RESIZING ALLOWED!")
       (force-output *terminal-io*))
  (sb-ext:exit ))

(defun is-resize (input)
  (eq (char-code input) 410))

(defun handle-input (input)
  (log-to-file (format nil "got a char ~a~% " input ))
  (charms:write-string-at-point 
               (charms:standard-window)
               (string input)
               (random 10)
               (random 10)))

(defun main (args)
  (declare (ignore args))
  (swank-init)
  (charms:with-curses ()
    (log-to-file "i am working...")
    (loop for input = (charms:get-char
                       (charms:standard-window) :ignore-error t) do
         (if (is-resize input )
             (handle-resize)
             (handle-input input)))))
