
(in-package :cl-user)
(defpackage #:drogue-utils
  (:nicknames utils)
  (:use #:cl
        #:cl-charms)
  (:export :log-to-file
           :handle-resize
           :is-resize))

(in-package #:drogue-utils)

(defun log-to-file (msg)
  (with-open-file
      (log "logfile.log"
           :direction :output
           :if-exists :supersede)
    (format log "~a~%" msg)))

(defun handle-resize ()
  (loop while (is-resize (get-char (standard-window)))
     do
       (format *terminal-io* "NO RESIZING ALLOWED!")
       (force-output *terminal-io*))
  (sb-ext:exit ))


(defun is-resize (input)
  (eq (char-code input) 410))
