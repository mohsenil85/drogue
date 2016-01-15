(in-package :cl-user)
(defpackage #:drogue-utils
  (:nicknames utils)
  (:use #:cl
        #:cl-charms)
  (:export :log-to-file
           :handle-resize
           :is-resize
           :with-init
           :print-box))

(in-package #:drogue-utils)

(defun print-box (startx starty w h)
  (let ((win (standard-window)))
    (loop for i from startx below w do
         (loop for j from starty below h do
              (charms:write-string-at-point
               win
               "*" i j)))))

(defmacro with-init (&body body)
  `(progn
    (charms:with-curses ()
     (charms/ll:curs-set 0)
     (charms:disable-echoing)
     (charms:enable-raw-input :interpret-control-characters t)
     ,@body
     )))

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
