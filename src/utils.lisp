(in-package :cl-user)
(defpackage #:drogue-utils
  (:nicknames utils)
  (:use #:cl
        #:cl-charms)
  (:export :log-to-file
           :handle-resize
           :print-center
           :is-resize
           :with-init
           :swank-listen
           :is-swank-running
           :is-swank-connected
           :swank-kill
           :print-box
           :*width*
           :*height*
           ))

(in-package #:drogue-utils)

(defun print-box (startx starty w h)
  (log-to-file "called printbox")
  (let ((win (standard-window)))
    (loop for i from startx below (+ w startx) do
         (loop for j from starty below (+ h starty) do
              (charms:write-string-at-point
               win
               "*" i j)))))

(defparameter *width* 0)
(defparameter *height* 0)

(defun set-width-and-height ()
  (multiple-value-bind (w h) (window-dimensions (standard-window))
    (setq *width* w)
    (setq *height* h)))

(defmacro with-init (&body body)
  `(progn
     (charms:with-curses ()
       (set-width-and-height)
       (charms/ll:curs-set 0)
       (charms:disable-echoing)
       (charms:enable-raw-input :interpret-control-characters t)
       ,@body)))


(defun print-center (string &optional (plus-y 0))
  (let ((win (standard-window))
        (offset (floor (length string) 2)))
    (multiple-value-bind (w h) (window-dimensions win)
      (write-string-at-point win string
                             (floor (- w offset) 2 )
                             (floor (+ h plus-y) 2 ) )))) 


(defun log-to-file (msg)
  (with-open-file
      (log "logfile.log"
           :direction :output
           :if-exists :supersede)
    (format log "~a~%" msg)))

(defun handle-resize ()
  (get-char (standard-window))
  (format *terminal-io* "press any key to exit.")
  (force-output *terminal-io*)
  (sb-ext:exit ))

(defun is-resize (input)
  (eq (char-code input) 410))

(defun swank-init ()
  (log-to-file "listening on 9000...")
  (swank:set-package 'drogue)
  (swank:create-server :port 9000 :style :spawn :dont-close t))

(let ((listening nil )
      (connected nil))

  (defun on-client-connect (conn)
    (log-to-file (format nil "~A" conn))
    (charms:clear-window (standard-window) :force-repaint t)
    (setf connected t))

  (defun swank-listen ()
    (unless listening
      (log-to-file (format nil"swank-running ~a" listening))
      (push 'on-client-connect
            swank::*new-connection-hook*)
      (swank-init)
      (setf listening t)))

  
  (defun swank-kill ()
    (when listening
      (log-to-file (format nil"swank-running ~a" listening))
      (swank:stop-server 9000 )
      (setf listening nil)
      (setf connected nil)))

  (defun is-swank-running ()
    listening)
  (defun is-swank-connected ()
    connected)
  )
