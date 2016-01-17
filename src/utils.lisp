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
           :print-box
           :print-logo
           :*width*
           :*height*
           :*running*
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
(defparameter *running* nil )

(defparameter *logo* '(
                       
":::::::::  :::::::::   ::::::::   ::::::::  :::    ::: ::::::::::"
":+:    :+: :+:    :+: :+:    :+: :+:    :+: :+:    :+: :+:       "
"+:+    +:+ +:+    +:+ +:+    +:+ +:+        +:+    +:+ +:+       "
"+#+    +:+ +#++:++#:  +#+    +:+ :#:        +#+    +:+ +#++:++#  "
"+#+    +#+ +#+    +#+ +#+    +#+ +#+   +#+# +#+    +#+ +#+       "
"#+#    #+# #+#    #+# #+#    #+# #+#    #+# #+#    #+# #+#       "
"#########  ###    ###  ########   ########   ########  ##########"

                       ))


(defun print-logo (&optional (logo *logo*))
  (loop
     for l in (reverse logo)
     with i = 0
     do
       (print-center l i)
       (decf i)))


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
                             (- (floor w  2 ) offset)
                             (+ (floor h  2 ) plus-y) )))) 


(defun log-to-file (msg)
  (with-open-file
      (log "logfile.log"
           :direction :output
           :if-exists :supersede)
    (format log "~a~%" msg)))

(defun handle-resize ()
    (format *terminal-io* "press any key to exit.")
    (force-output *terminal-io*)
  (let ((c  (get-char (standard-window))))
    (format *terminal-io* "you pressed ~a" c))
  (sb-ext:exit ))

(defun is-resize (input)
  (eq (char-code input) 410))
