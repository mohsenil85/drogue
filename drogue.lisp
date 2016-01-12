;;;; drogue.lisp
(defpackage #:drogue
  (:use #:cl
        #:termbox
        #:swank)
  (:export :main))

(in-package #:drogue)

(defstruct keypress  
  type mod ch key)

(defmacro log-debug (msg)
  `(with-open-file  (logfile "mylog.log"
                        :if-exists :supersede
                        :direction :output )
    (format logfile "~a~%" ,msg)) )   

(defun event->keypress (event)
  (let ((type (nth 1 event))
        (mod (nth 3 event))
        (ch (nth 5 event))
        (key (nth 7 event)))
    (make-keypress :type type
                   :mod mod
                   :ch ch
                   :key key)))
(defun event-char (event)
  (let ((c (nth 5 event))
        (mod (nth 7 event)))
    (case mod
      (9 #\TAB)
      (13 #\Return)
      (27 #\Esc)
      (32 #\Space)
      (otherwise (code-char c)))))

(defparameter *ui-stack* '('start 'play 'win 'lose))


(defun print-string (string x y)
  (let ((h (termbox:height))
        (w (termbox:width)))
    (declare (ignore h w))
    (loop for c across string do
         (termbox:change-cell x y (char-code c )))
    )
  )

(defparameter *inside-drogue* "yes i am inside drogue")
(defun sw-listen ()
  (log-debug (format nil "listening on ~a~%" 9000) )
  (swank:create-server :port 9000 :dont-close t   ))

(defun visible-debug ()
  (log-debug (format nil "called hahaha visi-debug~%" ) )
  (change-cell 1 1 (char-code #\#) termbox:+green+)
  (termbox:present)
  )


(defun do-loady ()
  (log-debug (format nil "called do-loady ~%" ) )
  (load "load.lisp"))

(defun log-bug ()
  (log-debug (format nil "called log-bug" ) )
  )

(defun main (args)
  (declare (ignore args))
  (sw-listen )
  (-main)
  )

(defun -main ()
  (termbox:init)
  (termbox:clear)
  (loop named input-loop for c = (event-char (termbox:poll-event)) do
       (case c
         (#\Return (log-bug))
         (#\q (return-from input-loop))
         (#\l (visible-debug) )
         (#\p (visible-debug) ))
       (log-debug (format nil "~a" c )))
  (termbox:change-cell 0 0 (char-code #\#) termbox:+green+ 0)
  (termbox:present)
  (termbox:shutdown))
