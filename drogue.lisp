(defpackage #:drogue
  (:use #:cl
        #:termbox
        #:swank)
  (:export :main))

(in-package #:drogue)

(defstruct event 
  type mod ch key)


(defun log-debug (msg)
  (with-open-file  (logfile "logfile.log"
                             :if-exists :supersede
                             :direction :output )
    (format logfile "~a~%" msg)) )   

(defun event-char (event)
  (let ((c (nth 5 event))
        (mod (nth 7 event)))
    (case mod
      (9 #\TAB)
      (13 #\Return)
      (27 #\Esc)
      (32 #\Space)
      (otherwise (code-char c)))))

(defun is-resize (event)
  (= 2 (nth 1 event)))

(defun ensure-screen-size (event)
  (let ((w (nth 3 event))
        (h (nth 5 event)))
    (if (and (not (= w 130))
                     (not (= h 40)))
        (progn
          (force-clear)
          (put-string "WRONG SIZE" 0 0)
          (put-string (format nil "H: ~a W: ~a" h w)  0 1)
          (present)))))

(defparameter *ui-stack* '('start 'play 'win 'lose))


(defun sw-listen ()
  (log-debug (format nil "listening on ~a~%" 9000) )
  (with-open-file
      (logfile "logfile.log" :if-exists :supersede
               :direction :output)
    (let ((*standard-output* logfile))
      (swank:create-server :port 9000 :dont-close t ))))

(defun visible-debug ()
  (log-debug (format nil "called hahaha visi-debug~%" ) )
  (change-cell 1 1 (char-code #\#) termbox:+green+)
  (termbox:present))

(defun put-string
    (string x y &optional
                  (fg termbox:+black+)
                  (bg termbox:+white+) )
  (let ((string string)
        (accum 0))
    (loop for c across string do
         (change-cell  (+ x accum) y  (char-code c) fg bg )
         (incf accum))))

(defun do-loady ()
  (log-debug (format nil "called do-loady ~%" ) )
  (load "load.lisp"))

(defun log-bug ()
  (log-debug (format nil "called log-bug" ) )
  )

(defun force-clear ()
  (loop for i below (termbox:width) do
       (loop for j below (termbox:height) do
            (put-string " " i j)))
  (termbox:present))

(defparameter *running* nil)

(defun handle-input (input)
  (case input
    (#\Return (log-bug))
    (#\q (sb-ext:exit))
    (#\t (print-a-thing) )
    (#\c (progn
           (termbox:clear)
           (termbox:present)))
    (#\s (put-string "no you" 0 4))
    (#\l (do-loady) )
    (#\p (visible-debug) )))


(defun print-a-thing ()
  (termbox:change-cell 0 0 (char-code #\#) termbox:+green+ 0))

(defun main (args)
  (declare (ignore args))
  (setf *running* t)
  (sw-listen )
  (termbox:init)
  (-main)
  (termbox:shutdown))


(defun -main ()
  (loop while *running* for event = (termbox:poll-event) do
       (let ((width (termbox:width))
             (height (termbox:height))
             (c (event-char event)))
         (log-debug (format nil "~a" event))
         (log-debug (format nil "~a" c ))
         (log-debug (format nil "~a" height ))
         (log-debug (format nil "~a" width ))
         (if (is-resize event)
             (ensure-screen-size event)
             (handle-input c))
         )))
