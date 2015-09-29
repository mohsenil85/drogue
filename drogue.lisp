;;;; drogue.lisp
(defpackage #:drogue
  (:use #:cl
        #:cl-charms)
  (:export :main))

(in-package #:drogue)

;;clostery

(defclass <game> ()
  ((ticks :initform 0
         :type number
         :accessor ticks)))

(defclass <ui> ()())
(defclass <start> (<ui>)())
(defclass <play> (<ui>)())
(defclass <win> (<ui>)())
(defclass <lose> (<ui>)())

(defmethod render-ui :around((<play> <ui>) <game>)
  (clear-window (standard-window)))

;; (defmethod render-ui (<ui> <game>)
;;   (render-string "fobboob"
;;                  :x (random 10)
;;                  :y (random 10)))

(defmethod render-ui ((<play> <ui>) <game>)
  (render-string "fobboob"
                 :x (random 10)
                 :y (random 10)))

(defmethod render-ui ((<play> <ui>) <game>)
    (progn
      (render-string "#" :x *width* :y *height* )
      (render-string (format nil "GAME TIME: ~A" (ticks <game>)) :x 5 :y 5 )
      (render-string "i am the play ui q to quit f to incf game-time"
                  :x 0
                  :y 0)))

(defmethod render-ui ((<start> <ui>) <game>)
  (render-string-center "You awake in a quiet place..."))

(defmethod render-ui :after ( <ui> <game>)
  (move-cursor (standard-window) (1- *width*) (1- *height*)))


(defmethod process-input ((<start> <ui>) <game> input)
  (when input
    (switch-ui *play* <game>)))
(defmethod process-input ((<play> <ui>) <game> input)
  (case input
    ((#\q) (quit-game))
    ((#\f) (update-game <game> ))
    ((#\c) (render-string-center "this should be centered"))
    (otherwise (render-string
                "i am the ihnput handler"
                :x (random 10)
                :y (random 10)))))

(defmethod switch-ui (<ui> <game>)
  (setf *current-ui* <ui>)
  (render-ui <ui> <game>))
(defmethod update-game (<game>)
  (incf (ticks <game>)))


;;cl-charms wrappers

(defun quit-game ()
  (sb-ext:exit))

(defun get-input ()
  (get-char (standard-window) :ignore-error t))

(defun render-string (string &key (x 0) (y 0))
  (write-string-at-point (standard-window) string x y))

(defun render-string-center (string)
  (let ((y (floor (/ *height* 2)))
        (x (-
            (floor (/ *width* 2))
            (floor (/ (length string) 2)))))
    (render-string string :x x :y y)))


;;stuff in the namespace
(defparameter *game* nil)
(defparameter *current-ui* nil)
(defvar *height* 25)
(defvar *width* 80)

(defvar *play* (make-instance '<play>))
(defvar *start* (make-instance '<start>))


;;game-wide stuff

(defun run-game (ui game input)
  (render-ui ui game)
  (process-input ui game input))

(defun init ()
  (disable-echoing)
  (enable-raw-input :interpret-control-characters t)
  (setf *game* (make-instance '<game>))
  (setf *current-ui* *start*)
  (render-ui *current-ui* *game*)
  )

(defun main (args)
  (declare (ignore args))
  (with-curses ()
    (init)
    (loop
       for input = (get-input)
       do
         (run-game *current-ui* *game* input)

         )
    )
  )
