;;;; drogue.lisp
(defpackage #:drogue
  (:use #:cl
        #:cl-charms)
  (:export :main))

(in-package #:drogue)

;;stuff in the namespace
(defvar *game* nil)
(defvar *current-ui* nil)


;;clostery

(defclass <game> ()())

(defclass <ui> ()())
(defclass <play> (<ui>)())
(defclass <win> (<ui>)())
(defclass <lose> (<ui>)())
(defmethod render-ui (<ui> <game>)
  (render-string "fobboob"
                 :x (random 10)
                 :y (random 10)))

(defmethod render-ui ((<play> <ui>) <game>)
  (render-string "i am the play ui"
                 :x (random 10)
                 :y (random 10)))

(defmethod process-input ((<play> <ui>) <game> input)
  (case input
    ((#\q) (quit-game))
    (otherwise (render-string
                "i am the ihnput handler"
                :x (random 10)
                :y (random 10)))))



;;cl-charms wrappers

(defun quit-game ()
  (sb-ext:exit))

(defun get-input ()
  (get-char (standard-window) :ignore-error t))

(defun render-string (string &key (x 0) (y 0))
  (write-string-at-point (standard-window) string x y))


;;game-wide stuff

(defun run-game (ui game input)
  (render-ui ui game)
  (process-input ui game input))

(defun init ()
  (disable-echoing)
  (enable-raw-input :interpret-control-characters t)
  (setf *game* (make-instance '<game>))
  (setf *current-ui* (make-instance '<play>)))

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
