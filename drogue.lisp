;;;; drogue.lisp
(defpackage #:drogue
  (:use #:cl
        #:cl-charms)
  (:export :main))

(in-package #:drogue)

;;min window size ensurer
;;defui
;;;keymaps
;;;callbacks
;;;;start with inventory ui
;;;;also world state object
;;;need printing fns like print-center

(defparameter *ticks* 0)
(defparameter *width* 0)
(defparameter *height* 0)

(defun set-globals ()
  (multiple-value-bind (w h) (window-dimensions (standard-window))
    (setf *width* w)
    (setf *height* h)))

(defun write-at-center (string)
  (let* ((center-x (floor *width* 2))
         (center-y (floor *height* 2))
         (normal-x (- center-x (length string) )))
    (write-string-at-point (standard-window)
                           string
                           normal-x
                           center-y
                           )))

(defun game-loop ()
  (with-curses ()
    (disable-echoing)
    (enable-raw-input :interpret-control-characters t)
    (enable-non-blocking-mode (standard-window))
    (charms/ll:curs-set 0)
    (set-globals)
    (loop
       :named driver-loop
       :for c := (get-char (standard-window)
                           :ignore-error t)
       :do (progn
             (refresh-window (standard-window))
             (incf *ticks*)
             (write-at-center "foo")
             ))))


(defun main (args)
  (declare (ignore args))
  (game-loop)
  )
