;;;; drogue.lisp
(defpackage #:drogue
  (:use #:cl
        #:cl-charms)
  (:export :main))

(in-package #:drogue)


(defun main (args)
  (declare (ignore args))
  (format t "foo"))
