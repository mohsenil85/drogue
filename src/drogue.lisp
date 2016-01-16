(defpackage #:drogue
  (:use #:cl
        #:cl-charms
        #:drogue-ui
        #:drogue-utils)
  (:export #:main))
(in-package #:drogue)

(defparameter *running* nil )


(defparameter *debug-ui* (make-instance 'drogue-ui:<debug-ui>))

(defun init-world ()
  (setf (get 'world 'ticks) 0))


(defun main (args)
  (declare (ignore args))
  (utils:with-init
    (init-world)
    (utils:log-to-file "i am working...")
    (run-ui *debug-ui*)))
