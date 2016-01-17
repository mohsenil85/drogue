(defpackage #:drogue
  (:use #:cl
        #:cl-charms
        #:drogue-ui
        #:drogue-utils)
  (:export #:run))
(in-package #:drogue)

(defparameter *running* nil )

(defun init-world ()
  (setf (get 'world 'ticks) 0))

(defun run (args)
  (declare (ignore args))
  (utils:with-init
    (init-world)
    (utils:log-to-file "i am working...")
    (run-ui *debug-ui*)))
