(defpackage #:drogue
  (:use #:cl
        #:cl-charms
        #:drogue-ui
        #:drogue-utils)
  (:export #:run))
(in-package #:drogue)


(defun init-world ()
  (setf (get 'world 'ticks) 0))

(defun run (args)
  (declare (ignore args))
  (utils:with-init
    (init-world)
    (utils:log-to-file "i am working...")
    (define-ui-stack)
    (utils:log-to-file "ui stack is defined")
    (when utils:*running*
      (run-game))))
