(defpackage #:drogue
  (:use #:cl
        #:cl-charms
        #:drogue-ui
        #:drogue-utils)
  (:import-from #:swank
                #:set-package
                #:create-server
                #:*log-output*
                #:*log-events*)
  (:export #:main))
(in-package #:drogue)

(defparameter *running* nil )

(defun swank-init ()
  (swank:set-package 'drogue)
  (swank:create-server :port 9000 :style :spawn))

(defparameter *debug-ui* (make-instance 'drogue-ui:<ui>))
(defparameter *ui-stack* (cons *default-ui* '()))
(defun init-world ()
  (setf (get 'world 'ticks) 0))

(defmethod handle-input (<ui> input)
  :before
  (if (utils:is-resize input)
      (utils:handle-resize)
      (call-next-method  )))

(defmethod should-loop (<ui>)
  t)

(defun run-ui (ui)
  (render-ui ui)
  (loop
     while (should-loop ui)
     for input = (charms:get-char (standard-window) :ignore-error t)
     do (handle-input ui input)))


(defun main (args)
  (declare (ignore args))
  (swank-init)
  (utils:with-init
    (init-world)
    (utils:log-to-file "i am working...")
    (run-ui *debug-ui*)
    ))
