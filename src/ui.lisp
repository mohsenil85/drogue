(defpackage #:drogue-ui
  (:nicknames ui)
  (:use #:cl
        #:utils
        #:swank-server)
  (:export :<ui>
           :<debug-ui>
           :*debug-ui*
           :<logo-ui>
           :render-ui
           :handle-input
           :run-game
           :run-ui)
  (:import-from #:cl-charms
                #:standard-window
                #:write-string-at-point))

(in-package #:drogue-ui)

(defclass <ui> ()
  ((should-loop :accessor should-loop :initform t :type :bool)))


(defclass <debug-ui> (<ui>) ())
(defclass <press-any-key> (<ui>) ())
(defclass <quit-ui> (<press-any-key>) ())

(defclass <sub-ui> (<press-any-key>) ())

(defclass <logo-ui> (<press-any-key>)
  (world ))

(defclass <middle-ui> (<press-any-key>)
  (world ))

(defmethod render-ui ((ui <middle-ui>))
  (print-center "i am the middle ui"))

(defmethod render-ui ((ui <logo-ui>))
  (print-logo))

(defmethod render-ui ((ui <sub-ui>))
  (print-center "i am the sub ui, press any key to exit"))

(defmethod handle-input :before ((ui <press-any-key>) input)
  (log-to-file "handle-input after called")
  (case input
    (t (pop-ui-stack))))

(defmethod render-ui ((ui <quit-ui>))
  (print-center "lol"))


(defmethod handle-input ((ui <ui>) input)
  (case input
    (#\newline (print-box 20 20 20 20))
    (#\c (setf *running* nil))
    (#\q (setf (should-loop ui) nil ))))

(defparameter *logo-ui* (make-instance '<logo-ui>))
(defparameter *debug-ui* (make-instance '<debug-ui>))
(defparameter *middle-ui* (make-instance '<middle-ui>))
(defparameter *quit-ui* (make-instance '<quit-ui>))
(defparameter *ui-stack* '())


(defgeneric render-ui (<ui>))
(defgeneric handle-input  (<ui> input))

(defmethod render-ui :after (<ui>)
  (charms:refresh-window (standard-window)))

(defmethod render-ui (<debug-ui>)
  (print-center "welcome to the debug ui" -10)
  (print-center "q to quit" -5)
  (print-center "l to launch swank" -3)
  (print-center "k to kill swank" -2)
  (print-center "p to play ui" -1)
  (print-center "c to clear screen" 0)
  (print-center "enter to do a thing" 5)
  (print-center (format nil "debug output: height: ~a width: ~a" *height* *width*) 8)
  (print-center (format nil "swank ~a" (swank-server:is-swank-running)) 10)
  (print-center (format nil "connected ~a" (swank-server:is-swank-connected)) 12))

(defun print-test ()
  (write-string-at-point
   (standard-window)
   "haha" (random *width*) (random *height*)))


(defun define-ui-stack ()
  (setf *running* t)
  (push *quit-ui* *ui-stack*)
  (push *middle-ui* *ui-stack*)
  (push *logo-ui* *ui-stack*))

(defmethod handle-input ((ui <debug-ui>)  input )
  (utils:log-to-file (format nil "got a char ~a~% " input ))
  (case input
    (#\c (charms:clear-window (standard-window) :force-repaint t) )
    (#\q (setf (should-loop ui) nil) )
    (#\Q (setf utils:*running*  nil) )
    (#\S (setf utils:*running*  t) )
    (#\l (swank-server:swank-listen) )
    (#\p (define-ui-stack) )
    (#\o (run-game) )
    (#\k (swank-server:swank-kill) )
    (#\r (ui:render-ui ui))
    (otherwise (utils:log-to-file "got here to end of input"))))


(defmethod handle-input :before (<ui> input)
  (charms:clear-window (standard-window) :force-repaint t)
  (if (utils:is-resize input)
      (utils:handle-resize)))


(defmethod switch-ui ((old <ui>) (new <ui>) )
  (setf (should-loop old) nil)
  (setf (should-loop new) t))

(defmethod run-ui :after ((ui <ui>))
  (charms:clear-window (standard-window) :force-repaint t))


(defun pop-ui-stack ()
  (pop *ui-stack*))

(defun run-game ()
  (mapcar #'run-ui *ui-stack*))

(defmethod run-ui ((ui <ui>))
  (render-ui ui)
  (loop
     while (should-loop ui)
     for input = (charms:get-char (standard-window) :ignore-error t)
     do
       (handle-input ui input)
       (render-ui ui)
       (log-to-file "end loop")
     finally 
       (pop-ui-stack )
       ))
