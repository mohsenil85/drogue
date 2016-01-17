(defpackage #:drogue-ui
  (:nicknames ui)
  (:use #:cl
        #:utils
        #:swank-server)
  (:export :<ui>
           :<debug-ui>
           :*debug-ui*
           :<play-ui>
           :render-ui
           :define-ui-stack
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
(defclass <quit-ui> (<ui>) ())

(defclass <play-ui> (<ui>)
  (world ))

(defparameter *logo1* '("     _____        _____           _____          _____     ____   ____      ______   " " ___|\\    \\   ___|\\    \\     ____|\\    \\     ___|\\    \\   |    | |    | ___|\\     \\  " "|    |\\    \\ |    |\\    \\   /     /\\    \\   /    /\\    \\  |    | |    ||     \\     \\ " "|    | |    ||    | |    | /     /  \\    \\ |    |  |____| |    | |    ||     ,_____/|" "|    | |    ||    |/____/ |     |    |    ||    |    ____ |    | |    ||     \\--'\\_|/" "|    | |    ||    |\\    \\ |     |    |    ||    |   |    ||    | |    ||     /___/|  " "|    | |    ||    | |    ||\\     \\  /    /||    |   |_,  ||    | |    ||     \\____|\\ " "|____|/____/||____| |____|| \\_____\\/____/ ||\\ ___\\___/  /||\\___\\_|____||____ '     /|" "|    /    | ||    | |    | \\ |    ||    | /| |   /____ / || |    |    ||    /_____/ |" "|____|____|/ |____| |____|  \\|____||____|/  \\|___|    | /  \\|____|____||____|     | /" "                                                 |____|/                    |_____|/ "))

(defmethod render-ui ((ui <play-ui>))
  (print-logo *logo1*))


(defmethod render-ui ((ui <quit-ui>))
  (print-center "lol"))

(defun print-logo (logo)
  (loop
     for l in (reverse logo)
     with i = 0
     do
       (print-center l i)
       (decf i)))

(defmethod handle-input ((ui <ui>) input)
  (case input
    (#\newline (print-box 20 20 20 20))
    (#\q (setf (should-loop ui) nil ))))

(defparameter *play-ui* (make-instance '<play-ui>))
(defparameter *debug-ui* (make-instance '<debug-ui>))
(defparameter *quit-ui* (make-instance '<quit-ui>))
(defparameter *ui-stack* '())


(defun define-ui-stack () 
  (push *quit-ui* *ui-stack*)
  (push *debug-ui* *ui-stack*)
  (push *play-ui* *ui-stack*))

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

(defmethod handle-input ((ui <debug-ui>)  input )
  (utils:log-to-file (format nil "got a char ~a~% " input ))
  (case input
    (#\c (charms:clear-window (standard-window) :force-repaint t) )
    (#\q (setf (should-loop ui) nil) )
    (#\Q (setf utils:*running*  nil) )
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
  (setf (next-ui old) new)
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
