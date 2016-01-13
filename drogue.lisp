(in-package #:drogue)

(defparameter *running* nil )

(defun swank-init ()
  (swank:set-package 'drogue)
  (swank:create-server :port 9000 :dont-close t))

(defparameter *default-ui* (make-instance 'drogue-ui:<ui>))
(defparameter *ui-stack* (cons *default-ui* '()))

(defun print-box (startx starty w h)
  (let ((win (standard-window)))
    (loop for i from startx below w do
         (loop for j from starty below h do
              (charms:write-string-at-point
               win
               "*" i j)))))


(defun main (args)
  (declare (ignore args))
  (swank-init)
  (charms:with-curses ()
    (setf *running* t)
    (charms/ll:curs-set 0)
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (utils:log-to-file "i am working...")
    (loop
       while *running*
       for win = (standard-window)
       for input = (charms:get-char win
                    :ignore-error t)
         for ui = (car *ui-stack*)

       do
         (progn
           (ui:render-ui ui)
           (charms:refresh-window win )
           (if (utils:is-resize input)
               (utils:handle-resize)
               (ui:handle-input ui input))
           (utils:log-to-file "end of loop"))
         )))
