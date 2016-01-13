(in-package #:drogue-ui)

(defclass <ui> () ())
(defmethod render-ui (<ui>)
  (charms:write-string-at-point (standard-window) "now this" 10 10))

(defmethod handle-input (<ui> input)
  (utils:log-to-file (format nil "got a char ~a~% " input ))
  (case input
    (#\c (write-string-at-point (standard-window) "output" 12 7 ))
    (#\newline (write-string-at-point (standard-window) "nl" 12 7 ))
    (#\r (ui:render-ui <ui>))
    (otherwise (utils:log-to-file "got here")))
  )

