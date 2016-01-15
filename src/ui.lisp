(defpackage #:drogue-ui
  (:nicknames ui)
  (:use #:cl
        #:utils)
  (:export :<ui>
           :render-ui
           :handle-input)
  (:import-from #:cl-charms
                #:standard-window
                #:write-string-at-point))

(in-package #:drogue-ui)

(defclass <ui> () ())

(defgeneric render-ui (<ui>))
(defgeneric handle-input  (<ui> input))
(defgeneric next-ui (<ui>))

(defmethod render-ui (<ui>)
  (charms:write-string-at-point (standard-window) "now this" 10 10))

(defmethod handle-input (<ui> input )
  (utils:log-to-file (format nil "got a char ~a~% " input ))
  (case input
    (#\c (write-string-at-point (standard-window) "output" 12 7 ))
    (#\newline (write-string-at-point (standard-window) "nl" 12 7 ))
    (#\r (ui:render-ui <ui>))
    (otherwise (utils:log-to-file "got here")))
  )


;; (defmacro defui (name &key handle-input-fn render-ui-fn next-ui-fn ui-stack )
;;   `(defclass ,name (<ui>) ())
;;   `(defmethod handle-input (<ui> ,name)
;;      ,handle-input-fn)
;;   `(defmethod render-ui (<ui> ,name)
;;      ,render-ui-fn)
;;   `(defmethod next-ui (<ui> ,name)
;;      ,next-ui-fn)
;;   `(push (make-instance ',name) ,ui-stack))


;; (defparameter *uis* nil)

;; (defui start
;;     :handle-input-fn '(format t "i am handle input")
;;     :render-ui-fn '(format t "i am render")
;;     :next-ui-fn '(format t "i am next")
;;     :ui-stack *uis*)



