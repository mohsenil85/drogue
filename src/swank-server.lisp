(in-package :cl-user)
(defpackage #:swank-server
  (:use #:cl
        #:cl-charms)
  (:import-from #:swank
                #:set-package
                #:create-server
                #:*log-output*
                #:*log-events*)
  (:export :swank-listen
           :is-swank-running
           :is-swank-connected
           :swank-kill))

(in-package #:swank-server)


(defun swank-init ()
  (swank:set-package 'drogue)
  (swank:create-server :port 9000 :style :spawn :dont-close t))

(defparameter listening nil )
(defparameter connected nil)

(defun on-client-connect (conn)
  (declare (ignore conn))
  (charms:clear-window (charms:standard-window) :force-repaint t)
  (setf connected t))

(defun swank-listen ()
  (unless listening
    (push 'on-client-connect
          swank::*new-connection-hook*)
    (swank-init)
    (setf listening t)))


(defun swank-kill ()
  (when listening
    (swank:stop-server 9000 )
    (setf listening nil)
    (setf connected nil)))

(defun is-swank-running ()
  listening)
(defun is-swank-connected ()
  connected)


