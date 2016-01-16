(in-package :cl-user)
(defpackage #:swank-server
  (:use #:cl)
  (:import-from #:swank
                #:set-package
                #:create-server
                #:*log-output*
                #:*log-events*)
  (:export :swank-listen
           :is-swank-running
           :is-swank-connected
           :swank-kill))


(defun swank-init ()
  (log-to-file "listening on 9000...")
  (swank:set-package 'drogue)
  (swank:create-server :port 9000 :style :spawn :dont-close t))

(let ((listening nil )
      (connected nil))

  (defun on-client-connect (conn)
    (log-to-file (format nil "~A" conn))
    (charms:clear-window (standard-window) :force-repaint t)
    (setf connected t))

  (defun swank-listen ()
    (unless listening
      (log-to-file (format nil"swank-running ~a" listening))
      (push 'on-client-connect
            swank::*new-connection-hook*)
      (swank-init)
      (setf listening t)))

  
  (defun swank-kill ()
    (when listening
      (log-to-file (format nil"swank-running ~a" listening))
      (swank:stop-server 9000 )
      (setf listening nil)
      (setf connected nil)))

  (defun is-swank-running ()
    listening)
  (defun is-swank-connected ()
    connected)

  )
