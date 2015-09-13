;;;; drogue.lisp

(in-package #:drogue)


;;all these globals are where we store state..  so attatch to a game object?
(defparameter *current-ui* nil)
(defparameter *time* 0)
;;actual instances of uis...  so maybe it's ok to mutate them?
(defparameter *start* (make-instance '<start>))
(defparameter *play* (make-instance '<play>))
(defparameter *inventory* (make-instance '<inventory>))
(defparameter *quit* (make-instance '<quit>))


(defun init ()
  (cl-charms:disable-echoing)
  (charms:enable-raw-input :interpret-control-characters nil)
  ;;unlink the loop from the once-per-input time
  ;(charms:enable-non-blocking-mode charms:*standard-window*)
  (setf *current-ui* *start*)
  (draw *current-ui*))


(defun update-game ()
  (incf *time*))

(defun run-game (input window ui)
  (clear-window window)
  ;;super generic both of these
  ;;here, we'd have (set! state or something)
  (draw ui)
  (handle-input ui input)
  (update-game))


(defun main (args)
  (declare (ignore args))
  (cl-charms:with-curses ()
    (init)
    (loop
       :named main-loop
       :for input := (get-input)
       :do
       (run-game input *standard-window* *current-ui*))))
