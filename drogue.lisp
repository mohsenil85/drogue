;;;; drogue.lisp

(in-package #:drogue)

(defclass <game> () ((current-ui :initform (make-instance '<start>)
                                 :initarg ui
                                 :type <ui>
                                 :accessor ui)
                     (ticks :initform 0
                            :type number
                            :accessor ticks)
                     (input :accessor input)))

(defparameter *game* (make-instance '<game>))

;;ui.lisp talks to these as well...  make sure that the "correct"
;;way to swtich a ui is (switch-ui *inventory*)
(defparameter *start* (make-instance '<start>))
(defparameter *play* (make-instance '<play>))
(defparameter *inventory* (make-instance '<inventory>))
(defparameter *quit* (make-instance '<quit>))

;;utils

(defun get-slots (object)
  (format t "~%~A~%"
          (mapcar #'sb-pcl:slot-definition-name
                  (sb-pcl:class-slots (class-of object)))))

;(get-slots *game*)


(defun exit-game ()
  (sb-ext:exit))

(defun switch-ui (ui)
  (setf (ui *game*) ui)
  (render-ui *game*))

(defmethod render-ui :around (<ui> game)
  (clear-window *standard-window*)
  )

(defun render-string (string &key (x 0) (y 0))
  (write-string-at-point *standard-window* string x y ))

(defun get-input()
  (let ((c (charms/ll:wgetch *standard-window*)))
    (setf (input *game*) c)
    c
    ))
 ; (get-char *standard-window* :ignore-error t)

(defun init ()
  (cl-charms:disable-echoing)
  (charms:enable-raw-input :interpret-control-characters nil)
  (charms:enable-extra-keys *standard-window*)
;  (charms:enable-non-blocking-mode charms:*standard-window*)
  (setf *current-ui* *start*)
  (draw *current-ui*))


(defun update-game ()
  (incf (ticks *game*)))


(defun run-game (input window game)
  ;;super generic both of these
  ;;here, we'd have (set! state or something)
  (render-ui (ui game))
  ;;render-ui will only read the game
  (process-input (ui game) input game)
  (update-game))


(defun main (args)
  (declare (ignore args))
  (cl-charms:with-curses ()
    (init)
    (loop
       :named main-loop
       :for input := (get-input)
       :do
       (run-game input *standard-window* *game*))))
