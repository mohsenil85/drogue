;;;; drogue.lisp
(defpackage #:drogue
  (:use #:cl
        #:cl-charms
        #:omens)
  (:export :main))

(in-package #:drogue)

;;min window size ensurer
;;defui
;;;keymaps
;;;callbacks
;;;;start with inventory ui
;;;;also world state object
;;;need printing fns like print-center

(defstruct world width height ticks)
(defparameter *world* (make-world :width 0 :height 0 :ticks 0))

(defun init-world (world)
  (multiple-value-bind (w h) (window-dimensions (standard-window))
    (setf (world-ticks world) 0)
    (setf (world-height world) h)
    (setf (world-width world) w)))

(defun write-at-center (string)
  (let* ((world *world*)
         (width (world-width world))
         (height (world-height world))
         (center-x (floor width 2))
         (center-y (floor height 2))
         (normal-x (- center-x (floor (length string) 2) )))
    (write-string-at-point (standard-window)
                           string
                           normal-x
                           center-y)))

(defstruct ui 
  render-fn keymap cursor-visible next-ui)


(defun debugger (h w)
  (write-at-center (format nil "h: ~a; w: ~a" h w)))


;; (defun defui (world)
;;   (let ((ui (make-ui)))
;;     (setf (ui-render-fn ui))))


(defun run-ui (world)
  (render-ui (world-ui world)))


(defun game-loop (world)
  (with-curses ()
    (disable-echoing)
    (enable-raw-input :interpret-control-characters t)
    (enable-non-blocking-mode (standard-window))
    (charms/ll:curs-set 0)
    (init-world world)
    (ensure-screen-size )
    (loop
       :named driver-loop
       :for c := (get-char (standard-window)
                           :ignore-error t)
       :do (progn
             (refresh-window (standard-window))
             (incf (world-ticks world))
             (write-at-center "you awake in a quiet place...")
             ))))


(defun main (args)
  (declare (ignore args))
  (game-loop *world*))


;; (defui start-screen (world)
;;   :ui ;;output
;;   :keymap ;;input
;;   :cursor
;;   :next ;;that way q in the keymap always can be just go-next
;;   )
