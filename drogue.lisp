;;;; drogue.lisp

(in-package #:drogue)

(defclass tile () (glyph  color-fg color-bg visible))

(defclass <entity> ()
  (position tile))

(defclass <player> (<entity>)
  ((tile :initform (make-instance 'tile :glyph #\@))))

(defclass <ui-element> ()
  (
   (height :accessor height
           :initarg height
           :initform 4
           :type number)
   (width :initarg width
          :accessor width
          :initform 4
          :type number)
   (start-x :initarg start-x
            :accessor start-x
            :initform 0
            :type number)
   (start-y :initarg start-y
            :accessor start-y
            :initform 0
            :type number)
   (data :initarg data
         :accessor get-data
         :initform (make-hash-table)
         :type hash-table)
   ))
(defmethod render-ui-element (<ui-element>)
  (let ((width (width <ui-element>))
        (height (height <ui-element>)))
    (loop for i below width do
         (loop for j below height do
              (render-string "#" :x i :y j)))))
(defclass <world-map> (<ui-element>) ())

(defun create-ui ()
  (defparameter test-rect (make-instance '<ui-element>
                                         :width 3
                                         :height 5
                                         :start-x 40
                                         :start-y 8)))

;;this is just in case
(defmethod render-ui :around ((<ui> <play>) game )

  (loop for i below 10 do
       (loop for j below 13 do
            (render-string "#" :x i :y j)))
  )

(defclass <game> () ((current-ui :initform (make-instance '<start>)
                                 :initarg ui
                                 :type <ui>
                                 :accessor ui)
                     (ticks :initform 0
                            :type number
                            :accessor ticks)
                     (input :accessor input)
                     (world-map :type <world-map>
                                :accessor world-map)
                     ))

(defparameter *game* (make-instance '<game>))

;;ui.lisp talks to these as well...  make sure that the "correct"
;;way to swtich a ui is (switch-ui *inventory*)
(defparameter *start* (make-instance '<start>))
(defparameter *play* (make-instance '<play>))
(defparameter *inventory* (make-instance '<inventory>))
(defparameter *quit* (make-instance '<quit>))




;;utils

;; (defun get-slots (object)
;;   (format t "~%~A~%"
;;           (mapcar #'sb-pcl:slot-definition-name
;;                   (sb-pcl:class-slots (class-of object)))))

                                        ;(get-slots *game*)

(defun draw-map ()
  (render-string "foobars" :x 0 :y 0 ))


(defun create-map (game)
  (setf (world-map game) (make-instance '<world-map>
                                        :width 50
                                        :heigh 20)))

(defun exit-game ()
  (sb-ext:exit))

(defun switch-ui (ui)
  (setf (ui *game*) ui)
  (render-ui (ui *game*) *game*))

(defmethod render-ui :before (<ui> game)
  (clear-window *standard-window*)
  )

(defun render-string (string &key (x 0) (y 0))
  (write-string-at-point *standard-window* string x y ))

(defun get-input (window)
  (get-char window :ignore-error t)
  )

(defun init ()
  (cl-charms:disable-echoing)
                                        ;  (charms:enable-raw-input :interpret-control-characters t)
                                        ;  (charms:enable-extra-keys *standard-window*)
                                        ;  (charms:enable-non-blocking-mode charms:*standard-window*)
  (switch-ui *start*)
  )


(defun update-game ()
  (incf (ticks *game*)))


(defun run-game (input game)
  ;;super generic both of these
  ;;here, we'd have (set! state or something)
  (render-ui (ui game) game)
  ;;render-ui will only read the game
  (process-input (ui game) input game)
  (update-game))


(defun main (args)
  (declare (ignore args))
  (cl-charms:with-curses ()
    (init)
    (loop
       :named main-loop
       :for input := (get-input *standard-window*)
       :do
       (run-game input *game*))))
