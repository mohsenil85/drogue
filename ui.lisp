;;; ui.lisp

(in-package #:drogue)


(defclass <ui> ()
  ((message :type string
            :initarg :message
            :initform "this is the defualt message"))
  (:documentation
   "a parent ui class that holds data about the active ui.
they are held in a global stack called *ui-stack*.
the message slot can be rendered to put diagnostic info to the screen"))

(defclass <play> (<ui>)
  ((message :initform "press any key to play the game, or press q to quit"
            :type string
            :reader message)))

(defclass <start> (<ui>)
  ((message :initform "this is the start ui.  press 'e' to start playing"
            :type string
            :reader message)))
(defclass <inventory> (<ui>)
  ((message :initform "this is the inventory view"
            :type string
            :reader message))
  (:documentation "should the inventory be a part of 'world',
or should it be a field in here?
storing state in here seems like a bad idea" ))
(defclass <quit> (<ui>)
  ((message :initform "press any key to exit"
            :type string
            :reader message)))

;;drawing
(defgeneric draw (<ui>)
  (:documentation "draw the current ui (whatever it's type is)"))

(defmethod draw (<ui>)
  "the defualt way to draw a ui is to render it's message slot to the screen..
eventually this could take params like 'is-bordered' or something"
  (render-string (message <ui>)))
(defmethod draw ((<ui> <play>))
  "i guess all of the views are going to draw various aspects of the Game.
the play screen will probably end up being interested in map"
  (render-string "i render the map" :x 7 :y 8 ))

(defgeneric handle-input (<ui> input)
  (:documentation "called once per loop. switch-case on the user's input
 appropriately.  (eg, pressing i on the play screen is different than
 pressing i on the inventory screen)"))
(defmethod handle-input ((<ui> <start>) input)
  (when (eq input #\e)
    (switch-ui *play*)))

(defmethod handle-input ((<ui> <play>) input)
  (case input
    ((#\q) (switch-ui *quit*))
    ((#\i) (switch-ui *inventory*))
    (otherwise (render-string
                (format nil "the time is ~A" *time*)
                :x (random 10) :y (random 10)))))

(defmethod handle-input ((<ui> <inventory>) input)
 ( case input
    ((#\^[) (render-string "you look in your inventory" :x 50 :y 4))
    ((#\q) (switch-ui *play*))
    (otherwise (render-string (format nil "~A" input)))))

(defmethod handle-input ((<ui> <quit>) input)
  (when input
    (sb-ext:exit)))

(defun switch-ui (ui)
  (clear-window *standard-window*)
  (setf *current-ui* ui)
  (draw *current-ui*))

(defun render-string (string &key (x 0) (y 0))
  (write-string-at-point *standard-window* string x y ))

(defun get-input()
  (get-char *standard-window* :ignore-error t))
