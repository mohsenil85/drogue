;;;; drogue.asd

(asdf:defsystem #:drogue
  :description "this is a drogue"
  :author "lmohseni <mohsenil85@gmail.com>"
  :license "Specify license here"
  :depends-on (#:cl-termbox
               #:swank)
  :serial t
  :components ((:file "drogue")))
