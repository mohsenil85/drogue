;;;; drogue.asd

(asdf:defsystem #:drogue
  :description "this is a drogue"
  :author "lmohseni <mohsenil85@gmail.com>"
  :license "Specify license here"
  :depends-on (#:cl-charms)
  :serial t
  :components ((:file "package")
               (:file "drogue")))
