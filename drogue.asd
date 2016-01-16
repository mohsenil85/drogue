;;;; drogue.asd

(in-package :cl-user)
(defpackage drogue-asd
  (:use :cl :asdf))
(in-package :drogue-asd)

(defsystem #:drogue
  :description "this is a drogue"
  :author "lmohseni <mohsenil85@gmail.com>"
  :license "GPL"
  :depends-on (#:cl-charms
               #:swank)
  :serial t
  :components ((:module "src"
                        :components
                        ((:file "swank-server")
                         (:file "utils")
                         (:file "ui")
                         (:file "drogue"))))
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op drogue-test))))

