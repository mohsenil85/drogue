(in-package :cl-user)
(defpackage drogue-test-asd
  (:use :cl :asdf))
(in-package :drogue-test-asd)

(defsystem drogue-test
  :depends-on (:drogue
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "drogue"))))
  :description "Test system for fooproj"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
