(defpackage #:drogue-utils
  (:nicknames utils)
  (:use #:cl
        #:cl-charms)
  (:export :log-to-file
           :handle-resize
           :is-resize))

(defpackage #:drogue-ui
  (:nicknames ui)
  (:use #:cl
        #:utils)
  (:export :<ui>
           :render-ui
           :handle-input)
  (:import-from #:cl-charms
                #:standard-window
                #:write-string-at-point))

(defpackage #:drogue
  (:use #:cl
        #:cl-charms
        #:drogue-ui
        #:drogue-utils)
  (:import-from #:swank
                #:set-package
                #:create-server
                #:*log-output*
                #:*log-events*)
  (:export #:main))
