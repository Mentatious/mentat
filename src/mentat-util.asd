(asdf:defsystem #:mentat-util
  :description "Transitional system definition during code reorganization"
  :author "Alex Ermolov <aaermolov@gmail.com>"
  :version "0.1"
  :depends-on (#:local-time
               #:alexandria
               #:usocket)
  :serial t
  :components ((:file "util/util" )))
