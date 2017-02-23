(asdf:defsystem #:mentat-db
  :description "Transitional system definition during code reorganization"
  :author "Alex Ermolov <aaermolov@gmail.com>"
  :version "0.1"
  :depends-on (#:cl-mongo)
  :serial t
  :components ((:file "util/mongodb" )))
