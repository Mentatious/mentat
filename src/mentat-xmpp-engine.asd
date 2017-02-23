(asdf:defsystem #:mentat-xmpp-engine
  :description "Transitional system definition during code reorganization"
  :author "Alex Ermolov <aaermolov@gmail.com>"
  :version "0.1"
  :depends-on (#:cl-xmpp-tls
               #:cl-xmpp-sasl)
  :serial t
  :components ((:file "util/xmpp-engine" )))
