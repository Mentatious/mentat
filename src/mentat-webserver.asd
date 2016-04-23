(asdf:defsystem #:mentat-webserver
  :description "All-purpose webserver for mentat needs"
  :author "Alex Ermolov <aaermolov@gmail.com>"
  ;TODO: provide license clause
  :version "0.1"
  :depends-on (#:mentat
               #:unix-opts
               #:cl-mongo
               #:hunchentoot
               #:split-sequence
               #:alexandria
               #:swank)
  :serial t
  :components ((:file "webserver/package")
               (:file "util")
               (:file "mongodb")
               (:file "webserver/webserver")
               ))
