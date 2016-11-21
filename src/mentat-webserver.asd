(asdf:defsystem #:mentat-webserver
  :description "All-purpose webserver for mentat needs"
  :author "Alex Ermolov <aaermolov@gmail.com>"
  ;TODO: provide license clause
  :version "0.1"
  :depends-on (#:unix-opts
               #:cl-mongo
               #:local-time
               #:hunchentoot
               #:split-sequence
               #:alexandria
               #:swank)
  :serial t
  :components ((:file "webserver/package")
               (:file "webserver/parameters")
               (:file "util")
               (:file "db/mongodb")
               (:file "webserver/webserver")
               ))
