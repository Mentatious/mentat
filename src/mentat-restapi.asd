(asdf:defsystem #:mentat-restapi
  :description "Core REST API server for various Mentat frontends"
  :author "Alex Ermolov <aaermolov@gmail.com>"
  ;TODO: provide license clause
  :version "0.1"
  :depends-on (#:mentat
               #:unix-opts
               #:cl-mongo
               #:clack
               #:lack-middleware-backtrace
               #:clack-handler-hunchentoot
               #:ningle
               #:flexi-streams
               #:swank)
  :serial t
  :components ((:file "restapi/package")
               (:file "restapi/parameters")
               (:file "util")
               (:file "restapi/config")
               (:file "restapi/db/mongodb")
               (:file "restapi/restapi")
               ))
