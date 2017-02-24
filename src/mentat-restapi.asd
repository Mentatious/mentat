(asdf:defsystem #:mentat-restapi
  :description "Core REST API server for various Mentat frontends"
  :author "Alex Ermolov <aaermolov@gmail.com>"
  ;TODO: provide license clause
  :version "0.1"
  :depends-on (#:mentat-util
               #:unix-opts
               #:cl-mongo
               #:clack
               #:lack-middleware-backtrace
               #:clack-handler-hunchentoot
               #:ningle
               #:flexi-streams
               #:cl-json
               #:swank)
  :serial t
  :pathname "restapi"
  :components ((:file "package")
               (:file "parameters")
               (:file "config")
               (:file "db/mongodb")
               (:file "restapi")
               ))
