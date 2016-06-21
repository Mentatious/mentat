(asdf:defsystem #:mentat-notifier
  :description "Notifier for mentat, by means like xmpp, email, etc."
  :author "Alex Ermolov <aaermolov@gmail.com>"
  ;TODO: provide license clause
  :version "0.1"
  :depends-on (#:mentat
               #:unix-opts
               #:cl-mongo
               #:split-sequence
               #:alexandria
               #:local-time
               #:trivial-timers
               #:cl-cron
               #:bordeaux-threads
               #:uuid
               #:swank)
  :serial t
  :components ((:file "notifier/package")
               (:file "parameters")
               (:file "util")
               (:file "mongodb")
               (:file "notifier/notifier")
               ))
