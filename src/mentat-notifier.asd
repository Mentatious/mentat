(asdf:defsystem #:mentat-notifier
  :description "Notifier for mentat, by means like xmpp, email, etc."
  :author "Alex Ermolov <aaermolov@gmail.com>"
  ;TODO: provide license clause
  :version "0.1"
  :depends-on (#:mentat-xmpp
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
               (:file "notifier/parameters")
               (:file "util")
               (:file "bot/xmpp-engine")
               (:file "db/mongodb")
               (:file "notifier/notifier")
               ))
