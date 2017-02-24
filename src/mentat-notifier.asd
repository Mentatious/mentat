(asdf:defsystem #:mentat-notifier
  :description "Notifier for mentat, by means like xmpp, email, etc."
  :author "Alex Ermolov <aaermolov@gmail.com>"
  ;TODO: provide license clause
  :version "0.1"
  :depends-on (#:mentat-util
               #:mentat-db
               #:mentat-xmpp-engine
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
  :pathname "notifier"
  :components ((:file "package")
               (:file "parameters")
               (:file "notifier")
               ))
