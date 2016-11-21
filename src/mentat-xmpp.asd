(asdf:defsystem #:mentat-xmpp
  :description "Org-like assistant bot for personal information management"
  :author "Alex Ermolov <aaermolov@gmail.com>"
  ;TODO: provide license clause
  :version "0.1"
  :depends-on (#:unix-opts
               #:cl-mongo
               #:cl-xmpp-tls
               #:cl-xmpp-sasl
               #:split-sequence
               #:alexandria
               #:cxml
               #:cl-lex
               #:yacc
               #:local-time
               #:uuid
               #:swank)
  :serial t
  :components ((:file "bot/package")
               (:file "bot/parameters")
               (:file "util")
               (:file "db/mongodb" )
               (:file "bot/xmpp-engine")
               (:file "bot/bot")))
