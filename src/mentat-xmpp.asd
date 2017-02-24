(asdf:defsystem #:mentat-xmpp
  :description "Org-like assistant bot for personal information management"
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
               #:cxml
               #:cl-lex
               #:yacc
               #:local-time
               #:uuid
               #:swank)
  :serial t
  :pathname "bot"
  :components ((:file "package")
               (:file "parameters")
               (:file "bot")))
