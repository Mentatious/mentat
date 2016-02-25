(asdf:defsystem #:mentat
  :description "Org-like assistant bot for personal information management"
  :author "Alex Ermolov <aaermolov@gmail.com>"
  ;TODO: provide license clause
  :version "0.1"
  :depends-on (#:unix-opts
               #:cl-mongo
               #:cl-xmpp-tls
               #:cl-xmpp-sasl
               #:optima
               #:optima.ppcre
               #:split-sequence
               #:alexandria
               #:cxml)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "mongodb" )
               (:file "xmpp-engine")
               (:file "bot")))
