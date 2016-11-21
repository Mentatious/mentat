(in-package #:mentat-xmpp)

(defparameter *db-name* nil
  "Mongo DB name")
(defparameter *entries-collection-prefix* nil
  "Mongo collection name prefix")
(defparameter *xmpp-login* nil
  "xmpp login")
(defparameter *xmpp-password* nil
  "xmpp password")
(defparameter *xmpp-server* nil
  "xmpp server")
(defparameter *xmpp-resource* nil
  "xmpp resource")
;; TODO: provide setting up timezone info on per-user basis
;; (HINT: use bot commands + persistent storage, same as `sortby', for example)
(defparameter *client-timezone* nil
  "Timezone to keep user timestamps within")

(defparameter *timezone-repository-system-path* "/usr/share/zoneinfo/")
