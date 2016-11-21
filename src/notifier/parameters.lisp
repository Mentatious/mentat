(in-package #:mentat-notifier)

(defparameter *db-name* nil
  "Mongo DB name")
(defparameter *xmpp-notifier-resource* nil
  "xmpp resource for notifier")

;; TODO: provide setting up timezone info on per-user basis
;; (HINT: use bot commands + persistent storage, same as `sortby', for example)
(defparameter *client-timezone* nil
  "Timezone to keep user timestamps within")

(defparameter *timezone-repository-system-path* "/usr/share/zoneinfo/")
