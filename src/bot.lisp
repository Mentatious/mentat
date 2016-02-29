(in-package #:mentat)

(defparameter *db-name* nil
  "Mongo DB name")
(defparameter *entries-collection-name* nil
  "Mongo collection name")
(defparameter *xmpp-login* nil
  "xmpp login")
(defparameter *xmpp-password* nil
  "xmpp password")
(defparameter *xmpp-server* nil
  "xmpp server")
(defparameter *xmpp-resource* nil
  "xmpp resource")

(defparameter *errors* nil)
(defparameter *connection* nil
  "XMPP connection")

#+sbcl
(defun main-xmpp-bot ()
  (with-interactive-interrupt
    (swank-loader:init)
    (swank:create-server
     :port 4006
     :style swank:*communication-style*
     :dont-close t)
    (setq swank:*use-dedicated-output-stream* nil)
    (load-config)
    (init-storage)
    (connect)))

#+sbcl
(defun save-image-xmpp-bot ()
  (sb-ext:save-lisp-and-die "mentat-xmpp-bot"
                            :compression t
                            :executable t
                            :toplevel #'main-xmpp-bot))

(defun starts-with-nick (nick msg)
  (check-type nick string)
  (check-type msg string)
  (multiple-value-bind (starts rest)
      (alexandria:starts-with-subseq nick (trim msg)
                                     :return-suffix t)
    (when starts
      (let ((rest2 (trim rest)))
        (case (aref rest2 0)
          ((#\: #\,) (trim (subseq rest2 1)))
          (t rest2))))))

(defun reply-message (body)
  (optima:match body
    ((optima.ppcre:ppcre "[Aa]dd( todo| someday| waiting)*( \#[AaBbCc])*( .*)$" status priority entry)
     (add-entry entry
                :status status
                :priority priority)
     "Added.")
    ((optima.ppcre:ppcre "[Ll]istall$")
     (let ((entries (list-entries))
           (reply "No entries found."))
       (when (> (length entries) 0)
         (format nil "entries:~{~%~a~}" entries))))
    ((optima.ppcre:ppcre "[Ll]istallraw$")
     (let ((entries (list-entries t))
           (reply "No entries found."))
       (when (> (length entries) 0)
         (format nil "entries:~{~%~a~}" entries))))
    ((optima.ppcre:ppcre "[Ll]istallorg$")
     (let ((entries (list-entries t)) ;FIXME: make both 'raw' and 'org' working
           (reply "No entries found."))
       (when (> (length entries) 0)
         (format nil "entries:~{~%~a~}" entries))))
    ((optima.ppcre:ppcre "[Cc]learall")
     (clear-entries)
     "DB cleared.")
    ((optima.ppcre:ppcre "[Dd]rop (.*)$" index) ;TODO: initially parse as integer
     (let ((deleted (drop-entry (parse-integer index))))
       (format nil "Dropped '~a'" deleted)))
    (_ "Unknown query.")))

(defclass user-error ()
  ((object
    :accessor object
    :initarg :object)
   (catched-at
    :accessor catched-at
    :initarg :catched-at
    :initform (get-universal-time))))

(defun make-user-error (obj)
  (make-instance 'user-error :object obj))
