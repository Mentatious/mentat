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
(defun main ()
  (with-interactive-interrupt
    (load-config)
    (init-storage)
    (connect)))

#+sbcl
(defun save-image ()
  (sb-ext:save-lisp-and-die "mentat-xmpp-bot"
                            :compression t
                            :executable t
                            :toplevel #'main))

(defun process-message (connection message)
  (unless (search (concatenate 'string "/" (xmpp:username connection))
                  (xmpp:from message))
    (format xmpp:*debug-stream* "~&~a msg ~a from ~a to ~a~%"
            (xmpp::type- message)
            (xmpp:body message)
            (xmpp:from message)
            (xmpp:to message))
    (when (xmpp:body message)
      (let ((msg-type (xmpp::type- message)))
        (cond
          ((equal msg-type "chat") (process-chat-message connection message))
          ((equal msg-type "groupchat")
           (process-groupchat-message connection message)))
        ))))

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

(defun process-chat-message (connection message)
  (unless (process-personal connection message)
    (reply-chat connection (xmpp:from message)
                "Unknown query.."
                (xmpp::type- message))))

(defun process-personal (connection message)
  (let ((body (xmpp:body message)))
    (optima:match body
      ((optima.ppcre:ppcre "[Aa]dd( todo| someday| waiting)*( \#[AaBbCc])*( .*)$" status priority entry)
       (add-entry entry
                  :status status
                  :priority priority)
       (reply-chat connection (xmpp:from message)
                   (format nil "Added.")
                   (xmpp::type- message)))
      ((optima.ppcre:ppcre "[Ll]istall")
       (let ((entries (list-entries))
             (reply "No entries found."))
         (when (> (length entries) 0)
           (setf reply (format nil "entries:~{~%~a~}" entries)))
         (reply-chat connection (xmpp:from message)
                     reply
                     (xmpp::type- message))))
      ((optima.ppcre:ppcre "[Cc]learall")
       (clear-entries)
       (reply-chat connection (xmpp:from message)
                   (format nil "DB cleared.")
                   (xmpp::type- message)))
      ((optima.ppcre:ppcre "[Dd]rop (.*)$" index) ;TODO: initially parse as integer
       (let ((deleted (drop-entry (parse-integer index))))
         (reply-chat connection (xmpp:from message)
                     (format nil "Dropped '~a'" deleted)
                     (xmpp::type- message)))))))

(defun process-groupchat-message (connection message)
  (let ((starts (starts-with-nick (xmpp:username connection)
                                  (xmpp:body message))))
    (when starts
      (progn
        (setf (xmpp:body message) starts)
        (unless (process-personal connection message)
          (let* ((pos (position #\/ (xmpp:from message)))
                 (to (if pos
                         (subseq (xmpp:from message) (1+ pos))
                         (xmpp:from message))))
            (reply-chat connection (xmpp:from message)
                        (format nil "~a: Unknown query." to)
                        (xmpp::type- message)
                        )))))))

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

(defun callback-with-restart (&rest args)
  (restart-case
      (handler-case (apply #'xmpp::default-stanza-callback args)
        (error (err) (push (make-user-error err) *errors*)))
    (skip-stanza () '(ignored))))

(defun reply-chat (connection to reply kind &key highlight xhtml)
  (check-type reply (or string null))
  (check-type highlight (or string null))
  (format xmpp:*debug-stream* "~&[reply-chat] kind: ~a" kind)
  (if (string-equal kind "groupchat")
      (let* ((pos (position #\/ to))
             (to (if pos (subseq to 0 pos) to))
             (reply (if highlight (format nil "~a: ~a" highlight reply) reply)))
        (xmpp:message connection to reply :type :groupchat :xhtml-body xhtml))
      (xmpp:message connection to reply :type :chat :xhtml-body xhtml)))
