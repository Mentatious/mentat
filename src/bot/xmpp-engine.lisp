(in-package #:mentat)

(defmethod xmpp:message ((connection xmpp:connection)
                         to body &key id (type :chat) xhtml-body)
  (xmpp::with-xml-output (connection)
    (cxml:with-element "message"
      (cxml:attribute "to" to)
      (when id (cxml:attribute "id" id))
      (when type (cxml:attribute "type" (string-downcase (string type))))
      (cxml:with-element "body" (cxml:text body))
      (when xhtml-body
        (cxml:with-element "html"
          (cxml:attribute "xmlns" "http://jabber.org/protocol/xhtml-im")
          (cxml:with-element "body"
            (cxml:attribute "xmlns" "http://www.w3.org/1999/xhtml")
            (cxml:unescaped xhtml-body)))))))

(defmethod xmpp:handle ((connection xmpp:connection) (message xmpp:message))
  (format xmpp:*debug-stream* "~&message: ~a" (xmpp:body message))
  (format xmpp:*debug-stream* "~&message type: ~a" (xmpp::type- message))
  (process-message connection message)
  message)

(defmethod xmpp:handle ((connection xmpp:connection) (event xmpp:presence))
  (format xmpp:*debug-stream* "~&presence: ~a" event)
  event)

(defmethod xmpp:handle ((connection xmpp:connection) (event xmpp:xml-element))
  (format xmpp:*debug-stream* "~&UNKNOWN ELEMENT: ~a" event)
  event)

(defmethod xmpp:handle ((connection xmpp:connection)
                        (event (eql :session-initiated)))
  (xmpp:presence connection
                 :to (xmpp:username connection)
                 :x "http://jabber.org/protocol/muc" ; TODO find right namespace or remove method
                 :max-stanzas 0))

(defmethod xmpp:get-element ((obj (eql nil)) name &key (test 'eq))
  (format xmpp:*debug-stream* "~&get-element: ~a ~a" obj name))

(defmethod xmpp:presence ((connection xmpp:connection)
                          &key type to status show priority x pass max-stanzas)
  (xmpp::with-xml-output (connection)
    (cxml:with-element "presence"
      (when type
        (cxml:attribute "type" type))
      (when to
        (cxml:attribute "to" to))
      (when status
        (cxml:with-element "status"
          (cxml:text status)))
      (when show
        (cxml:with-element "show"
          (cxml:text show)))
      (when priority
        (cxml:with-element "priority"
          (cxml:text (format nil "~A" priority))))
      (when x
        (cxml:with-element "x"
          (cxml:attribute "xmlns" x)
          (when pass
            (cxml:with-element "password"
              (cxml:text pass)))
          (when max-stanzas
            (cxml:with-element "history"
              (cxml:attribute "maxstanzas" max-stanzas))))))))

(defun connect (&optional (login *xmpp-login*) (password *xmpp-password*)
                &key (nick *xmpp-login*) (server *xmpp-server*))
  (check-type login string)
  (check-type password string)
  (setf *connection* (xmpp:connect :hostname server))
  (handler-case
      (unwind-protect
           (progn
             (xmpp:auth *connection* login password *xmpp-resource* :mechanism :sasl-digest-md5)
             (xmpp:bind *connection* *xmpp-resource*)
             (xmpp:session *connection*)
             (xmpp:presence *connection*
                            :show "chat"
                            :status "online")
             (xmpp:receive-stanza-loop
              *connection*
              :stanza-callback #'callback-with-restart))
        (xmpp:disconnect *connection*))
    (error (e)
      (progn
        (format t "~&XMPP ERROR: ~a" e)
        (push (make-user-error) *errors*)
        (sleep 5)
        (connect login password :nick nick :server server)))))

(defun start-loop ()
  (xmpp:receive-stanza-loop *connection*))

(defun send (to msg)
  (xmpp:message *connection* to msg))

(defun get-stanza ()
  (xmpp:receive-stanza *connection*))

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
           (process-groupchat-message connection message)))))))

(defun process-chat-message (connection message)
  (unless (process-personal connection message)
    (reply-chat connection (xmpp:from message)
                "Unknown query.."
                (xmpp::type- message))))

(defun process-personal (connection message)
  (let* ((body (xmpp:body message))
         (from (xmpp:from message))
         (from-bare (subseq from 0 (position #\/ from))))
    (set-collection-name-by-user from-bare)
    (reply-chat connection from
                (reply-message (string-right-trim " " body))
                (xmpp::type- message))))

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
