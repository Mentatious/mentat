(in-package #:mentat)

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

(defparameter *errors* nil)
(defparameter *connection* nil
  "XMPP connection")

#+sbcl
(defun main-xmpp-bot ()
  (with-interactive-interrupt
    (setq swank:*use-dedicated-output-stream* nil)
    (swank:create-server
     :port 4006
     :style swank:*communication-style*
     :dont-close t)
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

(cl-lex:define-string-lexer bot-lexer
  ("[Aa]dd" (return (values 'add $@)))
  ("[Pp]rint" (return (values 'print $@)))
  ("all" (return (values 'all $@)))
  ("org" (return (values 'org $@)))
  ("raw" (return (values 'raw $@)))
  ("[Ss]ortby" (return (values 'sortby $@)))
  ("id" (return (values 'id $@)))
  ("status" (return (values 'status $@)))
  ("priority" (return (values 'priority $@)))
  ("heading" (return (values 'heading $@)))
  ("[Dd]rop" (return (values 'drop $@)))
  ("[Cc]leardb" (return (values 'cleardb $@)))
  ("\\:" (return (values 'tag-delim $@)))
  ("[0-9]+" (return (values 'number (parse-integer $@))))
  ("[A-Za-z0-9_.,\-/|><\:\'\=\(\)\*\"\?\#]+" (return (values 'word $@))) ;TODO: more general definition
  ("\\#[AaBbCc]" (return (values 'prio $@))))

(yacc:define-parser bot-parser
  (:start-symbol message)
  (:terminals (word number prio add print all org raw drop cleardb sortby id status priority heading))
  (message (add todo prio words timestamp tags)
           (add words
                #'(lambda (add words)
                    (declare (ignore add))
                    (add-entry words)
                    (format nil "Added.")))
           (add words tags)
           (add words timestamp)
           (add words timestamp tags)
           (add prio words)
           (add prio words tags)
           (add prio words timestamp)
           (add prio words timestamp tags)
           (add todo words)
           (add todo words tags)
           (add todo words timestamp)
           (add todo words timestamp tags)
           (add todo prio words)
           (add todo prio words tags)
           (add todo prio words timestamp)
           (sortby id
                   #'(lambda (sortby id)
                       (declare (ignore sortby))
                       (setf *sortby-criterion* "id")))
           (sortby status
                   #'(lambda (sortby status)
                       (declare (ignore sortby))
                       (setf *sortby-criterion* "status")))
           (sortby priority
                   #'(lambda (sortby priority)
                       (declare (ignore sortby))
                       (setf *sortby-criterion* "priority")))
           (sortby heading
                   #'(lambda (sortby heading)
                       (declare (ignore sortby))
                       (setf *sortby-criterion* "heading")))
           (sortby timestamp)
           (sortby tags)
           (print all #'(lambda (print all)
                          (declare (ignore print all))
                          (let ((entries (list-entries)))
                            (if (> (length entries) 0)
                                (format nil "entries:~{~%~a~}" entries)
                                (format nil "No entries found.")))))
           (print org #'(lambda (print org)
                          (declare (ignore print org))
                          (let ((entries (list-entries t)))
                            (if (> (length entries) 0)
                                (format nil "entries:~{~%~a~}" entries)
                                (format nil "No entries found.")))))
           (print raw #'(lambda (print raw)
                          (declare (ignore print raw))
                          (format nil "Not implemented yet.")))
           (drop number #'(lambda (drop number)
                            (declare (ignore drop))
                            (let ((deleted (drop-entry number)))
                              (format nil "Dropped '~a'" deleted))))
           (cleardb #'(lambda (cleardb)
                          (declare (ignore cleardb))
                          (clear-entries)
                          (format nil "DB wiped."))))
  (words word
         (words number
                #'(lambda (words word)
                    (string-right-trim
                     " "
                     (format nil "~{~a ~}~a" (alexandria:flatten words) word))))
         (words word
                #'(lambda (words word)
                    (string-right-trim
                     " "
                     (format nil "~{~a ~}~a" (alexandria:flatten words) word))))))

(defun reply-message (body)
  (handler-case
      (yacc:parse-with-lexer (bot-lexer body) bot-parser)
    (error (e)
      (progn
        (format nil "Unknown query: ~a" body)
        ))))

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
