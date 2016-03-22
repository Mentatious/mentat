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

;;TODO: automate
(defparameter *usage*
  '("add <words> - add entry"
    "print <all|org|raw> print in various formats, use 'all' by default"
    "sortby <id|status|priority|heading> - set sorting by entry field, 'id' used by default"
    "drop <number> - drop entry by number, print entries beforehand, otherwise numbers may skew"
    "cleardb - wipe database"
    "usage - print this reference"))

(cl-lex:define-string-lexer bot-lexer
  ("\"" (return (values 'quote $@)))
  ("\-" (return (values 'hyphen $@)))
  ("[0-9]+" (return (values 'number $@)))
  ("^[Aa]dd" (return (values 'add $@)))
  ("^[Pp]rint" (return (values 'print $@)))
  ("all$" (return (values 'all 'all)))
  ("org$" (return (values 'org 'org)))
  ("raw$" (return (values 'raw 'raw)))
  ("^[Ss]ortby" (return (values 'sortby $@)))
  ("id$" (return (values 'id 'id)))
  ("status$" (return (values 'status 'status)))
  ("priority$" (return (values 'priority 'priority)))
  ("heading$" (return (values 'heading 'heading)))
  ("^[Dd]rop" (return (values 'drop $@)))
  ("^[Cc]leardb" (return (values 'cleardb $@)))
  ("^[Uu]sage" (return (values 'usage $@)))
  ("[А-Яа-яA-Za-z0-9_.,\-/|><\:\'\=\(\)\*\?\#]+" (return (values 'word $@))) ;TODO: more general definition
  ("\\#[AaBbCc]" (return (values 'prio $@))))

(yacc:define-parser bot-parser
  (:start-symbol message)
  (:terminals (word number hyphen prio add quote print all org raw drop cleardb sortby id status priority heading usage))
  (message (add quote words quote
                #'(lambda (add quote words quote1)
                    (declare (ignore add quote quote1))
                    (add-entry words)
                    (format nil "Added.")))
           (add prio quote words quote)
           (add todo quote words quote)
           (add todo prio quote words quote)
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
           (drop number hyphen number #'(lambda (drop begin hyphen end)
                            (declare (ignore drop hyphen))
                            (let* ((begin-int (parse-integer begin))
                                   (end-int (parse-integer end))
                                   (start (min begin-int end-int))
                                   (finish (max begin-int end-int))
                                   (messages-to-drop nil)
                                   (dropped-messages-list nil))
                              (dolist (entry (loop for n from start below (+ finish 1) collect n))
                                 (push (pick-entry entry) messages-to-drop))
                              (dolist (entry (nreverse messages-to-drop))
                                (push (drop-entry entry) dropped-messages-list))
                              (format nil "~{~%Dropped '~a'~}" (nreverse dropped-messages-list)))))
           (drop number #'(lambda (drop number)
                            (declare (ignore drop))
                            (let ((deleted (drop-entry (parse-integer number))))
                              (format nil "Dropped '~a'" deleted))))
           (drop numbers #'(lambda (drop numbers)
                             (declare (ignore drop))
                             (let ((messages-to-drop nil)
                                   (dropped-messages-list nil))
                               (dolist (entry (mapcar #'parse-integer numbers))
                                 (push (pick-entry entry) messages-to-drop))
                               (dolist (entry (nreverse messages-to-drop))
                                 (push (drop-entry entry) dropped-messages-list))
                               (format nil "~{~%Dropped '~a'~}" (nreverse dropped-messages-list)))))
           (cleardb #'(lambda (cleardb)
                        (declare (ignore cleardb))
                        (clear-entries)
                        (format nil "DB wiped.")))
           (usage #'(lambda (usage)
                      (declare (ignore usage))
                      (format nil "~{~%~a~}" *usage*))))
  (numbers number
           (numbers number
                    #'(lambda (numbers number)
                        (split-sequence:split-sequence
                         #\Space
                         (string-right-trim
                          " "
                          (format nil "~{~a ~}~a" (alexandria:flatten numbers) number))))))
  (words number
         word
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
        ;; think of informative error analisys and output
        (format xmpp:*debug-stream* "~&[reply-message] encountered error: ~a" e)
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
