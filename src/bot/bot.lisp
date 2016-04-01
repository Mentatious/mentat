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
  '("add \"<entrydata>\" - add entry"
    "print <all|org|raw> print in various formats, use 'all' by default"
    "sortby <id|status|priority|heading> - set sorting by entry field, 'id' used by default"
    "drop <number> - drop entry by number, make sure to print entries beforehand, otherwise numbers may skew / state may evolve"
    "drop <number> <number> ... <number> - specify multiple entry numbers to delete (space delimited)"
    "drop <number>-<number> - specify range of entry numbers to delete"
    "cleardb - wipe database"
    "usage - print this reference"))

(cl-lex:define-string-lexer bot-lexer
  ("\"[А-Яа-яA-Za-z0-9_.,\-/|><\:\'\=\(\)\*\?\#\ ]+\""
   (return (values 'entrydata
                   (string-trim "\"" $@))))
  ("\-" (return (values 'hyphen $@)))
  ("[0-9]+" (return (values 'number $@)))
  ("^[Aa]dd" (return (values 'add $@)))
  ("^[Pp]rint" (return (values 'print $@)))
  ("all$" (return (values 'all 'all)))
  ("org$" (return (values 'org 'org)))
  ("raw$" (return (values 'raw 'raw)))
  ("^[Ss]ortby" (return (values 'sortby $@)))
  ("what" (return (values 'what $@)))
  ("id" (return (values 'id 'id)))
  ("status" (return (values 'status 'status)))
  ("priority" (return (values 'priority 'priority)))
  ("heading" (return (values 'heading 'heading)))
  ("ts" (return (values 'ts 'ts)))
  ("[Uu]pdate" (return (values 'update 'update)))
  ("set" (return (values 'set 'set)))
  ("^[Dd]rop" (return (values 'drop $@)))
  ("^[Cc]leardb" (return (values 'cleardb $@)))
  ("^[Uu]sage" (return (values 'usage $@)))
  ("^[Ss]earch" (return (values 'search $@)))
  ("todo" (return (values 'entrystatus $@)))
  ("done" (return (values 'entrystatus $@)))
  ("\\#[AaBbCc]" (return (values 'prio $@))))

(yacc:define-parser bot-parser
  (:start-symbol message)
  (:terminals (entrydata hyphen number
               add print all org raw sortby what id status priority heading ts
               update set drop cleardb usage entrystatus search prio))
  (message (add entrydata
                #'(lambda (add entrydata)
                    (declare (ignore add))
                    (add-entry entrydata)
                    (format nil "Added.")))
           (add prio entrydata)
           (add todo entrydata)
           (add todo prio entrydata)
           (sortby what
                   #'(lambda (sortby what)
                       (declare (ignore sortby what))
                       (format nil "Sorting by: ~a" *sortby-criterion*)))
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
           (sortby ts
                   #'(lambda (sortby ts)
                       (declare (ignore sortby))
                       (setf *sortby-criterion* "ts_added")))
           (print all #'(lambda (print all)
                          (declare (ignore print all))
                          (let ((entries (list-entries)))
                            (if (> (length entries) 0)
                                (format nil "entries:~{~%~a~}" entries)
                                (format nil "No entries found.")))))
           (print org #'(lambda (print org)
                          (declare (ignore print org))
                          (let ((entries (list-entries :as-org t)))
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
           (update number set heading entrydata
                   #'(lambda (update number set heading entrydata)
                       (declare (ignore update set))
                       (let* ((entry (pick-entry (parse-integer number)))
                              (formatted-before (format-entry entry)))
                         (set-entry-field entry "heading" entrydata)
                         (format nil "Updated.~%before: '~a'~%after : '~a'" formatted-before (format-entry entry)))))
           (update number set status entrystatus)
           (search priority prio
                   #'(lambda (search priority prio)
                          (declare (search priority))
                          (let ((entries (list-entries :field priority :value prio)))
                            (if (> (length entries) 0)
                                (format nil "entries:~{~%~a~}" entries)
                                (format nil "No entries found.")))))
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
                          (format nil "~{~a ~}~a" (alexandria:flatten numbers) number)))))))

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
