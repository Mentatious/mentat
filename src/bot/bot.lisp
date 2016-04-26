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

(defun ensure-list (var)
  (if (listp var)
      var
      (list var)))

(defun pick-entries (&key (numbers nil) (begin nil) (end nil))
  (let ((entries-to-process nil))
    (cond ((and numbers (listp numbers))
           (dolist (entry (mapcar #'parse-integer numbers))
             (push (pick-entry entry) entries-to-process)))
          ((and begin end)
           (let* ((begin-int (parse-integer begin))
                  (end-int (parse-integer end))
                  (start (min begin-int end-int))
                  (finish (max begin-int end-int)))
             (dolist (entry (loop for n from start below (+ finish 1) collect n))
               (push (pick-entry entry) entries-to-process))))
          (t nil))
    (when entries-to-process
      (nreverse entries-to-process))))

(cl-lex:define-string-lexer bot-lexer
  ("\"[А-Яа-яA-Za-z0-9_.,\%\-/|><\:\'\=\(\)\*\?\#\№\@\`\ ]+\""
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
  ("tags" (return (values 'tags 'tags)))
  ("none" (return (values 'none 'none)))
  ("ts" (return (values 'ts 'ts)))
  ("[Uu]pdate" (return (values 'update 'update)))
  ("set" (return (values 'set 'set)))
  ("^[Dd]rop" (return (values 'drop $@)))
  ("^[Cc]leardb" (return (values 'cleardb $@)))
  ("^[Uu]sage" (return (values 'usage $@)))
  ("^[Ss]earch" (return (values 'search $@)))
  ("todo" (return (values 'entrystatus $@)))
  ("done" (return (values 'entrystatus $@)))
  ("\\#[AaBbCc]" (return (values 'prio $@)))
  ("[0-9A-Za-z_]+\:"
   (return (values 'tag
                   (string-right-trim "\:" $@))))
  ("\:" (return (values 'colon $@))))

(yacc:define-parser bot-parser
  (:start-symbol message)
  (:terminals (entrydata hyphen number colon tag tags none
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
                                          (let ((dropped-messages-list nil))
                                            (dolist (entry (pick-entries :begin begin :end end))
                                              (push (drop-entry entry) dropped-messages-list))
                                            (format nil "~{~%Dropped '~a'~}" (nreverse dropped-messages-list)))))
           (drop number #'(lambda (drop number)
                            (declare (ignore drop))
                            (let ((deleted (drop-entry (parse-integer number))))
                              (format nil "Dropped '~a'" deleted))))
           (drop numbers #'(lambda (drop numbers)
                             (declare (ignore drop))
                             (let ((dropped-messages-list nil))
                               (dolist (entry (pick-entries :numbers numbers))
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
           (update number set tags colon manytags
                   #'(lambda (update number set tags colon manytags)
                       (declare (ignore update set tags colon))
                       (let* ((entry (pick-entry (parse-integer number)))
                              (formatted-before (format-entry entry)))
                         (set-entry-field entry "tags" (ensure-list manytags))
                         (format nil "Updated.~%before: '~a'~%after : '~a'" formatted-before (format-entry entry)))))
           (update numbers set tags colon manytags
                   #'(lambda (update numbers set tags colon manytags)
                       (declare (ignore update set tags colon))
                       (let ((updated-messages-list nil))
                         (dolist (entry (pick-entries :numbers numbers))
                           (let ((formatted-before (format-entry entry)))
                             (set-entry-field entry "tags" (ensure-list manytags))
                             (push
                              (format nil "Updated-----------~%before: '~a'~%after : '~a'" formatted-before (format-entry entry))
                              updated-messages-list)))
                         (format nil "~{~%~a~}" (nreverse updated-messages-list)))))
           (update number hyphen number set tags colon manytags
                   #'(lambda (update begin hyphen end set tags colon manytags)
                       (declare (ignore update hyphen set tags colon))
                       (let ((updated-messages-list nil))
                         (dolist (entry (pick-entries :begin begin :end end))
                           (let ((formatted-before (format-entry entry)))
                             (set-entry-field entry "tags" (ensure-list manytags))
                             (push
                              (format nil "Updated-----------~%before: '~a'~%after : '~a'" formatted-before (format-entry entry))
                              updated-messages-list)))
                         (format nil "~{~%~a~}" (nreverse updated-messages-list)))))
           (update number set tags none
                   #'(lambda (update number set tags none)
                       (declare (ignore update set tags none))
                       (let* ((entry (pick-entry (parse-integer number)))
                              (formatted-before (format-entry entry)))
                         (set-entry-field entry "tags" nil)
                         (format nil "Updated.~%before: '~a'~%after : '~a'" formatted-before (format-entry entry)))))
           (update number set priority prio
                   #'(lambda (update number set priority prio)
                       (declare (ignore update set))
                       (let* ((entry (pick-entry (parse-integer number)))
                              (formatted-before (format-entry entry)))
                         (set-entry-field entry "priority" prio)
                         (format nil "Updated.~%before: '~a'~%after : '~a'" formatted-before (format-entry entry)))))
           (search priority prio
                   #'(lambda (search priority prio)
                       (declare (ignore search priority))
                       (let ((entries (list-entries :field priority :value prio)))
                         (if (> (length entries) 0)
                             (format nil "entries:~{~%~a~}" entries)
                             (format nil "No entries found.")))))
           (search heading entrydata
                   #'(lambda (search heading entrydata)
                       (declare (ignore search heading))
                       (let ((entries (list-entries :field heading :value entrydata)))
                         (if (> (length entries) 0)
                             (format nil "entries:~{~%~a~}" entries)
                             (format nil "No entries found.")))))
           (search tags colon manytags
                   #'(lambda (search tags colon manytags)
                       (declare (ignore search tags colon))
                       (let ((entries (list-entries
                                       :field "tags"
                                       :value (if (listp manytags)
                                                  manytags
                                                  (list manytags)))))
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
                          (format nil "~{~a ~}~a" (alexandria:flatten numbers) number))))))
  (manytags tag
            (manytags tag
                      #'(lambda (manytags tag)
                          (mapcar #'(lambda (tagdata) (string-right-trim ":" tagdata))
                                  `(,@(alexandria:flatten manytags) ,tag))))))

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
