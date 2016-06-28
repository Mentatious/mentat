(in-package #:mentat)

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
    (local-time:reread-timezone-repository
     :timezone-repository (pathname mentat-config::*timezone-repository-system-path*))
    (setf local-time:*default-timezone* (local-time:find-timezone-by-location-name mentat-config::*client-timezone*))
    (init-storage)
    (start-connection-loop *connection*)))

#+sbcl
(defun save-image-xmpp-bot ()
  (swank-loader::init :load-contribs t)
  (sb-ext:save-lisp-and-die "mentat-xmpp-bot"
                            :compression t
                            :executable t
                            :toplevel #'main-xmpp-bot))

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

;;FIXME: do not cherry-pick entries in batch
(defun pick-entries (numbers &key (last-query nil))
  (let ((entries-to-process nil))
    (dolist (entry (mapcar #'parse-integer (ensure-list numbers)))
      (let ((picked-entry (pick-entry entry :last-query last-query)))
        (when picked-entry
          (push picked-entry entries-to-process))))
    (when entries-to-process
      (nreverse entries-to-process))))

(defun update-entries (entries field value)
  (let ((result-messages nil))
    (if entries
        (progn
          (dolist (entry entries)
            (let ((before (format-entry entry)))
              (set-entry-field entry field value)
              (push
               (format nil "Updated-----------~%before: '~a'~%after : '~a'" before (format-entry entry))
               result-messages)))
          (format nil "~{~%~a~}" (nreverse result-messages)))
        (format nil "No entries to update."))))

(cl-lex:define-string-lexer bot-lexer
  ("\"[А-Яа-яA-Za-z0-9ё_.,\%\&\-/|><\:\;\'\=\(\)\*\?\#\№\@\`\!\ ]+\""
   (return (values 'entrydata
                   (string-trim "\"" $@))))
  ("\-" (return (values 'hyphen $@)))
  ("schedule" (return (values 'schedule $@)))
  ("unschedule" (return (values 'unschedule $@)))
  ("deadline" (return (values 'deadline $@)))
  ("undeadline" (return (values 'undeadline $@)))
  ("pick" (return (values 'pick $@)))
  ("[0-9]{2}-[0-9]{2}-[0-9]{4}" (return (values 'date $@)))
  ("[0-9]{2}:[0-9]{2}" (return (values 'time $@)))
  ("[0-9]+" (return (values 'number $@)))
  ("^[Aa]dd" (return (values 'add $@)))
  ("^[Pp]rint" (return (values 'print $@)))
  ("all$" (return (values 'all 'all)))
  ("org$" (return (values 'org 'org)))
  ("raw$" (return (values 'raw 'raw)))
  ("timestamped" (return (values 'timestamped 'timestamped)))
  ("today" (return (values 'today 'today)))
  ("last" (return (values 'last $@)))
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
  (:terminals (add all cleardb colon date deadline drop entrydata entrystatus heading
               hyphen id last none number org pick print prio priority raw schedule
               search set sortby status tag tags time timestamped today ts undeadline
               unschedule update usage what))
  (message (add entrydata
                #'(lambda (add entrydata)
                    (declare (ignore add))
                    (add-entry entrydata)
                    (format nil "Added '~a'." entrydata)))
           (add prio entrydata)
           (add todo entrydata)
           (add todo prio entrydata)
           (sortby what
                   #'(lambda (sortby what)
                       (declare (ignore sortby what))
                       (format nil "Sorting by: ~a" *sortby-criterion*)))
           (sortby id
                   #'(lambda (sortby id)
                       (declare (ignore sortby id))
                       (setf *sortby-criterion* "id")))
           (sortby status
                   #'(lambda (sortby status)
                       (declare (ignore sortby status))
                       (setf *sortby-criterion* "status")))
           (sortby priority
                   #'(lambda (sortby priority)
                       (declare (ignore sortby priority))
                       (setf *sortby-criterion* "priority")))
           (sortby heading
                   #'(lambda (sortby heading)
                       (declare (ignore sortby heading))
                       (setf *sortby-criterion* "heading")))
           (sortby ts
                   #'(lambda (sortby ts)
                       (declare (ignore sortby ts))
                       (setf *sortby-criterion* "ts_added")))
           (print all #'(lambda (print all)
                          (declare (ignore print all))
                          (let ((entries (list-entries)))
                            (if (> (length entries) 0)
                                (format nil "entries:~{~%~a~}" (print-entries entries))
                                (format nil "No entries found.")))))
           (print org #'(lambda (print org)
                          (declare (ignore print org))
                          (let ((entries (list-entries)))
                            (if (> (length entries) 0)
                                (format nil "entries:~{~%~a~}" (print-entries entries :as-org t))
                                (format nil "No entries found.")))))
           (print raw #'(lambda (print raw)
                          (declare (ignore print raw))
                          (format nil "Not implemented yet.")))
           (print last #'(lambda (print last)
                          (declare (ignore print last))
                          (let ((entries *last-query-result*))
                            (if (> (length entries) 0)
                                (format nil "entries:~{~%~a~}" (print-entries entries))
                                (format nil "No last query results.")))))
           (print timestamped #'(lambda (print timestamped)
                          (declare (ignore print timestamped))
                          (let ((entries (find-timestamped-entries)))
                            (if (> (length entries) 0)
                                (format nil "entries:~{~%~a~}" (print-entries entries))
                                (format nil "No last query results.")))))
           (print timestamped today #'(lambda (print timestamped today)
                          (declare (ignore print timestamped today))
                          (let ((entries (find-timestamped-entries :today t)))
                            (if (> (length entries) 0)
                                (format nil "entries:~{~%~a~}" (print-entries entries))
                                (format nil "No last query results.")))))
           (drop numbers #'(lambda (drop indexes)
                             (declare (ignore drop))
                             (let ((dropped-messages-list nil))
                               (dolist (entry (pick-entries (ensure-list indexes)))
                                 (push (drop-entry entry) dropped-messages-list))
                               (format nil "~{~%Dropped '~a'~}" (nreverse dropped-messages-list)))))
           (drop last #'(lambda (drop last)
                          (declare (ignore drop last))
                          (let ((dropped-messages-list nil))
                            (if *last-query-result*
                                (progn
                                  (dolist (entry *last-query-result*)
                                    (push (drop-entry entry) dropped-messages-list))
                                  (format nil "~{~%Dropped '~a'~}" (nreverse dropped-messages-list)))
                                (format nil "No last query results.")))))
           (drop last numbers
                 #'(lambda (drop last indexes)
                          (declare (ignore drop last))
                          (let ((dropped-messages-list nil))
                            (if *last-query-result*
                                (progn
                                  (dolist (entry (pick-entries (ensure-list indexes) :last-query t))
                                    (push (drop-entry entry) dropped-messages-list))
                                  (format nil "~{~%Dropped '~a'~}" (nreverse dropped-messages-list)))
                                (format nil "No last query results.")))))
           (update numbers set heading entrydata
                   #'(lambda (update indexes set heading entrydata)
                       (declare (ignore update set heading))
                       (if (listp indexes)
                           (format nil "Cannot update headings in batch.")
                           (update-entries (pick-entries (ensure-list indexes)) "heading" entrydata))))
           (update last numbers set heading entrydata
                   #'(lambda (update last indexes set heading entrydata)
                       (declare (ignore update last set heading))
                       (if (listp indexes)
                           (format nil "Cannot update headings in batch.")
                           (update-entries (pick-entries (ensure-list indexes) :last-query t) "heading" entrydata))))
           (update numbers set status entrystatus
                   #'(lambda (update indexes set status entrystatus)
                       (declare (ignore update set status))
                       (update-entries (pick-entries (ensure-list indexes)) "status" entrystatus)))
           (update last set status entrystatus
                   #'(lambda (update last set status entrystatus)
                       (declare (ignore update last set status))
                       (update-entries *last-query-result* "status" entrystatus)))
           (update last numbers set status entrystatus
                   #'(lambda (update last indexes set status entrystatus)
                       (declare (ignore update last set status))
                       (update-entries (pick-entries (ensure-list indexes) :last-query t) "status" entrystatus)))
           (update numbers set tags colon manytags
                   #'(lambda (update indexes set tags colon manytags)
                       (declare (ignore update set tags colon))
                       (update-entries (pick-entries (ensure-list indexes)) "tags" (ensure-list manytags))))
           (update last set tags colon manytags
                   #'(lambda (update last set tags colon manytags)
                       (declare (ignore update last set tags colon))
                       (update-entries *last-query-result* "tags" (ensure-list manytags))))
           (update last numbers set tags colon manytags
                   #'(lambda (update last indexes set tags colon manytags)
                       (declare (ignore update last set tags colon))
                       (update-entries (pick-entries (ensure-list indexes) :last-query t) "tags" (ensure-list manytags))))
           (update numbers set tags none
                   #'(lambda (update indexes set tags none)
                       (declare (ignore update set tags none))
                       (update-entries (pick-entries (ensure-list indexes)) "tags" nil)))
           (update last set tags none
                   #'(lambda (update last set tags none)
                       (declare (ignore update last set tags none))
                       (update-entries *last-query-result* "tags" nil)))
           (update last numbers set tags none
                   #'(lambda (update last indexes set tags none)
                       (declare (ignore update last set tags none))
                       (update-entries (pick-entries (ensure-list indexes) :last-query t) "tags" nil)))
           (update numbers set priority prio
                   #'(lambda (update indexes set priority prio)
                       (declare (ignore update set priority))
                       (update-entries (pick-entries (ensure-list indexes)) "priority" prio)))
           (update last set priority prio
                   #'(lambda (update last set priority prio)
                       (declare (ignore update last set priority))
                       (update-entries *last-query-result* "priority" prio)))
           (update last numbers set priority prio
                   #'(lambda (update last indexes set priority prio)
                       (declare (ignore update last set priority))
                       (update-entries (pick-entries (ensure-list indexes) :last-query t) "priority" prio)))
           (schedule numbers pick timestamp
                  #'(lambda (schedule indexes pick timestamp)
                       (declare (ignore schedule pick))
                       (update-entries (pick-entries (ensure-list indexes))
                                       "scheduled" (make-timestamp timestamp))))
           (unschedule numbers
                  #'(lambda (unschedule indexes)
                       (declare (ignore unschedule))
                       (update-entries (pick-entries (ensure-list indexes))
                                       "scheduled" nil)))
           (schedule last pick timestamp
                  #'(lambda (schedule last pick timestamp)
                       (declare (ignore schedule last pick))
                       (update-entries *last-query-result* "scheduled" (make-timestamp timestamp))))
           (unschedule last
                  #'(lambda (unschedule last)
                       (declare (ignore unschedule last))
                       (update-entries *last-query-result* "scheduled" nil)))
           (schedule last numbers pick timestamp
                  #'(lambda (schedule last indexes pick timestamp)
                       (declare (ignore schedule last pick))
                       (update-entries (pick-entries (ensure-list indexes) :last-query t)
                                       "scheduled" (make-timestamp timestamp))))
           (unschedule last numbers
                  #'(lambda (unschedule last indexes)
                       (declare (ignore unschedule last))
                       (update-entries (pick-entries (ensure-list indexes) :last-query t)
                                       "scheduled" nil)))
           (deadline numbers pick timestamp
                  #'(lambda (deadline indexes pick timestamp)
                       (declare (ignore deadline pick))
                       (update-entries (pick-entries (ensure-list indexes))
                                       "deadline" (make-timestamp timestamp))))
           (undeadline numbers
                  #'(lambda (undeadline indexes)
                       (declare (ignore undeadline))
                       (update-entries (pick-entries (ensure-list indexes))
                                       "deadline" nil)))
           (deadline last pick timestamp
                  #'(lambda (deadline last pick timestamp)
                       (declare (ignore deadline last pick))
                       (update-entries *last-query-result* "deadline" (make-timestamp timestamp))))
           (undeadline last
                  #'(lambda (undeadline last)
                       (declare (ignore undeadline last))
                       (update-entries *last-query-result* "deadline" nil)))
           (deadline last numbers pick timestamp
                  #'(lambda (deadline last indexes pick timestamp)
                       (declare (ignore deadline last pick))
                       (update-entries (pick-entries (ensure-list indexes) :last-query t)
                                       "deadline" (make-timestamp timestamp))))
           (undeadline last numbers
                  #'(lambda (undeadline last indexes)
                       (declare (ignore undeadline last))
                       (update-entries (pick-entries (ensure-list indexes) :last-query t)
                                       "deadline" nil)))
           (search priority prio
                   #'(lambda (search priority prio)
                       (declare (ignore search))
                       (let ((entries (list-entries :field priority :value prio)))
                         (if (> (length entries) 0)
                             (format nil "entries:~{~%~a~}" (print-entries entries))
                             (format nil "No entries found.")))))
           (search heading entrydata
                   #'(lambda (search heading entrydata)
                       (declare (ignore search))
                       (let ((entries (list-entries :field heading :value entrydata)))
                         (if (> (length entries) 0)
                             (format nil "entries:~{~%~a~}" (print-entries entries))
                             (format nil "No entries found.")))))
           (search tags colon manytags
                   #'(lambda (search tags colon manytags)
                       (declare (ignore search tags colon))
                       (let ((entries (list-entries :field "tags" :value (ensure-list manytags))))
                         (if (> (length entries) 0)
                             (format nil "entries:~{~%~a~}" (print-entries entries))
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
                          (format nil "~{~a ~}~a" (alexandria:flatten numbers) number)))))
           (number hyphen number
                   #'(lambda (begin hyphen end)
                       (declare (ignore hyphen))
                       (let* ((entries-to-process nil)
                              (begin-int (parse-integer begin))
                              (end-int (parse-integer end))
                              (start (min begin-int end-int))
                              (finish (max begin-int end-int)))
                         (dolist (index (loop for n from start below (+ finish 1) collect n))
                           (push (write-to-string index) entries-to-process))
                         (nreverse entries-to-process)))))
  (manytags tag
            (manytags tag
                      #'(lambda (manytags tag)
                          (mapcar #'(lambda (tagdata) (string-right-trim ":" tagdata))
                                  `(,@(alexandria:flatten manytags) ,tag)))))
  (timestamp date
             (date time)))

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
