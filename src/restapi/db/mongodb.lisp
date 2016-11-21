(in-package #:mentat-restapi)

(defun init-storage ()
  (with-check-connection
    (cl-mongo:db.use mentat-config::*db-name*)))

(defparameter *current-collection-name* nil)

(defparameter *last-query-result* nil)

(defun set-user-context (username)
  (setf *current-collection-name*
        (concatenate 'string mentat-config::*entries-collection-prefix* "-" username)))

(defun add-entry (heading &key (status "") (priority "") (tags nil) (scheduled nil) (deadline nil))
  (with-check-connection
    (let* ((doc (cl-mongo:make-document))
           (ts_added (get-universal-time))
           (ts_updated ts_added))
      (when (plusp (length status)) (set-entry-field doc "status" status :save-entry nil))
      (when (plusp (length priority)) (set-entry-field doc "priority" priority :save-entry nil))
      (when (plusp (length heading)) (set-entry-field doc "heading" heading :save-entry nil))
      (when (plusp (length tags)) (set-entry-field doc "tags" tags :save-entry nil))
      (when scheduled (set-entry-field doc "scheduled" scheduled :save-entry nil))
      (when deadline (set-entry-field doc "deadline" deadline :save-entry nil))
      (set-entry-field doc "ts_added" (write-to-string ts_added) :save-entry nil)
      (set-entry-field doc "ts_updated" (write-to-string ts_updated) :save-entry nil)
      (set-entry-field doc "entry_id" (uuid::print-object (uuid:make-v4-uuid) nil))
      (cl-mongo:db.insert *current-collection-name* doc)
      )))


(defun set-entry-field (entry field value &key (save-entry t))
  (cond ((listp value) (cl-mongo:add-element field value entry))
        ((stringp value) (cl-mongo:add-element field (string-trim '(#\Space) value) entry))
        (t (cl-mongo:add-element field value entry)))
  (when save-entry
    (cl-mongo:add-element "ts_updated" (get-universal-time) entry)
    (cl-mongo:db.save *current-collection-name* entry)))

(defun get-entry-field (entry field)
  (cl-mongo:get-element field entry))

(defparameter *mentat-db-prefix* "mentat.")

(defun get-user-collection-names ()
  (mapcar #'(lambda (name) (subseq name (length *mentat-db-prefix*)))
          (remove-if
           #'(lambda (str)
               (or (search "$_id_" str :from-end t)
                   ;; FIXME: *headings should be removed from DB and hence from here as it's a dev artifact ;)
                   (member str (mapcar #'(lambda (name) (concatenate 'string *mentat-db-prefix* name))
                                       '("system.indexes" "headings"))
                           :test #'equal)))
           (mapcar #'(lambda (coll)
                       (cl-mongo::get-element "name" coll))
                   (cl-mongo:docs (cl-mongo:db.collections))))))

(defun clear-entries ()
  (with-check-connection
    (cl-mongo:rm *current-collection-name* :all)))

(defun format-entry (doc)
  (let ((status (get-entry-field doc "status"))
        (priority (get-entry-field doc "priority"))
        (heading (get-entry-field doc "heading"))
        (timestamp (get-entry-field doc "timestamp"))
        (tags (get-entry-field doc "tags"))
        (scheduled (get-entry-field doc "scheduled"))
        (deadline (get-entry-field doc "deadline")))
    (string-right-trim
     " " (format nil "~a~a~a~a~a~a~a"
                 (if status
                     (concatenate 'string (string-upcase status) " ")
                     "")
                 (if priority
                     (concatenate 'string "[" (string-upcase priority) "]" " ")
                     "")
                 (if heading
                     (concatenate 'string heading " ")
                     "")
                 (if timestamp
                     (concatenate 'string timestamp " ")
                     "")
                 (if tags
                     (format nil ":~{~a:~} " tags)
                     "")
                 (if scheduled
                     (concatenate 'string (format-timestamp scheduled :as-scheduled t) " ")
                     "")
                 (if deadline
                     (format-timestamp deadline :as-deadline t)
                     "")))))

(defparameter *sortby-criterion* "ts_added")

(defun find-entries-sorted (&key (field nil) (value nil) (sort-by *sortby-criterion*))
  (with-check-connection
    (cl-mongo:docs
     (cl-mongo:iter
      (cl-mongo:db.find
       *current-collection-name*
       (cl-mongo:kv
        (cl-mongo:kv "query"
                     (cl-mongo:kv field
                                  (if value
                                      (cl-mongo:kv (cl-mongo:kv "$regex" value)
                                                   (cl-mongo:kv "$options" "i"))
                                      value)))
        (cl-mongo:kv "orderby" (cl-mongo:kv (cl-mongo:kv "db" 1)
                                            (cl-mongo:kv sort-by 1))))
       :limit 0)))))

;;TODO: find less straightforward/more clever way to filter by lists
(defun find-entries-by-list-sorted (&key (field nil) (value nil) (sort-by *sortby-criterion*))
  (declare (ignore field value))
  (with-check-connection
    (let ((docs-ordered
           (cl-mongo:docs
            (cl-mongo:iter
             (cl-mongo:db.find
              *current-collection-name*
              (cl-mongo:kv
               (cl-mongo:kv "query"
                            (cl-mongo:kv nil nil))
               (cl-mongo:kv "orderby" (cl-mongo:kv (cl-mongo:kv "db" 1)
                                                   (cl-mongo:kv sort-by 1))))
              :limit 0)))))
      (format xmpp:*debug-stream* "~&docs-ordered: ~a" docs-ordered)
      (if (and field value (listp value))
          (remove-if
           (complement
            (lambda (doc)
              (intersection (cl-mongo:get-element field doc) value :test #'equalp))) docs-ordered)
          docs-ordered))))

(defun find-entries-nonempty-field (collection field)
  (with-check-connection
    (let ((docs-ordered
           (cl-mongo:docs
            (cl-mongo:iter
             (cl-mongo:db.find
              collection
              (cl-mongo:kv
               (cl-mongo:kv "query"
                            (cl-mongo:$!= field nil))
               (cl-mongo:kv "orderby" (cl-mongo:kv (cl-mongo:kv "db" 1)
                                                   (cl-mongo:kv field 1)))) :limit 0)))))
      (format xmpp:*debug-stream* "~&docs-ordered: ~a" docs-ordered)
      docs-ordered)))

(defun find-timestamped-entries (&key (collection *current-collection-name*) (today nil))
  (let ((results (union
                  (find-entries-nonempty-field collection "scheduled")
                  (find-entries-nonempty-field collection "deadline"))))
    (when today
      (setf results
            (remove-if
             (complement #'(lambda (entry)
                             (or (timestamp-is-today-p (get-entry-field entry "scheduled"))
                                 (timestamp-is-today-p (get-entry-field entry "deadline")))))
             results)))
    (setf *last-query-result* results)
    results))

(defun find-all-timestamped-entries-by-collection (&key (today nil))
  (let ((all-entries (make-hash-table :test #'equal)))
    (dolist (user-collection (get-user-collection-names))
      (setf (gethash (subseq user-collection (length "entries-")) all-entries)
            (find-timestamped-entries :collection user-collection :today today)))
    all-entries))

(defun list-entries (&key (field nil) (value nil) (sort-by *sortby-criterion*) (start 0) (end nil))
  (with-check-connection
      (let ((results (cond ((and value (listp value)) (find-entries-by-list-sorted :field field
                                                                                   :value value
                                                                                   :sort-by sort-by))
                           (t (find-entries-sorted :field field :value value :sort-by sort-by)))))
        (when (minusp start)
          (setf start (+ (length results) start)))
        (when (and end (minusp end))
          (setf end (+ (length results) end)))
        (if end
            (setf results (subseq results start end))
            (setf results (nthcdr start results)))
        (setf *last-query-result* results)
        results)))

(defun print-entries (entries &key (as-org nil))
  (loop for doc in entries
     for i from 1 to (length entries) collect
       (if as-org
           (concatenate 'string "* " (format-entry doc))
           (concatenate 'string (write-to-string i) ") " (format-entry doc)))))

(defun pick-entry (index &key (last-query nil))
  (let ((entries
         (if last-query
             *last-query-result*
             (find-entries-sorted))))
    (nth (- index 1) entries)))

(defun drop-entry (entry)
  (let ((formatted (format-entry entry)))
    (cl-mongo:db.delete *current-collection-name* entry)
    (setf *last-query-result* (remove entry *last-query-result*))
    formatted))
