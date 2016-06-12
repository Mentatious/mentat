(in-package #:mentat)

(defun init-storage ()
  (with-check-connection
    (cl-mongo:db.use *db-name*)))

(defparameter *current-collection-name* nil)

(defparameter *last-query-result* nil)

(defun set-collection-name-by-user (username)
  (setf *current-collection-name*
        (concatenate 'string *entries-collection-prefix* "-" username)))

(defun add-entry (heading &key (status "") (priority "") (timestamp "") (tags nil))
  (with-check-connection
    (let ((doc (cl-mongo:make-document))
          (ts_added (get-universal-time)))
      (when (> (length status) 0) (cl-mongo:add-element "status" (string-trim '(#\Space) status) doc))
      (when (> (length priority) 0) (cl-mongo:add-element "priority" (string-trim '(#\Space) priority) doc))
      (when (> (length heading) 0) (cl-mongo:add-element "heading" (string-trim '(#\Space) heading) doc))
      (when (> (length timestamp) 0) (cl-mongo:add-element "timestamp" (string-trim '(#\Space) timestamp) doc))
      (when (> (length tags) 0) (cl-mongo:add-element "tags" tags doc))
      (cl-mongo:add-element "ts_added" (write-to-string ts_added) doc)
      (cl-mongo:db.insert *current-collection-name* doc)
      )))

(defun set-entry-field (entry field value)
  (if (listp value)
      (cl-mongo:add-element field value entry)
      (cl-mongo:add-element field (string-trim '(#\Space) value) entry))
  (cl-mongo:db.save *current-collection-name* entry))

(defun clear-entries ()
  (with-check-connection
    (cl-mongo:rm *current-collection-name* :all)))

(defun format-entry (doc)
  (let ((status (cl-mongo:get-element "status" doc))
        (priority (cl-mongo:get-element "priority" doc))
        (heading (cl-mongo:get-element "heading" doc))
        (timestamp (cl-mongo:get-element "timestamp" doc))
        (tags (cl-mongo:get-element "tags" doc)))
    (string-right-trim
     " " (format nil "~a~a~a~a~a"
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
                     (format nil ":~{~a:~}" tags)
                     "")))))

(defparameter *sortby-criterion* "ts_added")

(defun find-entries-sorted (&key (field nil) (value nil))
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
                                            (cl-mongo:kv *sortby-criterion* 1))))
       :limit 0)))))

;;TODO: find less straightforward/more clever way to filter by lists
(defun find-entries-by-list-sorted (&key (field nil) (value nil))
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
                                                   (cl-mongo:kv *sortby-criterion* 1))))
              :limit 0)))))
      (format xmpp:*debug-stream* "~&docs-ordered: ~a" docs-ordered)
      (if (and field value (listp value))
          (remove-if
           (complement
            (lambda (doc)
              (intersection (cl-mongo:get-element field doc) value :test #'equalp))) docs-ordered)
          docs-ordered))))

(defun list-entries (&key (as-org nil) (field nil) (value nil))
  (with-check-connection
      (let ((results (cond ((and value (listp value)) (find-entries-by-list-sorted :field field :value value))
                           (t (find-entries-sorted :field field :value value)))))
        (setf *last-query-result* results)
        results)))

(defun print-entries (entries &key (as-org nil))
  (loop for doc in entries
     for i from 1 to (length entries) collect
       (if as-org
           (concatenate 'string "* " (format-entry doc))
           (concatenate 'string (write-to-string i) ") " (format-entry doc)))))

(defun pick-entry (index)
  (nth (- index 1) (find-entries-sorted)))

(defun drop-entry (entry)
  (let ((formatted (format-entry entry)))
    (cl-mongo:db.delete *current-collection-name* entry)
    formatted))
