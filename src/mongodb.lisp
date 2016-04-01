(in-package #:mentat)

(defun init-storage ()
  (with-check-connection
    (cl-mongo:db.use *db-name*)))

(defparameter *current-collection-name* nil)

(defun set-collection-name-by-user (username)
  (setf *current-collection-name*
        (concatenate 'string *entries-collection-prefix* "-" username)))

(defun add-entry (heading &key (status "") (priority "") (timestamp "") (tags ""))
  (with-check-connection
    (let ((doc (cl-mongo:make-document))
          (ts_added (get-universal-time)))
      (when (> (length status) 0) (cl-mongo:add-element "status" (string-trim '(#\Space) status) doc))
      (when (> (length priority) 0) (cl-mongo:add-element "priority" (string-trim '(#\Space) priority) doc))
      (when (> (length heading) 0) (cl-mongo:add-element "heading" (string-trim '(#\Space) heading) doc))
      (when (> (length timestamp) 0) (cl-mongo:add-element "timestamp" (string-trim '(#\Space) timestamp) doc))
      (when (> (length tags) 0) (cl-mongo:add-element "tags" (string-trim '(#\Space) tags) doc))
      (cl-mongo:add-element "ts_added" (write-to-string ts_added) doc)
      (cl-mongo:db.insert *current-collection-name* doc)
      )))

(defun set-entry-field (entry field value)
  (cl-mongo:add-element field (string-trim '(#\Space) value) entry)
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
                     (concatenate 'string tags " ")
                     "")))))

(defparameter *sortby-criterion* "ts_added")

(defun get-entries-sorted ()
  (with-check-connection
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

(defun list-entries (&optional (as-org nil))
  (with-check-connection
    (let ((results (get-entries-sorted)))
      (loop for doc in results
         for i from 1 to 1000 collect
           (if as-org
               (concatenate 'string "* " (format-entry doc))
               (concatenate 'string (write-to-string i) ") " (format-entry doc)))
           ))))

(defun pick-entry (index)
  (nth (- index 1) (get-entries-sorted)))

(defun drop-entry (entry)
  (let* ((entries (get-entries-sorted))
         (entry-to-delete
          (if (integerp entry)
              (nth (- entry 1) entries)
              entry))
         (formatted (format-entry entry-to-delete)))
    (cl-mongo:db.delete *current-collection-name* entry-to-delete)
    formatted))
