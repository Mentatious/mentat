(in-package #:mentat)

(defun init-storage ()
  (with-check-connection
    (cl-mongo:db.use *db-name*)))

(defun add-entry (heading &key (status "") (priority "") (timestamp "") (tags ""))
  (with-check-connection
    (let ((doc (cl-mongo:make-document)))
      (when (> (length status) 0) (cl-mongo:add-element "status" (string-trim '(#\Space) status) doc))
      (when (> (length priority) 0) (cl-mongo:add-element "priority" (string-trim '(#\Space) priority) doc))
      (when (> (length heading) 0) (cl-mongo:add-element "heading" (string-trim '(#\Space) heading) doc))
      (when (> (length timestamp) 0) (cl-mongo:add-element "timestamp" (string-trim '(#\Space) timestamp) doc))
      (when (> (length tags) 0) (cl-mongo:add-element "tags" (string-trim '(#\Space) tags) doc))
      (cl-mongo:db.insert *collection-name* doc)
      )))

(defun clear-entries ()
  (with-check-connection
    (cl-mongo:rm *collection-name* :all)))

(defun format-entry (doc)
  (let ((status (cl-mongo:get-element "status" doc))
        (priority (cl-mongo:get-element "priority" doc))
        (heading (cl-mongo:get-element "heading" doc))
        (timestamp (cl-mongo:get-element "timestamp" doc))
        (tags (cl-mongo:get-element "tags" doc)))
    (format nil "~a~a~a~a~a"
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
                ""))))

(defun get-entries-sorted ()
  (with-check-connection
    (cl-mongo:docs
     (cl-mongo:iter
      (cl-mongo:db.find
       *collection-name*
       (cl-mongo:kv
        (cl-mongo:kv "query"
                     (cl-mongo:kv nil nil))
        (cl-mongo:kv "orderby" (cl-mongo:kv (cl-mongo:kv "db" 1) (cl-mongo:kv "id" 1))))
       :limit 0)))))

(defun list-entries ()
  (with-check-connection
    (let ((results (get-entries-sorted)))
      (loop for doc in results
         for i from 1 to 1000
         collect
           (concatenate 'string (write-to-string i) ") " (format-entry doc))))))

(defun drop-entry (index)
  (let* ((entries (get-entries-sorted))
         (entry (nth (- index 1) entries))
         (formatted (format-entry entry)))
    (cl-mongo:db.delete *collection-name* entry)
    formatted))
