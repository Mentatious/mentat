(in-package #:mentat)

(defmacro with-interactive-interrupt (&body body)
  `(handler-case
       (progn ,@body)
     (sb-sys:interactive-interrupt ()
       (format t "Exiting due to an interactive interrupt.~%")
       (sb-ext:exit :code 1))))

(defmacro with-check-connection (&body body)
  `(handler-case
       (progn ,@body)
     (usocket:connection-refused-error (error)
       (format xmpp:*debug-stream* "[check-connection] ERROR: ~a~%Exiting...~%" error))))

(defun load-config (&optional (filename "config.lisp"))
  (handler-case (load filename :verbose t :print t)
    (error (e) (format t "~&Error loading config: ~a" e))))

(defun make-timestamp (timestamp)
  (let* ((tslist (ensure-list timestamp))
         (datepart (car tslist))
         (timepart (cadr tslist))
         components)
    (let ((dateparts (split-sequence:split-sequence #\- datepart)))
      (dolist (part (nreverse dateparts))
        (push (parse-integer part) components)))
    (if timepart
      (let ((timeparts (split-sequence:split-sequence #\: timepart)))
        (dolist (part timeparts)
          (push (parse-integer part) components)))
      (dotimes (x 2)
        (push 0 components)))
    (dotimes (x 2)
      (push 0 components))
    (local-time:timestamp-to-universal
     (apply #'local-time:encode-timestamp components))))

(defun format-timestamp (timestamp &key (as-scheduled nil) (as-deadline nil))
  (let (opening closing)
    (cond (as-scheduled (setf opening "<"
                              closing ">"))
          (as-deadline (setf opening "{"
                              closing "}"))
          (t (setf opening "["
                   closing "]")))
    (multiple-value-bind
          (nsec second minute hour date month year
                day-of-week dst-p tz-off tz-abbr)
        (local-time:decode-timestamp (local-time:universal-to-timestamp timestamp))
      (format nil "~a~2,'0d-~2,'0d-~d ~2,'0d:~2,'0d ~a~a"
              opening
              date
              month
              year
              hour
              minute
              tz-abbr
              closing))))

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

(defun ensure-list (var)
  (if (listp var)
      var
      (list var)))

(defun timestamp-in-future-p (timestamp)
  (local-time:timestamp>
   (local-time:universal-to-timestamp timestamp)
   (local-time:adjust-timestamp (local-time:now) (:offset :hour -1))))

(defun adjust-universal-timestamp (timestamp &key (offset-hours 1))
  (local-time:timestamp-to-universal
   (local-time:adjust-timestamp
       (local-time:universal-to-timestamp timestamp) (:offset :hour offset-hours))))
