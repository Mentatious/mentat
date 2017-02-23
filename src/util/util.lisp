(defpackage #:mentat-util
  (:use #:cl)
  (:export #:with-interactive-interrupt
           #:with-check-connection
           #:load-config
           #:format-timestamp
           #:starts-with-nick
           #:ensure-list
           #:timestamp-in-future-p
           #:timestamp-in-past-p
           #:adjust-universal-timestamp
           #:timestamp-is-today-p))

(in-package #:mentat-util)

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
       (format t "[check-connection] ERROR: ~a~%Exiting...~%" error))))

(defun load-config (&optional (filename "config.lisp"))
  (handler-case (load filename :verbose t :print t)
    (error (e) (format t "~&Error loading config: ~a" e))))

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

(defun timestamp-in-future-p (timestamp &key (tz-shift -1) (not-later-than nil))
  (let* ((adjusted-now (local-time:adjust-timestamp (local-time:now) (offset :hour tz-shift)))
        (ts (local-time:adjust-timestamp (local-time:universal-to-timestamp timestamp) (offset :hour tz-shift)))
        (in-future-p (local-time:timestamp> ts adjusted-now)))
    (if not-later-than
        (and in-future-p
             (local-time:timestamp< ts (local-time:universal-to-timestamp not-later-than)))
        in-future-p)))

(defun timestamp-in-past-p (timestamp &key (tz-shift -1) (not-earlier-than nil))
  (let* ((adjusted-now (local-time:adjust-timestamp (local-time:now) (offset :hour tz-shift)))
        (ts (local-time:adjust-timestamp (local-time:universal-to-timestamp timestamp) (offset :hour tz-shift)))
        (in-past-p (local-time:timestamp< ts adjusted-now)))
    (if not-earlier-than
        (and in-past-p
             (local-time:timestamp> ts (local-time:universal-to-timestamp not-earlier-than)))
        in-past-p)))

(defun adjust-universal-timestamp (timestamp &key (offset-minutes 0) (offset-hours 0) (offset-days 0))
  (local-time:timestamp-to-universal
   (local-time:adjust-timestamp
       (local-time:universal-to-timestamp timestamp)
     (offset :minute offset-minutes)
     (offset :hour offset-hours)
     (offset :day offset-days))))

(defun timestamp-is-today-p (timestamp)
  (when timestamp
    (let* ((today-ts (local-time:adjust-timestamp (local-time:today) (offset :hour 1))) ;TODO: make offset an explicit paramater
           (tomorrow-ts
            (local-time:adjust-timestamp today-ts
              (offset :hour 1)
              (offset :day 1)))) ;FIXME: find how to convert from GMT to timezone
      (local-time:timestamp<=
       today-ts
       (local-time:universal-to-timestamp timestamp)
       tomorrow-ts))))
