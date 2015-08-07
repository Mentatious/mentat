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
       (format t "ERROR: ~a~%Exiting...~%" error))))

(defun load-config (&optional (filename "config.lisp"))
  (handler-case (load filename :verbose t :print t)
    (error (e) (format t "~&Error loading config: ~a" e))))
