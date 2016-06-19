(in-package #:mentat-webserver)

#+sbcl
(defun webserver-main ()
  (mentat::load-config)
  (mentat::init-storage)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (sb-thread:join-thread
   (find-if
    (lambda (th)
      (string= (sb-thread:thread-name th) "hunchentoot-listener-*:4242"))
    (sb-thread:list-all-threads))))

#+sbcl
(defun save-image-webserver ()
  (swank-loader::init :load-contribs t)
  (sb-ext:save-lisp-and-die "mentat-webserver"
                            :compression t
                            :executable t
                            :toplevel #'webserver-main))

(hunchentoot:define-easy-handler (test-uri :uri "/mentat.org"
                                           :default-request-type :get) (username)
  (setf (hunchentoot:content-type*) "text/plain")
  (let ((mentat::*current-collection-name*
         (concatenate 'string mentat::*entries-collection-prefix* "-" username)))
    (let ((entries (mentat::list-entries :as-org t)))
      (if (> (length entries) 0)
          (format nil "~{~%~a~}" entries)
          (format nil "No entries found.")))))
