(in-package #:mentat-webserver)

#+sbcl
(defun webserver-main ()
  (setq swank:*use-dedicated-output-stream* nil)
  (swank:create-server
   :port 4007
   :style swank:*communication-style*
   :dont-close t)
  (mentat-util:load-config "webserver-config.lisp")
  (mentat-db:init-storage *db-name*)
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
  (mentat-db:set-user-context username *entries-collection-prefix*)
  (let ((entries (mentat-db:print-entries (mentat-db:list-entries) :as-org t)))
    (if (plusp (length entries))
        (format nil "~{~%~a~}" entries)
        (format nil "No entries found."))))
