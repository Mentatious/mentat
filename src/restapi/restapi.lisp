(in-package #:mentat-restapi)

(defparameter *entries* nil)

(defparameter *app* (make-instance 'ningle:<app>))

;;FIXME: fix NIH
(defun get-request-raw-body ()
  (let* ((len (lack.request:request-content-length ningle:*request*))
         (str (make-string len)))
    (read-sequence str (lack.request:request-raw-body ningle:*request*))
    str))

(setf (ningle:route *app* "/entries" :method :get)
      #'(lambda (params)
          (format nil "~{~%~a~}" *entries*)))

(setf (ningle:route *app* "/entries" :method :post)
      #'(lambda (params)
          (let ((payload (get-request-raw-body)))
            (format t "payload: ~a" payload)
            (push payload *entries*)
            "ok")))

(setf (ningle:route *app* "/entries" :method :delete)
      #'(lambda (params)
          (setf *entries* nil)
          "ok"))

#+sbcl
(defun restapi-main ()
  (setq swank:*use-dedicated-output-stream* nil)
  (swank:create-server
   :port 4009
   :style swank:*communication-style*
   :dont-close t)
  (clack:clackup *app* :port 9003)
  (sb-thread:join-thread
   (find-if
    (lambda (th)
      (string= (sb-thread:thread-name th) "clack-handler-hunchentoot"))
    (sb-thread:list-all-threads))))

#+sbcl
(defun save-image-restapi ()
  (swank-loader::init :load-contribs t)
  (sb-ext:save-lisp-and-die "mentat-restapi"
                            :compression t
                            :executable t
                            :toplevel #'restapi-main))
