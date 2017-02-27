(in-package #:mentat-restapi)

;; (drakma:http-request "http://octocat.ru:9003/entries"
;;                      :method :post
;;                      :external-format-out :utf-8
;;                      :content "тест2")

(defparameter *entries* nil)

#+sbcl
(defun restapi-main ()
  (setq swank:*use-dedicated-output-stream* nil)
  (swank:create-server
   :port 4009
   :style swank:*communication-style*
   :dont-close t)
  (mentat-util:load-config "restapi-config.lisp")
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 9003))
  (sb-thread:join-thread
   (find-if
    (lambda (th)
      (string= (sb-thread:thread-name th) "hunchentoot-listener-*:9003"))
    (sb-thread:list-all-threads))))

(hunchentoot:define-easy-handler (api-endpoint :uri "/api/v1/"
                                           :default-request-type :post) ()
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string '((result . ok))))

(hunchentoot:define-easy-handler (entries :uri "/entries"
                                           :default-request-type :get) ()
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string `((entries . ,*entries*) (result . ok))))

;; TODO: reimplement "/entries" for POST

(defun save-image-restapi ()
  (swank-loader::init :load-contribs t)
  (sb-ext:save-lisp-and-die "mentat-restapi"
                            :compression t
                            :executable t
                            :toplevel #'restapi-main))
