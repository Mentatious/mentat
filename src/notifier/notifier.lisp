(in-package #:mentat-notifier)

(defparameter *timers* (make-hash-table :test #'equal))

#+sbcl
(defun main-notifier (argv)
  (declare (ignore argv))
  (setq swank:*use-dedicated-output-stream* nil)
  (swank:create-server
   :port 4008
   :style swank:*communication-style*
   :dont-close t)
  (mentat-util:load-config "notifier-config.lisp")
  (local-time:reread-timezone-repository
   :timezone-repository (pathname *timezone-repository-system-path*))
  (setf local-time:*default-timezone* (local-time:find-timezone-by-location-name *client-timezone*))
  (mentat-db:init-storage *db-name*)
  (cl-cron:make-cron-job 'check-update-notifications :step-min 1)
  (cl-cron:start-cron)
  (bordeaux-threads::join-thread cl-cron::*cron-dispatcher-thread*))

(defun check-update-notifications ()
  (maphash
   (lambda (user entries)
     (dolist (entry entries)
       (let* ((scheduled (mentat-db:get-entry-field entry "scheduled"))
              (deadline (mentat-db:get-entry-field entry "deadline"))
              (id (mentat-db:get-entry-field entry "entry_id")))
         (when scheduled
           (process-timestamp scheduled "scheduled_" id user entry))
         (when deadline
           (process-timestamp deadline "deadline_" id user entry)))))
   (mentat-db:find-all-timestamped-entries-by-collection)))

(defun process-timestamp (timestamp prefix id user entry)
  (let* ((timer-id (concatenate 'string prefix id))
         (existing-timer (gethash timer-id *timers*)))
    (when (mentat-util:timestamp-in-future-p timestamp)
      (unless existing-timer
        (setf existing-timer
              (trivial-timers:make-timer
               #'(lambda () (handle-notification timer-id user entry))
               :name timer-id
               :thread t))
        (setf (gethash timer-id *timers*) existing-timer))
      (unless (trivial-timers:timer-scheduled-p existing-timer)
        (trivial-timers:schedule-timer
         existing-timer
         (mentat-util:adjust-universal-timestamp timestamp :offset-hours 1)
         :absolute-p t)))))

(defun handle-notification (timer-id user entry)
  (mentat-xmpp-engine::connect-and-send user
                                        (mentat-db:format-entry entry)
                                        :resource *xmpp-notifier-resource*)
  (remhash timer-id *timers*))
