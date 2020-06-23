(in-package :bailout)

(defclass supervisor (job)
  ((jobs :initarg :jobs
         :initform '()
         :accessor %jobs
         :reader jobs)
   (incident-mailbox :initform (safe-queue:make-mailbox)
                    :reader incident-mailbox)
   (problems :initform '()
             :reader problems
             :accessor %problems)))

(defmethod stop-job :after ((supervisor supervisor))
  (mapc #'stop-job (jobs supervisor)))

(defgeneric file-incident (supervisor incident)
  (:method ((supervisor supervisor) incident)
    (safe-queue:mailbox-send-message (incident-mailbox supervisor)
                                     incident)
    t))

(defgeneric handle-message (supervisor message)
  (:method ((supervisor supervisor) incident)
    (let ((problem (some (lambda (problem)
                           (incident-related-p problem incident))
                         (problems supervisor))))
      (when (null problem)
        (setf problem (problem-from-incident incident
                                             supervisor
                                             (job incident)))
        (push problem (%problems supervisor)))
      (add-incident problem incident)
      (tagbody
       try-again
         (let ((options (options incident)))
           (destructuring-bind (function . recovery)
               (next-recovery problem)
             (format *debug-io* "Attempting ~s to recover from ~s (in ~s)~%"
                     (name recovery) incident problem)
             (handler-case (funcall function options)
               (not-applicable ()
                 (go try-again)))
             (format *debug-io* "Attempted ~s"
                     (name recovery))))))))

(defmethod run-job ((supervisor supervisor))
  (invoke-in-job-context supervisor
   (lambda ()
     (loop with mailbox = (incident-mailbox supervisor)
           for message = (safe-queue:mailbox-receive-message mailbox)
           do (handle-message supervisor message)))))

(defgeneric add-job    (supervisor job)
  (:method ((supervisor supervisor) job)
    (setf (%supervisor job) supervisor)
    (push job (%jobs supervisor))
    (start-job job)))
(defgeneric remove-job (supervisor job)
  (:method ((supervisor supervisor) job)
    (stop-job job)
    (setf (%jobs supervisor)
          (remove job (%jobs supervisor)))))
