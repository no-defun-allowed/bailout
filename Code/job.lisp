(in-package :bailout)

(defclass job ()
  ((supervisor :initarg :supervisor
               :initform nil
               :type (or null supervisor)
               :accessor %supervisor
               :reader supervisor)
   (thread     :reader thread
               :initform nil
               :type (or null bt:thread)
               :accessor %thread)
   (incidents  :initform 0
               :accessor %incidents
               :reader incidents)
   (response-mailbox
    :initform (safe-queue:make-mailbox :name "Response mailbox")
    :reader response-mailbox)
   (associated-states :initarg :associated-states
                      :initform '()
                      :type list
                      :reader associated-states))
  (:documentation "A representation of a task, which is nominally supervised by a supervisor."))

(defgeneric job-error-handler (job
                               &key restart-continuation exit-continuation
                               &allow-other-keys)
  (:method ((job job) &key restart-continuation exit-continuation)
    (lambda (error)
      (let* ((incident (make-instance 'error-signalled
                                      :job job
                                      :error error))
             (options  (compute-options job incident
                        :restart-continuation restart-continuation
                        :exit-continuation exit-continuation)))
        (setf (%options incident) options)
        (file-incident (supervisor job) incident)
        (let ((option (safe-queue:mailbox-receive-message
                       (response-mailbox job))))
          (assert (eq job (job option)))
          (assert (valid-p option))
          (mapc #'invalidate-option options)
          (invoke-option option))))))
        
(defgeneric invoke-in-job-context (job function)
  (:method ((job job) function)
    (catch 'out
      (if (null (supervisor job))
          ;; Just call the function. If we don't have a supervisor, the operator
          ;; is the supervisor. 
          (funcall function)
          (loop
            (block work
              (handler-bind ((error
                               (job-error-handler job
                                :exit-continuation (lambda ()
                                                     (throw 'out nil))
                                :restart-continuation (lambda ()
                                                        (return-from work)))))
                (funcall function)
                (return))))))))

(defclass function-job (job)
  ((function :initarg :function :reader job-function))
  (:documentation "A job with a function as its work."))

(defgeneric run-job (job)
  (:method ((job function-job))
    (invoke-in-job-context job (job-function job))))

(defgeneric start-job (job)
  (:method ((job job))
    (setf (%thread job)
          (bt:make-thread (lambda ()
                            (run-job job))))))
(defgeneric stop-job (job)
  (:method ((job job))
    (when (null (thread job))
      (error "The job has already been stopped."))
    (ignore-errors
     (bt:interrupt-thread (thread job)
                          (lambda ()
                            (throw 'out nil))))
    (setf (%thread job) nil)))

(defgeneric send-option (job option)
  (:method ((job job) option)
    (safe-queue:mailbox-send-message (response-mailbox job)
                                     option)))
