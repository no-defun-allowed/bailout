(in-package :bailout)

;;; Instead of the supervisor having arbitrary access to its jobs' dynamic
;;; environments, job threads present the supervisor with a list of options,
;;; which the supervisor should choose one of.
;;; This is not unlike the protocol proposed between a debugger and its
;;; application in "Omnipresent and low-overhead application debugging" by
;;; Robert Strandh.

(defclass option ()
  ((job :initarg :job
        :reader job)
   (name :initarg :name
         :reader name)
   (valid-p :initform t
            :accessor %valid-p
            :reader valid-p))
  (:documentation "An option, which a supervisor can choose when a job has an incident."))

(defgeneric compute-options (job incident &key &allow-other-keys)
  (:documentation "Compute a list of options that the supervisor may choose from to alleviate the job of an incident."))
(defgeneric invoke-option (option)
  (:documentation "Perform the action associated with this option. This should only be called by the job that created the option."))

(defgeneric invalidate-option (option)
  (:method ((option option))
    (setf (%valid-p option) nil)))

(defclass nonlocal-transfer (option)
  ((continuation :initarg :continuation
                 :reader continuation))
  (:documentation "An option that causes some generic non-local transfer of control, by calling a function that performs such a transfer."))
(defmethod invoke-option ((option nonlocal-transfer))
  (funcall (continuation option)))

(defmethod compute-options ((job job) incident
                            &key exit-continuation restart-continuation)
  (list (make-instance 'nonlocal-transfer
                       :continuation restart-continuation
                       :name 'restart
                       :job job)
        (make-instance 'nonlocal-transfer
                       :continuation exit-continuation
                       :name 'exit
                       :job job)))

(defun find-option (name options)
  (find name options :key #'name))
