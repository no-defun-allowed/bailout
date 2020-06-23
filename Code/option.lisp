(in-package :bailout)

(defclass option ()
  ((job :initarg :job
        :reader job)
   (name :initarg :name
         :reader name)
   (valid-p :initform t
            :accessor %valid-p
            :reader valid-p))
  (:documentation "An option that the supervisor can choose when a job has an incident."))

(defgeneric compute-options (job incident &key &allow-other-keys))
(defgeneric invoke-option (option))
(defgeneric invalidate-option (option)
  (:method ((option option))
    (setf (%valid-p option) nil)))

(defclass nonlocal-transfer (option)
  ((continuation :initarg :continuation
                 :reader continuation)))
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
