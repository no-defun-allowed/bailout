(in-package :bailout)

(defclass problem ()
  ((incidents :initform '()
              :accessor %incidents
              :reader incidents)
   (job        :initarg :job
               :initform (alexandria:required-argument :job)
               :reader job)
   (recoveries :accessor %recoveries
               :reader recoveries)
   (supervisor :initarg :supervisor
               :initform (alexandria:required-argument :supervisor)
               :reader supervisor)))

(defmethod initialize-instance :after ((problem problem) &key)
  (setf (%recoveries problem) (compute-recoveries problem)))

(defgeneric incident-related-p (problem incident)
  (:method ((problem problem) incident)
    nil))

(defgeneric add-incident (problem incident)
  (:method ((problem problem) incident)
    (push incident (%incidents problem))
    t))

(defclass error-problem (problem)
  ((condition-type :initarg :type
                   :reader condition-type)))

(defgeneric problem-from-incident (incident supervisor job)
  (:method ((incident error-signalled) supervisor job)
    (make-instance 'error-problem
                   :type (type-of (error-signalled incident))
                   :job job
                   :supervisor supervisor)))

(define-condition out-of-recoveries (error)
  ((problem :initarg :problem)))

(defgeneric next-recovery (problem)
  (:method ((problem problem))
    (if (null (recoveries problem))
        (error 'out-of-recoveries :problem problem)
        (pop (%recoveries problem)))))
