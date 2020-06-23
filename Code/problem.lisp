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
   (attempted-recoveries :accessor %attempted-recoveries
                         :reader attempted-recoveries)
   (supervisor :initarg :supervisor
               :initform (alexandria:required-argument :supervisor)
               :reader supervisor))
  (:documentation "An aggregation of incidents, which a supervisor tries to alleviate.")) 

(defmethod initialize-instance :after ((problem problem) &key)
  (setf (%recoveries problem) (compute-recoveries problem)))

(defgeneric incident-related-p (problem incident)
  (:method ((problem problem) incident)
    nil))

(defgeneric add-incident (problem incident)
  (:method ((problem problem) incident)
    (incf (%incidents (job incident)))
    (push incident (%incidents problem))
    t))

(defgeneric problem-from-incident (incident supervisor job))

(define-condition out-of-recoveries (error)
  ((problem :initarg :problem)))

(defgeneric next-recovery (problem incident)
  (:method ((problem problem) incident)
    (if (null (recoveries problem))
        (error 'out-of-recoveries :problem problem)
        (let ((recovery (pop (%recoveries problem))))
          (push (list incident recovery) (%attempted-recoveries problem))
          recovery))))
