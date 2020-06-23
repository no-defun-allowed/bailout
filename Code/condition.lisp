(in-package :bailout)

(defclass error-signalled (incident)
  ((error :initarg :error :reader error-signalled)))

(defclass error-problem (problem)
  ((condition-type :initarg :type
                   :reader condition-type)))

(defmethod incident-related-p ((problem error-problem) (incident error-signalled))
  (equal (type-of (error-signalled incident))
         (condition-type problem)))
 
(defmethod problem-from-incident ((incident error-signalled) supervisor job)
  (make-instance 'error-problem
   :type (type-of (error-signalled incident))
   :job job
   :supervisor supervisor))

(defclass restart-transfer (option)
  ((restart :initarg :restart :reader transfer-restart))
  (:documentation "An option that invokes a restart."))

(defmethod compute-options ((job job)
                            (incident error-signalled)
                            &key)
  (append (loop for restart in (compute-restarts (error-signalled incident))
                collect (make-instance 'restart-transfer
                                       :restart restart
                                       :name (restart-name restart)
                                       :job job))
          (call-next-method)))
