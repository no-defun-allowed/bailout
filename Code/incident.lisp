(in-package :bailout)

(defclass incident ()
  ((job :initarg :job :reader job)
   (options :accessor %options
            :reader options)))
