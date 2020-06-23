(in-package :bailout)

(defclass incident ()
  ((job :initarg :job :reader job)
   (options :accessor %options
            :reader options)))

(defclass error-signalled (incident)
  ((error :initarg :error :reader error-signalled)))
