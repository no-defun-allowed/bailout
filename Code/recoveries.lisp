(in-package :bailout)

(defclass recovery ()
  ((function :initarg :function
             :type function
             :reader recovery-function)
   (name :initarg :name
         :type symbol
         :reader name)
   (priority :initarg :priority
             :type integer
             :reader priority)))

(defmethod print-object ((recovery recovery) stream)
  (print-unreadable-object (recovery stream :type t :identity t)
    (prin1 (name recovery) stream)))

(defvar *recoveries* '())

(defun add-recovery (name priority function)
  (setf *recoveries*
        (cons (make-instance 'recovery
                             :name name
                             :priority priority
                             :function function)
              (remove name *recoveries*
                      :key #'name))))

(defmacro define-recovery ((name priority)
                           (problem options &key (precondition t))
                           &body body)
  (when (listp problem)
    (destructuring-bind (problem problem-class) problem
      (setf precondition `(and (typep ,problem ',problem-class) ,precondition)
            problem problem)))
  `(progn
     (defun ,name (,problem)
       (declare (ignorable ,problem))
       (unless ,precondition
         (not-applicable))
       (lambda (,options)
         (declare (ignorable ,options))
         ,@body))
     (add-recovery ',name ',priority ',name)))

(define-condition not-applicable () ())

(defun not-applicable ()
  (error 'not-applicable))

(defgeneric compute-recoveries (problem)
  (:method ((problem problem))
    (let ((recoveries '()))
      (dolist (recovery *recoveries*)
        (handler-case (funcall (recovery-function recovery)
                               problem)
          (:no-error (function)
            (push (cons function recovery) recoveries))
          (not-applicable ())))
      (sort recoveries #'>
            :key (alexandria:compose #'priority #'cdr)))))

(define-recovery (restart-thread 0)
    (problem options)
  (let ((option (find-option 'restart options)))
    (if (null option)
        (not-applicable)
        (send-option (job problem) option))))
