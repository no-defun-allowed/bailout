(defpackage :bailout
  (:use :cl)
  (:export #:job #:supervisor #:function-job
           #:start-job #:stop-job
           #:add-job #:remove-job
           #:define-recovery #:not-applicable
           #:problem #:incident #:recovery #:incident #:state
           #:write-supervisor-tree))
