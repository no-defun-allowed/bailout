(defpackage :bailout
  (:use :cl)
  (:export #:job #:supervisor #:function-job
           #:start-job #:stop-job
           #:problem #:incident #:recovery #:incident #:state))
