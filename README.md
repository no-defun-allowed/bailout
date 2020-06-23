# bailout

> I decided in the shower that a library that tries to fix up state when 
> something goes wrong, through possibly clumsy strategies, should be called
> *bailout*.

We would like to tell the computer how it should go about trying to diagnose
and recover from problems we never anticipated. 

This has almost been done before; 
[supervisor trees](https://erlang.org/doc/design_principles/des_princ.html)
in Erlang can be used to restart workers if they have problems, but supervisor
trees alone may not suffice, in a language with mutable state that is detached
from threads, like Common Lisp. We also try to integrate with the Lisp condition
system, in case the client provides restarts, or other ways to continue from an
error.

### Structure

A *supervisor* attempts to make sure some *jobs* run properly. When a job 
doesn't run properly, the supervisor is informed about an *incident*, and it 
creates a new *problem*, or adds the incident to a currently existing problem.

The supervisor then computes a list of *recoveries* it may be able to use to
recover from the problem, such as restarting the thread, or sending mail to the
operator to tell them to investigate. The problem is then solved, if related
incidents don't appear for some time.

> Fuck it, I'll turn this video off, and go home, take my nine nines with me.

```lisp
(defvar *supervisor* (make-instance 'supervisor))

(start-job *supervisor*)
(add-job *supervisor*
         (make-instance 'function-job
                        :function (lambda ()
                                    (loop for number = (random 10)
                                          if (zerop number)
                                            do (error "I don't like zeroes")
                                          else
                                            do (print number)
                                          do (sleep 0.3)))))
```
