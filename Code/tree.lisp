(in-package :bailout)

(defun write-supervisor-tree (supervisor output)
  "Render the supervisor tree as a Graphviz document." 
  (etypecase output
    (stream (%write-supervisor-tree supervisor output))
    ((or pathname string)
     (with-open-file (stream output
                             :direction :output
                             :if-exists :rename)
       (%write-supervisor-tree supervisor stream)))))

(defun %write-supervisor-tree (supervisor stream)
  (write-line "digraph {" stream)
  (write-tree-edge supervisor stream)
  (write-line "}" stream))

(defgeneric write-tree-edge (job stream)
  (:method ((supervisor supervisor) stream)
    (dolist (job (jobs supervisor))
      (format stream "~&~s -> ~s~%" supervisor job)
      (write-tree-edge job stream)))
  (:method ((job job) stream)))
