(defun set-force-breakpoint (dist)
  (setf *force-func* (alexandria:curry #'test-force-func dist)))

(defun new-graph ()
  (defparameter *g* (generate-random-graph 400 15)))

(defun start-sim ()
  (defparameter *force-func* (alexandria:curry #'test-force-func 5))
  (defparameter *g* (generate-random-graph 400 15))
  (setf *friction* 0.99)
  (start-gui))
