(defun random-pn-float (limit)
  "Random positive or negative float"
  (- (random (* 2.0 limit)) limit))

(defclass gobject ()
  ((x :accessor x :initarg :x :initform (random-pn-float 10.0))
   (y :accessor y :initarg :y :initform (random-pn-float 10.0))
   (vx :accessor vx :initarg :vx :initform 0.0)
   (vy :accessor vy :initarg :vy :initform 0.0)
   (fx :accessor fx :initarg :vx :initform 0.0)
   (fy :accessor fy :initarg :vy :initform 0.0)))

(defmethod dist-sqr ((g1 gobject) (g2 gobject))
  (let ((g1x (x g1))
	(g2x (x g2))
	(g1y (y g1))
	(g2y (y g2)))
    (+ (* (- g1x g2x) (- g1x g2x))
       (* (- g1y g2y) (- g1y g2y)))))

(defmethod dist ((g1 gobject) (g2 gobject))
  (sqrt (dist-sqr g1 g2)))

(defmethod dist-vect ((g1 gobject) (g2 gobject))
  (let ((g1x (x g1))
	(g2x (x g2))
	(g1y (y g1))
	(g2y (y g2)))
    (complex (- g2x g1x) (- g2y g1y))))

(defparameter *friction* 0.99)

(defmethod dynamics-step ((g gobject) (timestep float))
  (incf (vx g) (* (fx g) timestep))
  (incf (vy g) (* (fy g) timestep))
  (incf (x g) (* (vx g) timestep))
  (incf (y g) (* (vy g) timestep))
  (setf (vx g) (* (vx g) *friction*))
  (setf (vy g) (* (vy g) *friction*))
  (setf (fx g) 0)
  (setf (fy g) 0))

(defmethod set-force ((g gobject) fcomplex)
  (setf (fx g) (realpart fcomplex))
  (setf (fy g) (imagpart fcomplex)))

(defclass node (gobject)
  ((id :initarg :id :reader id)
   (mass :initarg :mass :initform (+ (random 1.0) 1.0) :reader mass)))

(defun print-edge-dests (v)
  (cl-graph:iterate-edges v
			  (lambda (e)
			    (format t "~A," (cl-graph:target-vertex e)))))

(defun print-graph (g)
  (cl-graph:iterate-vertexes g
			     (lambda (v)
			       (progn
				 (format t "~A has edges: " v)
				 (print-edge-dests v)
				 (terpri)))))

(defun find-node-id (g target-id)
  (cl-graph:element (cl-graph:find-vertex-if g (lambda (v)
						 (eq (slot-value (cl-graph:element v) 'id) target-id)))))

(defun find-vertex-with-id (g target-id)
  (cl-graph:find-vertex-if g (lambda (v)
			       (eq (slot-value (cl-graph:element v) 'id) target-id))))

(defun force (ff vertex)
  "Iterate over all peers of the node and compute total force at distances using ff"
  (let ((forcesum (complex 0 0)))
    (cl-graph:iterate-source-edges vertex
				   (lambda (e)
				     (let* ((ns (cl-graph:element (cl-graph:source-vertex e)))
					    (distv (dist-vect ns
							      (cl-graph:element (cl-graph:target-vertex e)))))
				       (setf forcesum (/ (+ forcesum (funcall ff distv)) (mass ns))))))
    forcesum))

(defun set-force-on-node (ff vertex)
  (set-force (cl-graph:element vertex) (force ff vertex)))

(defun graph-force-dynamics (ff g time)
  (cl-graph:iterate-vertexes g (lambda (v) (set-force-on-node ff v)))
  (cl-graph:iterate-vertexes g (lambda (v) (dynamics-step (cl-graph:element v) time))))

(defun test-force-func (breakpoint distv)
  (let* ((xd (realpart distv))
	 (yd (imagpart distv))
	 (xa (abs xd))
	 (ya (abs yd)))
    (complex (if (> xa breakpoint) xd (- 0 xd))
	     (if (> ya breakpoint) yd (- 0 yd)))))

(defparameter force1 (alexandria:curry #'test-force-func 1))
(defparameter force5 (alexandria:curry #'test-force-func 5))

(defun generate-random-graph (vertex-count edges-per-vertex)
  (let ((vs (make-array vertex-count))
	(g (cl-graph:make-graph 'cl-graph:dot-graph :default-edge-type :directed)))
    ; Create the vertices
    (loop for i from 1 upto vertex-count do
      (let ((n (make-instance 'node :id i)))
	(cl-graph:add-vertex g n)
	(setf (aref vs (- i 1)) n)))
    ; Create the edges for all vertices
    (loop for si from 0 upto (- vertex-count 1) do
      ; Create all edges for the vertex connections4
      (loop for ei from 1 upto edges-per-vertex do
	(let ((di (random vertex-count)))
	  (cl-graph:add-edge-between-vertexes g
					      (aref vs di)
					      (aref vs si))
	  (cl-graph:add-edge-between-vertexes g
					      (aref vs si)
					      (aref vs di)))))
    g))
