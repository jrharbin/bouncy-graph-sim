					; Factor out the paths

(defparameter *node-filename* (merge-pathnames "test.bmp" #.*compile-file-pathname*))
(defparameter *node-pic* (sdl2:load-bmp *node-filename*))
(defvar *background-rgb*)

(defparameter *frame-delay* 0.01)
(defparameter *tdelta* *frame-delay*)

(defparameter *update-dynamics* t)

(defparameter *force-func* force5)

; Factor out the paths
; (defparameter *f* (sdl2-ttf:open-font "/usr/share/fonts/TTF/luximr.ttf" 24))
;(sdl2-ttf:render-text-blended *f* "Testing" 0 0 0 255) - generates a surface

(defvar last-timestamp)

(defparameter *active* nil)

(defun setup-new-window ()
  (sdl2:init :video)
  (let* ((w (sdl2:create-window :title "Random Graph Simulation"))
	 (s (sdl2:get-window-surface w)))
    ; Set background color to white
    (setf *background-rgb* (sdl2:map-rgb (sdl2:surface-format s) 255 255 255))
    (values w s)))

(defmethod render-node ((node node) window surface)
  ; Rewrite this code for proper mapping
  (let ((x (coerce (round (+ 300 (* 25 (slot-value node 'x)))) 'integer))
	(y (coerce (round (+ 300 (* 25 (slot-value node 'y)))) 'integer)))
    (sdl2:blit-surface *node-pic* (sdl2:make-rect 0 0 32 32)
			   surface (sdl2:make-rect x y 32 32))))

(defmethod render-graph ((graph cl-graph:dot-graph) window surface)
  (cl-graph:iterate-vertexes graph
			     (lambda (v)
			       (render-node (cl-graph:element v) window surface))))

(defun render-all (window surface)
  (sdl2:fill-rect surface nil *background-rgb*)
  (render-graph *g* window surface)
  (sdl2:update-window window))

(defun run-sdl-thread ()
  (setf *active* t)
  (bt:make-thread (lambda ()
		    (multiple-value-bind (w s) (setup-new-window)
		      (loop while *active* do 
			(render-all w s)
			(if (> *frame-delay* 1e-6) (sleep *frame-delay*))
			(if *update-dynamics* (graph-force-dynamics *force-func* *g* *tdelta*)))
		      (sdl2:destroy-window w)
		      (sdl2:quit))
		    :name "GUI Rendering Thread")))

(defun start-gui ()
  (run-sdl-thread))

(defun quit-gui ()
  (setf *active* nil))

(defun stop-gui ()
  (quit-gui))

