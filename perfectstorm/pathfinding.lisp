(in-package :storm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass path ()
  ((waypoints
    :initarg :waypoints
    :initform ()
    :reader waypoints)
   (costs
    :initarg :costs
    :initform 0
    :reader costs)))

(defclass waypoint ()
  ((location
    :initarg :location
    :reader location)
   ))

(defmethod print-object ((wp waypoint) stream)
  (format stream "{waypoint: ~a}" (location wp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass a*-data ()
  ((closed
    :initform nil
    :accessor closed)
   (estimated-costs
    :initarg :estimated-costs
    :initform (error "bla")
    :reader estimated-costs)))

(defclass a*-path ()
  ((current-location
    :initarg :current-location
    :reader current-location)
   (vertices
    :initarg :vertices
    :reader vertices)
   (edges
    :initarg :edges
    :reader edges)
   (existing-costs
    :initarg :existing-costs
    :reader existing-costs)
   (estimated-costs
   :initarg :estimated-costs
   :reader estimated-costs)))

(defmethod a*-extend-path ((tree quadtree) (goal vektor) (a*-path a*-path) (heap heap))
  (let ((current-vertex (first (vertices a*-path))))
    (quadtree-vertex-with-neighbours current-vertex
      (when (not (closed (a*-data neighbour)))
	(let ((new-location (margin-crossing-point edge)))
	  (insert! heap (make-instance 'a*-path
				       :current-location new-location
				       :vertices         (cons neighbour (vertices a*-path))
				       :edges            (cons edge (edges a*-path))
				       :existing-costs   (+ (distance new-location
								      (current-location a*-path))
							    (existing-costs a*-path))
				       :estimated-costs  (distance new-location goal))))))))

(defmethod a*-find-path ((tree quadtree) (start vektor) (goal vektor) (goal-vertices sequence) (heap heap))
  (when (empty? heap)
    (error "a* didn't find a path from ~a to ~a" start goal))
    
  (let* ((a*-path (top heap))                         ; cheapest of all active paths
	 (current-vertex (first (vertices a*-path)))) ; current last vertex of path   
    (delete-top! heap)

    (when (not (closed (a*-data current-vertex)))
      (if (find current-vertex goal-vertices)
	  (return-from a*-find-path a*-path)
	(progn
	  (setf (closed (a*-data current-vertex)) t)
	  (a*-extend-path tree goal a*-path heap))))

    (a*-find-path tree start goal goal-vertices heap)))

(defmethod a* ((tree quadtree) (start vektor) (goal vektor))
  ;init a*-data
  (traverse (lambda (vertex)
	      (setf (a*-data vertex) 
		    (make-instance 'a*-data
				   :estimated-costs 0))) ; not used atm: (distance (center vertex) goal))))
	    tree)

  (let* ((start-vertex (localize tree start))
	 (goal-vertex (localize tree goal))
	 (goal-vertices (list goal-vertex))
	 (heap (make-heap :compare-function (lambda (p1 p2) 
					      (- (+ (existing-costs p1) (estimated-costs p1))
						 (+ (existing-costs p2) (estimated-costs p2)))))))

    ; add start-vertex
    (insert! heap (make-instance 'a*-path
				 :current-location start
				 :vertices         (list start-vertex)
				 :edges            ()
				 :existing-costs   0
				 :estimated-costs  (distance start goal)))

    ; extended initial heap (neighbours of start-vertex are considered too)
    (quadtree-vertex-with-neighbours start-vertex
      (when (star-point? start-vertex neighbour start)
	(insert! heap (make-instance 'a*-path
				     :current-location start
				     :vertices         (list neighbour start-vertex)
				     :edges            ()
				     :existing-costs   0
				     :estimated-costs  (distance start goal)))))

    ; extended list of goal vertices (neighbours of goal-vertex are consideres too)
    (quadtree-vertex-with-neighbours goal-vertex
      (when (star-point? goal-vertex neighbour goal)
	(push neighbour goal-vertices)))

    (let ((a*-path (a*-find-path tree start goal goal-vertices heap)))
      ; TODO string-pulling
      (make-instance 'path
		     :waypoints (reverse (cons (make-instance 'waypoint :location goal)
					       (mapcar (lambda (e) 
							 (make-instance 'waypoint 
									:location (margin-crossing-point e)))
						       (edges a*-path)))))
      )))
