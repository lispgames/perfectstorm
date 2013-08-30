(in-package :storm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass a*-path ()
  ((vertices
    :initarg :vertices
    :initform ()
    :reader vertices)
   (expected-costs
    :initarg :expected-costs
    :reader expected-costs)
   (estimated-costs
    :initarg :estimated-costs
    :reader estimated-costs)))

(defmethod print-object ((path a*-path) stream)
  (format stream "{a*-path (costs: ~a): ~a}" (costs path) (mapcar (lambda (v) (center v)) (reverse (vertices path)))))

(defmethod costs ((path a*-path))
  (+ (expected-costs path) (estimated-costs path)))

(defmethod a*-old ((tree quadtree) (start point) (goal point))
  (let ((start-vertex (localize tree start))
	(goal-vertex (localize tree goal))
	(heap (make-heap :compare-function (lambda (p1 p2) (- (+ (expected-costs p1) (estimated-costs p2))
							      (+ (expected-costs p2) (estimated-costs p2)))))))

    ; init a*-data in tree
    (traverse (lambda (vertex)
		(setf (a*-data vertex)
		      (make-instance 'a*-data 
				     :estimated-costs (distance (center vertex) goal)))) tree)
    
    (insert! heap
	     (make-instance 'a*-path 
			    :vertices (list start-vertex)
			    :expected-costs 0
			    :estimated-costs (estimated-costs (a*-data start-vertex))))

    (loop :while (not (empty? heap)) :do
       (let* ((path (top heap))
	      (current-vertex (first (vertices path))))
	 (delete-top! heap)
	 (when (not (closed (a*-data current-vertex)))
	   (when (eq current-vertex goal-vertex)
	     (return-from a*-old path))
	   (setf (closed (a*-data current-vertex)) t)
	   (dolist (edge (incident-edges current-vertex))
	     (let ((neighbour (opposite-vertex edge current-vertex)))
	       (when (not (closed (a*-data neighbour)))
		 (insert! heap
			  (make-instance 'a*-path
					 :vertices (cons neighbour (vertices path))
					 :expected-costs (+ (expected-costs path) (weight edge))
					 :estimated-costs (estimated-costs (a*-data neighbour))))))))))

    (error "a* didn't find a path from ~a to ~a" start goal)))
    		   
#| old triangulation stuff

(defconstant +epsilon+ 0.0001)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Computes the angle theta.
;
;         y
;
;         ^  
;         |  v
;         | /
;         |/ theta
; --------u--------> x 
;         |
;         |
;         |
;
(defmethod fancy-atan ((u point) (v point))
  (mod (- 90 (atan2p (subtract v u))) 360))

; Checks whether v is within the half-space defined by u and angle.
; Returns 1 if it is, 0 if it's a point on the separating straight and -1 if it isn't.
(defmethod within-half-space ((u point) angle (v point))
  (let* ((v<u (fancy-atan u v))
	 (-v<u- (mod (- v<u angle) 360)))
    (if (or (< (abs -v<u-) +epsilon+) (< (abs (- -v<u- 180)) +epsilon+))
	0
      (if (and (<= 0 -v<u-) (<= -v<u- 180))
	  1
	nil))))

(defun elt-mod (sequence index)
  (elt sequence (mod index (length sequence))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass polygon ()
  ((vertices
    :initform ()
    :initarg :vertices
    :accessor vertices)))

(defmethod initialize-instance :after ((polygon polygon) &key points clockwise)
  (when points
    (when clockwise (setf points (reverse points)))
    (let ((vertices ()))
      ;first vertex
      (push (make-instance 'polygon-vertex :x (x (first points)) :y (y (first points))) vertices)

      ; iterate over edges
      (loop
       :for i :from 1 :below (length points) :do
       (let* ((tail (first vertices))
	      (head (make-instance 'polygon-vertex :x (x (elt points i)) :y (y (elt points i))))
	      (edge (make-instance 'polygon-edge :tail tail :head head)))
	 (setf (out-edge tail) edge)
	 (setf (in-edge head) edge)
	 (push head vertices)))

      ; insert edge last -> first
      (let* ((tail (first vertices))
	     (head (car (last vertices)))
	     (edge (make-instance 'polygon-edge :tail tail :head head)))
	(setf (out-edge tail) edge)
	(setf (in-edge head) edge))

      (setf (vertices polygon) (reverse vertices)))))

(defmethod draw ((p polygon))
  (let ((points (vertices p)))
    (loop 
     :for i :from 0 :below (length points) :do
     (let ((l (make-instance 'visible-line :size-y 0.5 :points (list (elt points (mod (- i 1) (length points))) (elt points i)))))
       (draw l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass polygon-vertex (point)
  ((in-edge
    :initform ()
    :initarg :in-edge
    :accessor in-edge)
   (out-edge
    :initform ()
    :initarg :out-edge
    :accessor out-edge)))

(defmethod predecessor ((v polygon-vertex))
  (tail (in-edge v)))

(defmethod successor ((v polygon-vertex))
  (head (out-edge v)))

(defmethod are-neighbours-p ((v polygon-vertex) (w polygon-vertex))
  (or (eq (head (out-edge v)) w)
      (eq (tail (in-edge v)) w)))

(defmethod corner ((v polygon-vertex))
  (list (predecessor v) v (successor v)))

(defmethod corner-orientation ((v polygon-vertex) angle)
  (destructuring-bind (u v w) (corner v)
    (list
     (within-half-space v angle u)
     (within-half-space v angle w))))

(defmethod convex-corner-p ((v polygon-vertex))
  (destructuring-bind (u v w) (corner v)
    (declare (ignore u))
    (find (corner-orientation v (fancy-atan v w)) '((0 0) (1 0)) :test #'equal)))

(defmethod regular-corner-p ((v polygon-vertex))
  (find (corner-orientation v 0) '((-1 1) (1 -1)) :test #'equal))
                                ;'((-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0)) :test #'equal))
    
(defmethod stalagmitic-corner-p ((v polygon-vertex))
  (find (corner-orientation v 0) '((-1 -1)) :test #'equal))
                                ;'((-1 -1) (-1 0) (0 -1) (0 0)) :test #'equal))

(defmethod stalactitic-corner-p ((v polygon-vertex))
  (find (corner-orientation v 0) '((1 1)) :test #'equal))
                                ;'((0 0) (0 1) (1 0) (1 1)) :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass polygon-edge (line)
  ((tail
    :initform (error "Polygon edge without tail")
    :initarg :tail
    :accessor tail)
   (head
    :initform (error "Polygon edge without head")
    :initarg :head
    :accessor head)))
  

(defmethod p1 ((e polygon-edge))
  (tail e))

(defmethod p2 ((e polygon-edge))
  (head e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod trapezoidize ((p polygon))
  (let ((vertices (sort (copy-list (vertices p)) (lambda (v w) (if (= (y v) (y w)) (< (x v) (x w)) (> (y v) (y w))))))
	(finished-trapezoids ())
	(active-trapezoids ())
	(levels (make-hash-map))
	(xmin (- (apply #'min (mapcar x vertices)) 1))
	(xmax (+ (apply #'max (mapcar x vertices)) 1)))

    ; some handy macros
    (macrolet ((find-edge (edge) 
			  `(find ,edge 
				 active-trapezoids 
				 :test (lambda (v) (if (or (= ,edge (in-edge v))
							   (= ,edge (out-edge v)))
						       trapezoid
						     nil))))
	       (remove-active-trapezoid (trapezoid)
					`(setf active-trapezoids (remove ,trapezoid active-trapezoids)))

	       )

      ; init levels
      (dolist (v (reverse vertices))
	(push v (gethash (x v) levels)))

      (dolist (current-vertex vertices)
	(let ((orientation ())
	      (level (gethash (x current-vertex) levels)))
	  (cond
	   ((setf orientation (regular-corner-p current-vertex))
	    ; Type 1: Corner is regular
	    ;         => orientation is in '((-1 1) (1 -1))
	    (let ((active-trapezoid nil))
	      (if (= (first orientation) 1)
		  (setf active-trapezoid (find-trapezoid (in-edge current-vertex)))
		(setf active-trapezoid (find-trapezoid (out-edge current-vertex))))
	      (assert active-trapezoid "Type 1: No active trapezoid found")

	      (if (= (first orientation) 1)
		  ;
		  ;   | in
		  ;   v
		  ;  point - new - >
		  ;   |
		  ;   v out
		  ;
		  (let ((intersected-edge (find-intersecting-edge p (make-line current-vertex xmax))) ; TODO function missing: find-intersecting-edge
			(intersection nil)
			(interesting-vertices nil))
		    (when intersected-edge
		      (setf intersection (nth-value 1 (distance (intersected-edge)
								(make-line current-vertex xmax)))))
		    ; collect interesting vertices TODO
		    ; (loop
		    ; :for i :from (+ 1 (position vertex level)) :to (length level)
		    ; (when (or (not intersection) (< (x (elt level i) (first intersection))))
		    ;   (push (elt level i) interesting-vertices)))
		    ;(setf (interesting-vertices) (reverse interesting-vertices))
		    ; ...
		    
		
		    (let ((new (make-instance 'polygon-vertex :x (first intersection) :y (second intersection))))
		      (if (> (x (head intersected-edge)) (x current-vertex))
			  
		      
		    )
		;
		;         in |
		;            v
		; < - new - point
		;            |
		;        out v
		;
		(progn
		  )

				
		(setf finished-trapezoid (find-trapezoid out-edge)))
	      (remove-trapezoid finished-trapezoid)
	      (push (list finished-trapezoid point) finished-trapezoids)
	      (push point active-trapezoids)))


	   ((stalagmitic-corner-p p point)
	  ; Type 2: Corner is stalagmitic
	  ; test if point is within active trapezoid
	    (let ((trapezoid (within-active-trapezoid point)))
	    (if trapezoid
		(progn ; TODO b0rken
		  (remove-trapezoid trapezoid)
		  (push (list finished-trapezoid point) finished-trapezoids)
		  (push (predecessor point) active-trapezoids)
		  (push (successor point) active-trapezoids)) 
	      (push point active-trapezoids))))
	 ((stalactitic-corner-p p point)
	  
	  ; Type 3: Corner is stalactitic
	  ()
	  )
	 (t (error "Point is neither regular, stalagmitic or stalactitic")))))))
      )


    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar test-polygon-1
  (make-instance 'polygon 
		 :points (list
			  (make-point  0  0)
			  (make-point  6 -1)
			  (make-point 11  1)
			  (make-point  9  5)
			  (make-point  7  4)
			  (make-point  5  5)
			  (make-point  4  4)
			  (make-point  3  2)
			  (make-point  2  1))))

(defvar test-polygon-2
  (make-instance 'polygon
		 :points (list
			  (make-point  0  0)
			  (make-point  1 -2)
			  (make-point  7 -4)
			  (make-point 11  0)
			  (make-point 14 -1)
			  (make-point 12 -3)
			  (make-point 16 -5)
			  (make-point 14 -3)
			  (make-point 18 -1)
			  (make-point 13  1)
			  (make-point 15  2)
			  (make-point  9  4)
			  (make-point  9 -1)
			  (make-point  3 -1)
			  (make-point  4  1)
			  (make-point  8  3)
			  (make-point  2  4))))


(defun test ()
  (setf *units* ()
        *ground-units* ()
        *entities* ()
        *independent-entities* ()
        *moving-entities* () 
        *thinking-entities* ()
        *gui-things* ()
        *textures* (make-hash-table)
        *groups* ()
        *players* ()
        *ticks* 0
        *gl-ticks* 0
        *zoom* 50
        *gui-owner* nil
        *selection-rectangle* nil
        *selected-entities* ()
        *scroll* (make-point 0 0)
        *textures-done* nil
        *fps-display* (make-instance 'text :text "dumdidum" :screen-pos (make-point 10 20))
        *info-display* (make-instance 'text :text "" :screen-pos (make-point 300 20)))

  (grid-init)

  (push test-polygon-2 *gui-things*)

  (glut:display-window (make-instance 'storm-window)))

|#
