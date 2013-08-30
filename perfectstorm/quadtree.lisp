(in-package :storm)

(defparameter *quadtree-default-depth* 6)
(defparameter *quadtree-epsilon* 0.0000023)

; new generics 

(defgeneric localize (object1 object2))

; for vertices
(defgeneric lines (vertex))                     ;
(defgeneric add-neighbour (vertex1 vertex2))    ;
(defgeneric remove-neighbour (vertex1 vertex2)) ;
(defgeneric compare (vertex1 vertex2))          ; 
(defgeneric common-margin (vertex1 vertex2))    ; returns the interection of the margins of vertex1 and vertex2 (as `line` object)
(defgeneric split! (vertex &key))               ;

; for edges
(defgeneric weight (edge))
(defgeneric opposite-vertex (edge vertex))
(defgeneric connects-vertices? (edge vertex1 vertex2))

;;; some functions for debugging

(defun quadtree-test ()
  (let* ((objects 
	 (list
	  (make-instance 'polygon :corners (list (make-point 50 30)
						 (make-point 30 50)
						 (make-point 50 70)
						 (make-point 70 50)))
	  (make-instance 'polygon :corners (list (make-point   0 80)
						 (make-point  10 50)
						 (make-point -10 10)))
	  (make-instance 'polygon :corners (list (make-point  -40 -40)
						 (make-point -160 30)
						 (make-point -150 -100)
						 (make-point -100 -50)))
	  (make-instance 'polygon :corners (list (make-point  15  0)
						 (make-point 40  0)
						 (make-point 25 -30)
						 (make-point 80 -40)
						 (make-point 60 10)
						 (make-point 90 -30)
						 (make-point 100 -100)
						 (make-point 30 -80))))))
    (make-instance 'quadtree 
		   :bottom-left-corner (make-point -200 -200)
		   :width 400
		   :height 400
		   :objects objects)))

; TODO this should be somewhere else
(defun objects-draw (objects)
  (dolist (o objects)
    (loop :for i :below (length (corners o)) :do
       (push (make-instance 'visible-line 
			    :size-y 0.2
			    :points (list (elt (corners o) i)
					  (elt-mod (corners o) (+ i 1))))
	     *gui-things*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass quadtree () 
  ((root
    :reader root)
   (display-list
    :initform nil
    :accessor display-list)))

(defmethod initialize-instance :after ((quadtree quadtree) 
				       &key 
				       (bounding-box nil)
				       (bottom-left-corner nil) (width 0) (height 0)
				       (objects ())
				       (quadtree-depth *quadtree-default-depth*))

  ; initialize quadtree from bounding-box or bottom-left-corner, width and height
  (setf (slot-value quadtree 'root)
	(make-instance 
	 'quadtree-vertex
	 :parent  nil
	 :corners (cond
		   (bounding-box 
		    (list
		     (make-point (xmin bounding-box) (ymax bounding-box))
		     (make-point (xmin bounding-box) (ymin bounding-box))
		     (make-point (xmax bounding-box) (ymin bounding-box))
		     (make-point (xmax bounding-box) (ymax bounding-box))))
		   (bottom-left-corner
		    (list
		     (make-point (x bottom-left-corner) (+ (y bottom-left-corner) height))
		     (make-point (x bottom-left-corner) (y bottom-left-corner))
		     (make-point (+ (x bottom-left-corner) width) (y bottom-left-corner))
		     (make-point (+ (x bottom-left-corner) width) (+ (y bottom-left-corner) height))))
		   (t
		    (error "you have to initialize the quadtree with a bounding-box or a bottom-left-corner, width and height")))))
  
  (setf (objects (root quadtree)) objects)

  ; build up tree
  (split! (root quadtree) :quadtree-depth quadtree-depth))

; throws exception, if point not in quadtree, returns leaf else
(defmethod localize ((quadtree quadtree) (point vektor))
  (let ((result (localize (root quadtree) point)))
    (if result
	result
      (error "quadtree doesn't contain point ~a" point))))

(defmethod traverse (function (quadtree quadtree) &key (mode :leaves) (collect-results nil))
  (traverse function (root quadtree) :mode mode :collect-results collect-results))

(defmethod draw ((quadtree quadtree))
  (when (not (display-list quadtree))
    (setf (display-list quadtree) (gl:gen-lists 1))
    (gl:with-new-list ((display-list quadtree) :compile)
      (gl:color 1 0 0 0.9)
      (gl:bind-texture :texture-2d (gethash 'visible-line *textures*))
      (traverse (lambda (v)
                  (draw-line-strip (corners v) 1 :closed t :bind-texture nil))
                quadtree :mode :all :collect-results nil)

      (gl:color 0 1 0 0.9)
      (traverse (lambda (v)
  
                  (loop :for edge :in (incident-edges v)
                        :do (when (eq (first (vertices edge)) v)
                              (draw-line-strip (list (center v)
                                                     (center (opposite-vertex edge v)))
                                               1
                                               :closed nil :bind-texture nil))))
                quadtree :collect-results nil)))      
  (gl:call-list (display-list quadtree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass quadtree-vertex ()
  ((corners
    :initarg :corners
    :initform (error "quadtree-vertex needs corners")
    :reader corners)
   (center
    :reader center)
   (objects
    :initform ()
    :accessor objects)
   (parent
    :initarg :parent
    :initform (error "quadtree-vertex is an orphan :(")
    :reader parent)
   (level
    :accessor level)
   (children
    :initform ()
    :accessor children)
   (incident-edges
    :initform ()
    :accessor incident-edges)
   (bounding-box 
    :initform nil)
   (a*-data
    :accessor a*-data)))

(defmethod initialize-instance :after ((v quadtree-vertex) &key)
  (setf (slot-value v 'center) (convex-combination (first (corners v)) (third (corners v)) 0.5))
  (setf (level v) (if (parent v) (+ 1 (level (parent v))) 0)))

(defmethod bounding-box ((v quadtree-vertex))
  (when (not (slot-value v 'bounding-box))
    (setf (slot-value v 'bounding-box)
	  (make-instance 'bounding-box
			 :xmin (x (second (corners v)))
			 :xmax (x (fourth (corners v)))
			 :ymin (y (second (corners v)))
			 :ymax (y (fourth (corners v))))))
  (slot-value v 'bounding-box))

(defmethod contains? ((v quadtree-vertex) (p vektor))
  (contains? (bounding-box v) p))

; ATTENTION: possible numerical problems here!
;            two vertices v and u are incident iff an edge of one of them is part of an edge of the other.
;            due to numerical inaccuracies it might happen that the bounding-box check below fails 
;	     (the one edge might be slightly away from the other).
;            maybe (distance one-edge other-edge) < eps should be used here (for each pair of edges) but
;            this solution is much faster and seems to work perfectly.
;
;            i think that nothing bad can happen if you consider the way the quadtree is computed...
;            you want proof? i have none... ;)
(defmethod incident? ((v quadtree-vertex) (u quadtree-vertex))
  (or 
   (= 2 (loop :for corner :in (corners v) :sum
	   (if (contains? (bounding-box u) corner) 1 0)))
   (= 2 (loop :for corner :in (corners u) :sum
	   (if (contains? (bounding-box v) corner) 1 0)))))

(defmethod localize ((v quadtree-vertex) (p vektor))
  (when (children v)
    (dolist (child (children v))
      (when (and child (contains? child p))
	(return-from localize (localize child p))))
    (return-from localize nil))
  v)

(defmethod add-neighbour ((v quadtree-vertex) (u quadtree-vertex))
  (let ((edge (make-instance 'quadtree-edge :vertices (list v u))))
    (push edge (incident-edges v))
    (push edge (incident-edges u))))

(defmethod remove-neighbour ((v quadtree-vertex) (u quadtree-vertex))
  (setf (incident-edges v) (remove-if (lambda (e) (connects-vertices? e u v)) (incident-edges v)))
  (setf (incident-edges u) (remove-if (lambda (e) (connects-vertices? e u v)) (incident-edges u))))

(defmacro quadtree-vertex-with-neighbours (vertex &body body)
  `(dolist (edge (incident-edges ,vertex))
     (let ((neighbour (opposite-vertex edge ,vertex)))
       ,@body)))

(defmethod compare ((v quadtree-vertex) (u quadtree-vertex))
  (- (level u) (level v)))

(defmethod margin-lines ((v quadtree-vertex))
  (loop :for i :below 4 :collect
     (make-line (elt (corners v) i) (elt-mod (corners v) (+ i 1)))))

(defmethod common-margin ((v quadtree-vertex) (u quadtree-vertex))
  (destructuring-bind (smaller bigger) (if (> 0 (compare v u)) (list v u) (list u v))
    (dolist (margin-line (margin-lines smaller))
      (when (intersect margin-line (make-line (center smaller) (center bigger)))
	(return-from common-margin margin-line)))))

(defmethod star-point? ((v quadtree-vertex) (u quadtree-vertex) (p vektor))
  (let ((cmp (compare v u))
	(common-margin (common-margin v u)))
    (or (= 0 cmp)
	(< (abs (reduce #'+ (mapcar (lambda (c) 
				      (distance common-margin
						(make-line p c)))
				    (corners (if (> 0 cmp) v u)))))
	   *quadtree-epsilon*))))
	  
(defmethod split! ((parent quadtree-vertex) &key (quadtree-depth *quadtree-default-depth*))
  (macrolet ((select-corners (i j k l) `(list (elt corners ,i)
					      (elt corners ,j)
					      (elt corners ,k)
					      (elt corners ,l))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; calculate splitting points (the children's corners)
    ;
    ; scheme (of identifiers):
    ;
    ; a,b,c,d     : corners of parent 
    ; 0,1,...,8   : index of corner in list of corners 
    ; c-0,...,c-3 : children
    ;
    ; a=0-----1-----2=d
    ;   |     |     |
    ;   | c-0 | c-3 |
    ;   |     |     |
    ;   3-----4-----5
    ;   |     |     |
    ;   | c-1 | c-2 |
    ;   |     |     | 
    ; b=6-----7-----8=c
    ;
    (let* ((a (first  (corners parent)))
	   (b (second (corners parent)))
	   (c (third  (corners parent)))
	   (d (fourth (corners parent)))
	   (corners (list a                            (convex-combination a d 0.5) d
			  (convex-combination a b 0.5) (convex-combination a c 0.5) (convex-combination c d 0.5)
			  b                            (convex-combination b c 0.5) c))
	   (children (list (make-instance 'quadtree-vertex  :corners (select-corners 0 3 4 1) :parent parent)
			   (make-instance 'quadtree-vertex  :corners (select-corners 3 6 7 4) :parent parent)
			   (make-instance 'quadtree-vertex  :corners (select-corners 4 7 8 5) :parent parent)
			   (make-instance 'quadtree-vertex  :corners (select-corners 1 4 5 2) :parent parent))))
      (setf (children parent) children)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; allocate objects
      (loop 
       :for i :below 4 :do
       (block allocate-objects-to-child-i
	 (let* ((child (elt children i))
		(child-polygon (make-instance 'polygon :corners (corners child))))
	   (dolist (o (objects parent))
	     (let ((intersection (intersects? child-polygon o :check-inclusion t)))
	       (when intersection 
		 (if (eq intersection child-polygon)
		     ; child is completely inside of the object (see documentation of 'intersects?')
		     ; => drop and continue with next child
		     (progn
		       (setf (elt children i) nil)
		       (return-from allocate-objects-to-child-i))
		   ; object intersects child
		   (push o (objects child)))))))))

      ; children must not contain any obects if finest resolution is reached
      ; => remove children that contain objects
      (when (= quadtree-depth 0)
	(setf children (mapcar (lambda (c) (if (and c (objects c)) nil c)) children)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; update neighbourhood

      ; allocate neighbours to children
      (dolist (edge (incident-edges parent))
	(let ((neighbour (opposite-vertex edge parent)))
	  (dolist (child children)
	    (when (and child (incident? child neighbour))
	      (add-neighbour child neighbour)))))

      ; some children are neighbours
      (loop 
       :for i :below 4 :do
       (when (and (elt children i) (elt-mod children (+ i 1)))
	 (add-neighbour (elt children i) (elt-mod children (+ i 1)))))

      ; the parent's neighbours are removed
      (dolist (edge (copy-list (incident-edges parent)))
	(remove-neighbour parent (opposite-vertex edge parent)))

      ; go on splitting?
      (when (> quadtree-depth 0)
	(dolist (child children)
	  (when (and child (objects child))
	    (split! child :quadtree-depth (- quadtree-depth 1))))))))

(defmethod traverse (function (v quadtree-vertex) &key (mode :leaves) (collect-results nil))
  (let ((results ()))
    (if (children v)
	(progn
	  (when (not (eq mode :leaves))
	    (let ((result (funcall function v)))
	      (when collect-results
		(push result results))))

	  (dolist (child (children v))
	    (when child
	      (let ((result (traverse function child :mode mode :collect-results collect-results)))
		(when collect-results
		  (setf results (append result results)))))))

      ; vertex has no children => leaf
      (let ((result (funcall function v)))
	(when collect-results
	  (push result results))))
    results))

(defmethod print-object ((v quadtree-vertex) stream)
  (format stream "{quadtree-vertex: ~a}" (corners v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass quadtree-edge ()
  ((vertices
    :initarg :vertices
    :initform (error "quadtree-edge needs vertices")
    :reader vertices)
   (weight
    :initform nil)
   (margin-crossing-point
    :reader margin-crossing-point)))
   
(defmethod initialize-instance :after ((edge quadtree-edge) &key)
  (let* ((u (first (vertices edge)))
	 (v (second (vertices edge)))
	 (common-margin (common-margin u v)))
    (setf (slot-value edge 'margin-crossing-point)
	  (intersect (make-line (center u) (center v)) common-margin))))
    
(defmethod weight ((edge quadtree-edge))
  (when (not (slot-value edge 'weight))
    (setf (slot-value edge 'weight)
	  (distance (center (first (vertices edge))) 
		    (center (second (vertices edge))))))
  (slot-value edge 'weight))

(defmethod opposite-vertex ((edge quadtree-edge) (vertex quadtree-vertex))
  (if (eq vertex (first (vertices edge)))
      (second (vertices edge))
    (first (vertices edge))))

(defmethod connects-vertices? ((edge quadtree-edge) (v quadtree-vertex) (u quadtree-vertex))
  (or (and (eq (first (vertices edge)) v) (eq (second (vertices edge)) u))
      (and (eq (first (vertices edge)) u) (eq (second (vertices edge)) v))))
