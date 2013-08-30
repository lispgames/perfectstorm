(in-package :storm)

(defmethod print-object ((p vektor) stream)
  (format stream "(~$,~$)" (x p) (y p)))

(defparameter *positive-infinity* 1e38)
(defparameter *negative-infinity* -1e38)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bounding-box
(defclass bounding-box ()
  ((xmin
    :initarg :xmin
    :accessor xmin)
   (xmax
    :initarg :xmax
    :accessor xmax)
   (ymin
    :initarg :ymin
    :accessor ymin)
   (ymax
    :initarg :ymax
    :accessor ymax)))

(defmethod print-object ((b bounding-box) (stream stream))
  (format stream "(bounding-box [~a, ~a] x [~a, ~a])"
	  (xmin b) (xmax b) (ymin b) (ymax b)))

(defmethod distance ((b bounding-box) (p vektor))
  (min (distance p (make-line (make-point (xmin b) (ymin b)) (make-point (xmax b) (ymin b))))
       (distance p (make-line (make-point (xmax b) (ymin b)) (make-point (xmax b) (ymax b))))
       (distance p (make-line (make-point (xmax b) (ymax b)) (make-point (xmin b) (ymax b))))
       (distance p (make-line (make-point (xmin b) (ymax b)) (make-point (xmin b) (ymin b))))))       

(defmethod contains? ((b bounding-box) (p vektor))
  (and (and (>= (x p) (xmin b)) (<= (x p) (xmax b)))
       (and (>= (y p) (ymin b)) (<= (y p) (ymax b)))))

(defmethod intersects? ((a bounding-box) (b bounding-box) &key)
  (and 
   (or (and (>= (xmin a) (xmin b)) (<= (xmin a) (xmax b)))
       (and (>= (xmin b) (xmin a)) (<= (xmin b) (xmax a))))
   (or (and (>= (xmin a) (xmin b)) (<= (xmin a) (xmax b)))
       (and (>= (xmin b) (xmin a)) (<= (xmin b) (xmax a))))))

(defgeneric make-bounding-box (thing-to-be-boxed))
(defmethod make-bounding-box ((list-of-points cons))
  (let ((xmin *positive-infinity*)
	(ymin *positive-infinity*)
	(xmax *negative-infinity*)
	(ymax *negative-infinity*))
    (loop for point in list-of-points
	  do (progn
	       (when (< (x point) xmin)
		 (setf xmin (x point)))
	       (when (< (y point) ymin)
		 (setf ymin (y point)))
	       (when (> (x point) xmax)
		 (setf xmax (x point)))
	       (when (> (y point) ymax)
		 (setf ymax (y point)))))
    (make-instance 'bounding-box
		   :xmax xmax
		   :ymax ymax
		   :xmin xmin
		   :ymin ymin)))



;wrapper
(defgeneric rotate (thing angle))
(defmethod rotate ((v vektor) alpha)
  (rotate-z v alpha :mode :deg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; geometric primitives
;;



(defclass line ()
  ((p1
    :initarg :p1
    :accessor p1) ;update bounding box when p1 or p2 is written?
   (p2
    :initarg :p2
    :accessor p2)
   (bounding-box
    :reader bounding-box)))

(defun make-line (p1 p2)
  (make-instance 'line :p1 p1 :p2 p2))

(defgeneric update-bounding-box (boxed-thing))
(defmethod update-bounding-box ((line line))
  (setf (slot-value line 'bounding-box)
	(make-bounding-box (list (p1 line) (p2 line)))))

(defmethod initialize-instance :after ((line line) &key)
  (update-bounding-box line))

(defclass circle ()
  ((center
    :initarg :center
    :accessor center)
   (radius
    :initarg :radius
    :accessor radius)))

(defun make-circle (center radius)
  (make-instance 'circle :center center :radius radius))

(defmethod distance ((c circle) (l line))
  (multiple-value-bind (dist lot)
      (distance (center c) l)
    (values
      (- dist (radius c))
      lot)))

(defmethod distance ((c circle) (p vektor))
  (values (- (distance (center c) p) (radius c))
	  (add (center c) 
	       (normalize (subtract p (center c)) (radius c)))))

(defmethod distance ((p vektor) (line line))
  (let* ((a (p1 line))
	 (b (p2 line))
	 (length-squared (distance-squared a b)))
    (if (> length-squared 0)
	(let* ((a->b (subtract b a))
	       (a->p (subtract p a))
	       (s (/ (dot-product a->b a->p) length-squared))
	       (lot (add a (scale a->b s))))
	       (cond ((> s 1)
		      (values (distance p b)
			      lot))
		     ((< s 0) 
		      (values (distance p a)
			      lot))
		     (t 
		      (values (distance p lot)
			      lot))))
      ; line is degenerated
      (values (distance p a)
	      a))))

(defmethod distance ((l1 line) (l2 line))
  (flet ((line-distance ()
           (min
            (distance (p1 l1) l2)
            (distance (p2 l1) l2)
            (distance (p1 l2) l1)
            (distance (p2 l2) l1))))
    (let* ((x1 (x (p1 l1)))
           (y1 (y (p1 l1)))
           (x2 (x (p2 l1)))
           (y2 (y (p2 l1)))
           (x3 (x (p1 l2)))
           (y3 (y (p1 l2)))
           (x4 (x (p2 l2)))
           (y4 (y (p2 l2)))
           (denominator
            #I((y4 - y3) * (x2 - x1)
               - (x4 - x3) * (y2 - y1)))
           (num1
            #I((x4 - x3) * (y1 - y3)
               - (y4 - y3) * (x1 - x3)))
           (num2
            #I((x2 - x1) * (y1 -  y3)
               - (y2 - y1) * (x1 - x3))))
  
      (if (= 0 denominator)
                                        ; lines are parallel or congruent
          (values
           (line-distance)
           ())
          (let ((r (/ num1 denominator))
                (s (/ num2 denominator)))
            (if (and
                 (>= r 0)
                 (<= r 1)
                 (>= s 0)
                 (<= s 1))
                (values
                 0
                 (add (p1 l1) (scale (subtract (p2 l1) (p1 l1)) r))) ;second return value is the intersection point
                (values
                 (line-distance)
                 ())))))))

(defgeneric intersect (object1 object2))

(defmethod intersect ((l1 line) (l2 line))
  (and (intersects? (bounding-box l1) (bounding-box l2))
       (multiple-value-bind (dist vektor) (distance l1 l2)
         (declare (ignore dist))
         vektor)))

(defmethod intersect ((c circle) (l line))
  (multiple-value-bind (dist lot)
      (distance (center c) l)
    (when (<= dist (radius c))
        (let* ((x (sqrt (- (expt (radius c) 2) ;siehe skizze :)
                          (expt dist 2))))
              (offset (normalize (subtract (p2 l) (p1 l)) x)))
          (format t "x: ~a~%" x)
          (values (subtract lot offset) ;reihenfolge?
                  (add lot offset))))))


(defun centroid-2d (list-of-points)
  (case (length list-of-points)
    (0 (make-point 0 0))
    (1 (first list-of-points))
    (otherwise (apply #'centroid list-of-points))))


(defgeneric angle (thing1 thing2))

(defmethod angle ((line1 line) (line2 line))
    (let ((angle (mod (- (atan2 (subtract (p2 line1) (p1 line1)))
                         (atan2 (subtract (p2 line2) (p1 line2))))
                      360)))
      (if (> angle 180)
          (- angle 360)
          angle)))

(defgeneric contains? (outer-object inner-object))

(defgeneric intersects? (object1 object2 &key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bounding-box
(defclass bounding-box ()
  ((xmin
    :initarg :xmin
    :accessor xmin)
   (xmax
    :initarg :xmax
    :accessor xmax)
   (ymin
    :initarg :ymin
    :accessor ymin)
   (ymax
    :initarg :ymax
    :accessor ymax)))

(defmethod print-object ((b bounding-box) (stream stream))
  (format stream "(bounding-box [~a, ~a] x [~a, ~a])"
	  (xmin b) (xmax b) (ymin b) (ymax b)))

(defmethod distance ((b bounding-box) (p vektor))
  (min (distance p (make-line (make-point (xmin b) (ymin b)) (make-point (xmax b) (ymin b))))
       (distance p (make-line (make-point (xmax b) (ymin b)) (make-point (xmax b) (ymax b))))
       (distance p (make-line (make-point (xmax b) (ymax b)) (make-point (xmin b) (ymax b))))
       (distance p (make-line (make-point (xmin b) (ymax b)) (make-point (xmin b) (ymin b))))))       

(defmethod contains? ((b bounding-box) (p vektor))
  (and (and (>= (x p) (xmin b)) (<= (x p) (xmax b)))
       (and (>= (y p) (ymin b)) (<= (y p) (ymax b)))))

(defmethod intersects? ((a bounding-box) (b bounding-box) &key)
  (and 
   (or (and (>= (xmin a) (xmin b)) (<= (xmin a) (xmax b)))
       (and (>= (xmin b) (xmin a)) (<= (xmin b) (xmax a))))
   (or (and (>= (xmin a) (xmin b)) (<= (xmin a) (xmax b)))
       (and (>= (xmin b) (xmin a)) (<= (xmin b) (xmax a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; polygon
(defclass polygon ()
  ((corners 
    :initarg :corners
    :initform (error "polygon needs corners")
    :reader corners)
   (lines
    :reader lines)
   (bounding-box
    :reader bounding-box)))

(defmethod initialize-instance :after ((p polygon) &key)
  (setf (slot-value p 'lines) 
	(loop 
	 :for i :below (length (corners p)) :collect
	 (make-line (elt (corners p) i) (elt-mod (corners p) (+ i 1)))))
  (setf (slot-value p 'bounding-box)
	(make-bounding-box (loop for line in (lines p)
				 collect (p1 line)
				 collect (p2 line)))))

(defmethod contains? ((p polygon) (q vektor))
  (and (contains? (bounding-box p) q)
       (let ((intersections 0)
	     (ray (make-line q (make-point (+ (xmax (bounding-box p)) 1) (y q)))))
	 (dolist (line (lines p))
	   (let ((intersection (intersect line ray)))
	     (when (and intersection (>= (x intersection) (x q)))
	       (if (and (> (y (p1 line)) (y q)) (<= (y (p2 line)) (y q)))
		   (incf intersections 1)
		 (when (and (<= (y (p1 line)) (y q)) (> (y (p2 line)) (y q)))
		   (incf intersections 1))))))
	 (= (mod intersections 2) 1))))


(defmethod make-rectangle ((center vektor) width height)
  (let ((translation (make-point (/ width 2) (/ height 2))))
    (make-instance 'polygon :corners (list (subtract center translation)
					   (make-point (- (x center) (x translation))
						       (+ (y center) (y translation)))
					   (add center translation)
					   (make-point (+ (x center) (x translation))
						       (- (y center) (y translation)))))))

; returns t if p intersects q, nil otherwise
;
; if check-inclusion, two special cases are considered additionaly:
; 1. p is returned if p is completely inside of q
; 2. q is returned if q is completely inside of p
(defmethod intersects? ((p polygon) (q polygon) &key (check-inclusion nil))
  ; check if bounding boxes intersect
  (when (intersects? (bounding-box p) (bounding-box q))
    
    ; check all lines for intersections
    ; return t if one exists
    (dolist (p-line (lines p))
      (dolist (q-line (lines q))
	(when (intersect p-line q-line) (return-from intersects? t))))

    ; no intersection =>
    ; is a point of p in q => p completely in q => return p
    (when (and check-inclusion (contains? q (first (corners p))))
      (return-from intersects? p))

    ; is a point of q in p => q completely in p => return q
    (when (and check-inclusion (contains? p (first (corners q))))
      (return-from intersects? q)))
  nil)