(in-package :vektor)

(defparameter *angle-mode* :deg) ; :rad or :deg
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; vector manipulation library
;;


(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *coordinate-names* '(x y z w))

  (defclass vektor ()
    ((dimension
      :initform (error"no dimension")
      :initarg :dimension
      :reader dimension)
     (data
      :initform (error "empty vektor")
      :initarg :data
      :accessor data))))

(defclass matrix ()
  ((data
    :initarg :data
    :accessor data)))

(defmethod initialize-instance :after ((matrix matrix) &key dimensions (data ()) (initial-element 0))
  (setf (data matrix)
        (if data
            data
            (loop for row from 0 to (first dimensions)
                  collect (loop for column from 0 to (second dimensions)
                                collect initial-element)))))

(defun make-matrix (data)
  (make-instance 'matrix :data data))

(defmethod rows ((matrix matrix))
  (length (data matrix)))

(defmethod columns ((matrix matrix)) ;assumes well-formed matrices
  (length (first (data matrix))))


(defmethod element ((matrix matrix) i j)
  (nth i (nth j (data matrix))))

(defclass vektor-3d (vektor)
  ((dimension
    :initform 3)))

(defun make-vektor-3d (&rest values)
  (if (and (= (length values) 3)
           (every #'numberp values))
      (make-instance 'vektor-3d :data values :dimension 3)
      (if (and (= 1 (length values))
             (consp (first values)))
        (apply #'make-vektor-3d (first values))
        (error "cannot create vektor from ~a" values))))
  

(defun make-vektor (&rest values)
  (if (and (>= (length values) 1)
           (<= (length values) (length *coordinate-names*))
           (every #'numberp values))
      (if (= 3 (length values))
          (apply #'make-vektor-3d values)
          (make-instance 'vektor :data values :dimension (length values)))
    (if (and (= 1 (length values))
             (consp (first values)))
        (apply #'make-vektor (first values))
        (error "cannot create vektor from ~a" values))))

(defun make-point (&rest values) ;synonym
  (apply #'make-vektor values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; operations in R^n
;;
;;

(defgeneric vektor= (thing1 thing2)
  (:documentation  "compare two vektors. return t if they are both vektors, the numer of dimensions is = and their contents are =, else nil"))

(defmethod vektor= ((something t) (v2 vektor))
  nil)
(defmethod vektor= ((v1 vektor) (something t))
  nil)

(defmethod vektor= ((v1 vektor) (v2 vektor))
  (and (= (dimension v1) (dimension v2))
       (every (lambda (i j)
                (= i j))
              (data v1)
              (data v2))))

(defmethod print-object ((vektor vektor) stream)
  (format stream "#<vektor (~{~a~^,~})>" (data vektor)))

(defgeneric composition-by-element (function thing &rest more-things))
(defmethod composition-by-element (function (vektor vektor) &rest vektors)
  (apply #'make-vektor
         (apply #'mapcar
                (cons function
                      (mapcar #'data (cons vektor vektors))))))

(defmethod composition-by-element (function (things cons) &rest more-lists)
  (let ((things (apply #'append (cons things more-lists))))
    (apply #'composition-by-element (cons function things))))

(defgeneric update (target source)) ;update an existing vektor with another's position
(defmethod update ((target vektor) (source vektor))
  (if (= (dimension target) (dimension source))
    (setf (slot-value target 'data) (slot-value source 'data))
    (error "can't update vektor when source and target dimension differ."))
  target)

(defgeneric add (thing &rest more-tings))
(defmethod add ((list cons) &rest ignored)
  (apply #'add list))
(defmethod add ((vektor vektor) &rest vektors)
  (composition-by-element #'+ (cons vektor vektors)))

(defgeneric subtract (thing &rest more-tings)) 
(defmethod subtract ((vektor vektor) &rest vektors)
  (composition-by-element #'- (cons vektor vektors)))

(defgeneric scale (thing factor))
(defmethod scale ((vektor vektor) (factor number))
  (composition-by-element (lambda (element)
                            (* factor element))
                          vektor))

(defgeneric dot-product (v1 v2))
(defmethod dot-product ((v1 vektor) (v2 vektor))
  (reduce #'+
          (mapcar #'* (data v1) (data v2))))

(defgeneric multiply (thing1 thing2))
(defmethod multiply ((v1 vektor) (v2 vektor))
  (dot-product v1 v2)) ;multiply on vectors is an alias for dot-product

(defgeneric norm-squared (v1))
(defmethod norm-squared ((vektor vektor))
  (dot-product vektor vektor))

(defgeneric norm (thing &key type))
(defmethod norm ((vektor vektor) &key (type :euclidean))
  (case type
    (:euclidean (sqrt (norm-squared vektor)))
    (otherwise (error "not implemented"))))

(defgeneric absolute (thing)) ;synonyms
(defmethod absolute ((vektor vektor))
  (norm vektor))

(defgeneric absolute-squared (thing))
(defmethod absolute-squared ((vektor vektor))
  (norm-squared vektor))

(defgeneric distance (thing1 thing2))
(defmethod distance ((v1 vektor) (v2 vektor))
  (norm (subtract v1 v2)))

(defgeneric distance-squared (thing1 thing2))
(defmethod distance-squared ((v1 vektor) (v2 vektor))
  (norm-squared (subtract v1 v2)))

(defgeneric normalize (vektor &optional new-norm))
(defmethod normalize ((vektor vektor) &optional (new-norm 1))
  (if (= 0 (norm vektor))
      (progn
        (error "normalizing 0-vektor, returning itself.")
         vektor)
    (scale vektor (/ new-norm (norm vektor)))))

(defgeneric centroid (&rest vektors))
(defmethod centroid (&rest vektors)
  (format t "~a~%" vektors)
  (scale (apply #'add vektors)
         (/ (length vektors))))

(defgeneric affine-combination (thing1 thing2 bias))
(defmethod affine-combination ((v1 vektor) (v2 vektor) bias)
  (add v1
       (scale (subtract v2 v1)
              bias)))

(defgeneric convex-combination (thing1 thing2 bias))
(defmethod convex-combination ((v1 vektor) (v2 vektor) bias)
  (affine-combination v1 v2 (max (min bias 1) 0)))

(defgeneric crop (thing maximum &optional minimum))
(defmethod crop ((number number) (maximum number) &optional minimum)
  (max (min number maximum) minimum))

(defmethod crop ((vektor vektor) (maximum number) &optional minimum)
  (let ((abs (absolute vektor)))
    (unless (> abs maximum)
      (setf vektor (normalize vektor maximum)))
    (unless (< abs minimum)
      (setf vektor (normalize vektor minimum)))))


(defgeneric project (subject target))
(defmethod project ((subject vektor) (target vektor))
  (normalize target
             (dot-product subject (normalize target))))



   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; macros for incf-like usage
;; todo: rethink this
;;
(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *setf-macros*
    '((addf add)
      (subtractf subtract)
      (scalef scale)
      (normalizef normalize))))

(defmacro make-setf-macros ()
  `(eval-when ( :load-toplevel)
    ,@(loop for (macro-name function-name) in *setf-macros*
            collecting `(defmacro ,macro-name (place offset)
                          `(setf ,place
                                 (,',function-name ,place ,offset))))))
(make-setf-macros)


;
;the aboce macro will create something like this:
;(defmacro addf ((place offset))
;  `(setf ,place
;         (add ,place ,offset)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; single coordinate extraction and setf methods
;;


(defmacro make-coordinate-accessors ()
  `(eval-when (:compile-toplevel :load-toplevel)
    ,@(loop for coord-name in *coordinate-names*
            for i from 0 to (- (length *coordinate-names*) 1)
            collecting `(defgeneric ,coord-name (vektor))
            collecting `(defmethod ,coord-name ((vektor vektor))
                         (nth ,i (data vektor)))
            collecting `(defmethod (setf ,coord-name) (new-value (vektor vektor))
                         (setf (nth ,i (data vektor))
                          new-value)))))
(make-coordinate-accessors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; stuff with angles
;;



(defgeneric rotate-z (thing angle &key mode)) ;todo: angle-mode verwenden

(defun angle-mode-factor (&key (mode *angle-mode*))
  (case mode
       (:rad 1)
       (:deg (/ 180 pi))
       (otherwise (error "please specify :mode :rad or :mode :deg"))))

(defmethod rotate-z ((v1 vektor) (rot-angle number) &key (mode *angle-mode*))
  (let ((angle (atan2 v1)))
    (normalize (make-vektor
                    (sin (/ (+ angle rot-angle) (angle-mode-factor :mode mode)))
                    (cos (/ (+ angle rot-angle) (angle-mode-factor :mode mode))))
           (absolute v1))))

(defun atan2-inner (x y)
  "somewhat awful but stolen, tested and working atan2"
  (if (= y 0.0)
      (if (< x 0.0)
      (+ PI (/ PI 2.0))
    (/ PI 2.0))
    (if (= x 0.0)
    (if (> y 0.0)
        0.0
      PI)
      (let ((at (atan (/ x y))))
    (if (> x 0.0)
        (if (> y 0.0)
        at
          (+ PI at))
      (if (> y 0.0)
          (+ PI PI at)
        (+ PI at)))))))

(defun atan2-of-numbers (x y &key (mode *angle-mode*))
  ;i want &optional and &key :(
  (* (atan2-inner x y)
     (angle-mode-factor :mode mode)))



(defgeneric atan2 (thing &key mode))

(defmethod atan2 ((vektor vektor) &key (mode *angle-mode*))
  "compute atan2 between x and y coords. might not make sense for dimensions other than 2"
  (atan2-of-numbers (x vektor) (y vektor) :mode mode))


;;;;;;;;;;;;;;
;;
;; fixed dimensionality stuff
;;

(defmethod cross-product ((v1 vektor-3d) (v2 vektor-3d))
  (make-vektor
    (- (* (y v1) (z v2))
       (* (z v1) (y v2)))
    
    (- (* (z v1) (x v2))
       (* (x v1) (z v2)))
    
    (- (* (x v1) (y v2))
       (* (y v1) (x v2)))))



;; todo: mit quaternionen um beliebige achsen drehen


;;;;;;;;;;;;;;;;;;;
;;
;; matrix handling
;;
;; dimensions are in the format (rows columns)
;;

(defmethod print-object ((matrix matrix) (stream stream))
  (format stream "~{|~{~$~^ ~}|~%~}" (data matrix)))


(defmethod vektor-from-row ((matrix matrix) row)
  (make-vektor (nth row (data matrix))))


(defmethod vektor-from-column ((matrix matrix) column)
  (make-vektor (mapcar (lambda (row)
                         (nth column row))
                       (data matrix))))

(defmethod transpose ((matrix matrix))
  (make-matrix (apply #'mapcar #'list (data matrix))))
 

(defmethod multiply ((m1 matrix) (m2 matrix)) ;todo: 1. make fast, 2. use &rest
  (let ((m2-columns (apply #'mapcar #'list (data m2))))
    (make-matrix (mapcar (lambda (row)
                           (mapcar (lambda (column)
                                     (apply #'+ (mapcar #'* row column)))
                                   m2-columns))
                         (data m1)))))


(defmethod copy ((matrix matrix))
  (make-matrix (copy-tree (data matrix))))

(defmethod cycle! ((matrix matrix) &optional (i 1) (j 1))
  (setf (data matrix)
        (mapcar (lambda (row)
                  (cycle row i))
                (cycle (data matrix) j)))
  matrix)

(defmethod cycle ((matrix matrix) &optional (i 1) (j 1))
  (cycle! (copy matrix) i j))


(defun make-rot-matrix-3d (angle axis &key (mode :angle-mode))
  (let ((angle (/ angle (angle-mode-factor :mode mode))))
    (cycle! (make-matrix `((1                0            0)
                           (0 ,(cos  angle)     ,(sin angle))
                           (0 ,(- (sin angle)) ,(cos angle))))
                         (- axis)
                         (- axis))))

(defmethod multiply ((m matrix) (v vektor))
  (assert (= (rows m) (dimension v)))
  (make-vektor (mapcar (lambda (row v-entry)
                         (apply #'+ (mapcar (lambda (m-entry)
                                              (* m-entry v-entry))
                                            row)))
                       (data m)
                       (data v))))

(defmethod multiply ((v vektor) (m matrix))
  (multiply v (transpose m)))


(defgeneric det (matrix))
(defmethod det ((matrix matrix))
  (let ((rows (rows matrix))
        (columns (columns matrix)))
    (cond ((and (= rows 2)
                (= columns 2))
             (- (* (element matrix 0 0) (element matrix 1 1))
                (* (element matrix 0 1) (element matrix 1 0))))
          ((and (= rows 3)
                (= columns 3))
             (- (loop for i from 0 to 2
                      sum (apply #'* (loop for j from 0 to 2
                                           collect (element matrix (mod (+ i j) 3) j))))
                (loop for i from 0 to 2
                      sum (apply #'* (loop for j from 0 to 2
                                           collect (element matrix (mod (- i j) 3) j))))))
          (t (error "i am too stupid to do that")))))