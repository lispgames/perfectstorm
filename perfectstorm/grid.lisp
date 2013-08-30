



(in-package :storm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; grid functions

(defvar *GRID*)

(defun grid-init ()
  (assert (> (length *players*) 0))
  (setf *GRID* (make-array (list (/ *MAP-SIZE-X* *GRID-SIZE*) ;must be an integer. doom?
			         (/ *MAP-SIZE-Y* *GRID-SIZE*))))
  (loop  :for k :from 0 :to (- (first (array-dimensions *GRID*)) 1)
         :do (loop :for l :from 0 :to (- (second (array-dimensions *GRID*)) 1)
	           :do (setf (aref *GRID* k l)
                             (make-instance 'grid-cell
                                            :player-units (loop :for player :in *players*
                                                                :append `(,player 0))))))) ;make plist of players
  
(defclass grid-cell ()
  ((player-units :initform ()
                 :accessor player-units
                 :initarg :player-units) ;plist: mapping player => number of this player's units in this cell
   (entities :initform ()
             :accessor entities
             :initarg :entities)))

(defmethod print-object ((cell grid-cell) (stream stream))
  (format stream "[~{~a~^,~}]" (mapcar (lambda (player)
                                         (getf (player-units cell) player))
                                       *players*)))


(defmethod grid-get-index ((p vektor))
  (list
   (floor (/ (+ (x p) (/ *MAP-SIZE-X* 2)) *GRID-SIZE*))
   (floor (/ (+ (y p) (/ *MAP-SIZE-Y* 2)) *GRID-SIZE*))))

(defmethod grid-register ((e entity))
  (destructuring-bind (i j) (grid-get-index (pos e))
    ; insert into new grid
    (push e (entities (aref *GRID* i j)))
    ;increase owner's unit count in this cell
    (incf (getf (player-units (aref *GRID* i j)) (owner e)))
    ; update entitiy grid information
    (setf (grid-pos e) (list i j))))

(defmethod grid-update ((e entity))
  (destructuring-bind (i j) (grid-get-index (pos e))
    ; reregister unit if grid position has changed
    (when (not (and (= i (first (grid-pos e))) 
		    (= j (second (grid-pos e)))))

      (grid-unregister e)
      (grid-register e))))

(defmethod grid-unregister ((e entity))
  ;(format t "unregistering ~a at ~a,~a~%" e (first (grid-pos e)) (second (grid-pos e)))
  (destructuring-bind (i j) (grid-pos e)
    (let ((old-length (length (entities (aref *grid* i j))))
          (old-cell-content (copy-list (entities (aref *GRID* i j)))))
      (setf (entities (aref *GRID* i j))
            (remove e (entities (aref *GRID* i j))))
      ;decrease owner's unit count in this cell
      (decf (getf (player-units (aref *GRID* i j)) (owner e)))
      
      (unless (= old-length (+ 1 (length (entities (aref *grid* i j))))) ;todo: remove this
        (print-vars old-length (+ 1 (length (entities (aref *grid* i j)))) i j)
        (print-vars (aref *grid* i j) old-cell-content (grid-get-index (pos e)))
        (assert nil)))))


(defgeneric grid-get-contiguous-entities (thing-having-coordinates))

(defmethod grid-get-contiguous-entities ((e entity))
  (grid-get-contiguous-entities (pos e)))

(defmethod grid-get-contiguous-entities ((p vektor))
  (grid-get-entities-in-coarse-rectangle p p))

(defmethod grid-get-entities-in-coarse-rectangle ((p-i integer) (p-j integer) (q-i integer) (q-j integer) &key (player nil))
  ;rectangle + one safety in each direction
  ;if player is given, only entities this player does *not* own are returned
  (let ((other-players (remove player *players*)))
      (loop 
       :for k :from (max (min (- p-i 1) (- q-i 1))
                         0)
              :to (min (max (+ p-i 1) (+ q-i 1))
                       (- (first (array-dimensions *GRID*)) 1))
       :append (loop
                :for l :from (max (min (- p-j 1) (- q-j 1))
                                  0)
                       :to (min (max (+ p-j 1) (+ q-j 1))
                                (- (second (array-dimensions *GRID*)) 1))  
                :when (or (not player)
                          (< 0 (loop :for enemy :in other-players
                                     :sum (getf (player-units (aref *GRID* k l)) enemy))))
	        :append (if player
                          (remove-if (lambda (e)
                                       (eq (owner e) player))
                                     (entities (aref *GRID* k l)))
                          (entities (aref *GRID* k l)))))))

(defmethod grid-get-neighbour-entities ((entity entity))
  (let ((i (first (grid-pos entity)))
        (j (second (grid-pos entity))))       
    (grid-get-entities-in-coarse-rectangle i j i j)))

(defmethod grid-get-entities-in-rectangle ((p vektor) (q vektor) &key (player nil))
  (destructuring-bind (p-i p-j q-i q-j) (append (grid-get-index p)
                                                (grid-get-index q))
    (remove-if-not (lambda (e) (and (in-range-p (x (pos e)) #R[((min (x p) (x q)) (max (x p) (x q)))])
				    (in-range-p (y (pos e)) #R[((min (y p) (y q)) (max (y p) (y q)))])))
		   (grid-get-entities-in-coarse-rectangle p-i p-j q-i q-j :player player))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;