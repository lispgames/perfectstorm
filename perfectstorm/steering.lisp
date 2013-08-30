(in-package :storm)

;;;;;;;;;;;
;;
;; after Craig W. Reynold's "Steering Behaviors For Autonomous Characters"
;;
;; see locomotion.lisp
;; "steering" a vehicle means applying a force
;; this force is usually (a vektor pointing in the direction of) the difference between a desired and the actual velocity
;;
;; multiple "steer" calls within one locomotion step mean that the forces are added and then truncated
;;
;; a weight by which the force is multiplied may be supplied to indicate the importance of performing the task
;; (e.g. obstacle avoidance > seek)
;;
(defparameter *avoidance-distance* 20) ;how many ticks must a vehicle look ahead? todo: maybe integrate its agility or something?
(defparameter *collision-distance* 20) ;the same


(defgeneric seek (subject target &optional weight))
(defmethod seek ((vehicle vehicle) (target vektor) &optional (weight 1))
  (with-slots (pos max-speed velocity) vehicle
    (steer vehicle (subtract (normalize (subtract pos target)
                                        max-speed)
                             velocity) weight)))

(defgeneric flee (subject target &optional weight))
(defmethod flee ((vehicle vehicle) (target vektor) &optional (weight 1))
  (with-slots (pos max-speed velocity) vehicle
    (steer vehicle (subtract (normalize (subtract target pos)
                                        max-speed)
                             velocity) weight)))

(defgeneric pursue (subject target &optional weight))
(defmethod pursue ((vehicle vehicle) (target vehicle) &optional (weight 1))
  ;todo: use a better estimation
  (let* ((interval (/ (distance (pos vehicle) (pos target)) (max-speed vehicle)))
         (estimated-target (add (pos target)
                               (normalize (orientation target) (* (max-speed target) interval)))))
    (seek vehicle estimated-target weight)))

(defgeneric evade (subject target &optional weight))
(defmethod evade ((vehicle vehicle) (target vehicle) &optional (weight 1))
  ;todo: dont copypasta
  (let* ((interval (/ (distance (pos vehicle) (pos target)) (max-speed vehicle)))
         (estimated-target (add (pos target)
                               (normalize (orientation target) (* (max-speed target) interval)))))
    (flee vehicle estimated-target weight)))

(defgeneric arrive (subject target slowing-distance &optional weight))
(defmethod arrive ((vehicle vehicle) (target vektor) slowing-distance  &optional (weight 1))
  (with-slots (pos max-speed velocity) vehicle
      (let* ((offset (subtract target pos))
             (distance (absolute offset))
             (ramped-speed (min (* max-speed distance (/ slowing-distance)) max-speed)))
      (steer vehicle (subtract (normalize offset (/ ramped-speed distance))
                               velocity) weight))))

(defgeneric avoid (subject list-of-obstacles radius &optional weight)) ;avoid circular obstacles (other vehicles, small buildings)
(defmethod avoid ((vehicle vehicle) (obstacles cons) radius &optional (weight 1))
  (with-slots (orientation velocity pos max-speed) vehicle
    (let* ((left (rotate orientation 90))
           (candidates (remove-if (lambda (obstacle) ;obstacles are circles
                                   (let ((offset (subtract (pos obstacle) pos)))
			             (or (> 0 (dot-product offset orientation)) ;obstacle is behind
				         (> (sqr (* (absolute velocity) *avoidance-distance*)) (absolute-squared offset)) ;obstacle is too far away
				         (> (sqr radius) (absolute-squared (project offset left)))))) ;obstacle will not be hit by our current path
				  obstacles))
	   (nearest-obstacle (first (sort candidates (lambda (o1 o2) ;of all these, take the nearest
						       (< (- (distance o1 pos) (radius o1)) ;maybe replace this by a proper line-circle intersection?
							  (- (distance o2 pos) (radius o2))))))))
      (when nearest-obstacle
	(steer vehicle
	       (normalize (project (subtract (pos nearest-obstacle) pos) left)
         		  (* -1 (- (project (subtract (pos nearest-obstacle) pos) left) (radius nearest-obstacle))))
	       weight)))))

(defgeneric contain (subject list-of-obstacles radius &optional weight)) ;let the vehicle be contained by polygonal obstacles. probably use this with a large weight
(defmethod contain ((vehicle vehicle) (obstacles cons) radius &optional (weight 1))
  ;predict the vehicle's future position. if it is within an obstacle, adjust it.
  ;the case that the adjusted future position is still within an(other) obstacle will be ignored for now.
  (with-slots (orientation pos max-speed velocity) vehicle
    (let* ((future-pos (add pos (scale velocity *collision-distance*)))
           (obstacle (loop named obstacle-loop
	                   for obstacle in obstacles
	                   do (when (contains? obstacle future-pos)
				(return-from obstacle-loop obstacle))))
	   (path (make-line pos future-pos))) 
      (when obstacle
	(loop named line-loop
	      for line in (lines obstacle)
	      do (let ((intersection (intersect line path)))
	           (when intersection
	           ;;here it would be nice to know where the outside of a polygon's line is.
	           ;;instead, we assume that the vehicle's position is outside, which abviously
	           ;;sucks when it's inside
	             (let ((projected-pos (multiple-value-bind (dist projection)
				              (distance pos line)
					    (declare (ignore dist))
				            projection)))
		       (steer vehicle (normalize (subtract pos projected-pos)) weight)))))))))
	       
  
