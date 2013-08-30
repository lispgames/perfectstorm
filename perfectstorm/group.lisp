(in-package :storm)


(defgeneric join-group (unit group))
(defgeneric leave-group (unit group))
    
(defmethod join-group ((unit unit) (group group))
  (if (eq (owner unit) (owner group))
      (progn
        (push unit (units group))
        (setf (group unit) group)
        (update-centroid group)
        (if (leader group)
            (setf (destination-unit unit) (leader group)) ;when a group has a leader, it is to be followed
          (setf (leader group) unit)))
    (error "unit tried to commit treason")))

(defmethod leave-group ((unit unit) (group group))
  (when (eq (group unit) group)
      (setf (units group) (remove unit (units group)))
      (when (eq (leader group) unit)
        (choose-leader group))
      (when (= 0 (length (units group)))
        (invalidate group))))

(defmethod issue-move-order ((group group) (dest vektor))
  (setf (waypoints group)
        (mapcar #'location (waypoints (a* *quadtree* (pos (leader group)) dest)))
        (destination (leader group))
        (first (waypoints group))
        (waypoint-lines group)
        (mapcar (lambda (p1 p2)
                  (make-instance 'visible-line :points (list p1 p2) :color '(0 0 1 1)))
                (cons (pos (leader group)) (waypoints group))
                (waypoints group)))
  (print-vars (waypoints group)))

(defmethod waypoint-reached? ((group group))
  (waypoint-reached? (leader group)))

(defmethod waypoint-reached? ((unit ground-unit))
  (< (distance-squared (pos unit) (destination unit)) (sqr (radius unit))))

(defmethod choose-leader ((group group))
  (setf (leader group)
        (choose (units group)))
  (mapcar (lambda (unit)
            (setf (destination-unit unit) (leader group)))
          (units group)))

(defgeneric update-centroid (group))
(defmethod update-centroid ((group group))
  (setf (cached-centroid group) (centroid-2d (mapcar #'pos (units group)))))