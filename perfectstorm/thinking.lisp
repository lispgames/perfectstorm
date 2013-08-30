(in-package :storm)



(defgeneric think (entity))

(defmethod think ((group group))
  (when (and (waypoints group)
             (waypoint-reached? group))
    (format t "waypoint reached.~%")
    (unless (= 0 (length (waypoints group)))
      (pop (waypoint-lines group))
      (pop (waypoints group))) ;leave the last waypoint
    (setf (destination (leader group))
          (first (waypoints group)))))

(defmethod think ((unit ground-unit))
  ;destination or destination-unit must be set from the outside
  (when (group unit)
    (let ((path (subtract
                       (or (destination unit)
                           (and (destination-unit unit) (convex-combination (pos (destination-unit unit))
                                                                            (or (destination (destination-unit unit))
                                                                                (pos (destination-unit unit)))
                                                                            0.5))
			   (pos unit));todo
                       (pos unit))))
      (if (> (absolute path) (* *ticks-per-think* (max-speed unit)))
        
        (setf (velocity unit)
              (normalize path
                          (max-speed unit)))
        (setf (velocity unit) (make-point 0 0)))))
  (call-next-method))


(defmethod think ((unit unit))
 ;check if valid target(s) exist
  (unless (and (target unit)
               (some (lambda (weapon)
                       (targetable-p weapon (target unit)))
                     (weapons unit)))
  (let* ((max-range (+ (radius unit) (apply #'max (mapcar #'range (weapons unit)))))
         (half-diagonal (make-point max-range max-range))
         (candidates (remove-if (lambda (entity)
                                  (or (not (destructible entity))
                                      (eq (owner entity) (owner unit))))
                                  (grid-get-entities-in-rectangle (add (pos unit) half-diagonal)
                                                                (subtract (pos unit) half-diagonal)
                                                                :player (owner unit))))
         (usable-candidate (loop named auswahl for candidate in candidates
                                  do (when (and (< (distance-squared (pos candidate) (pos unit)))
                                                (every (lambda (weapon)
                                                        (targetable-p weapon candidate))
                                                      (weapons unit)))
                                       (return-from auswahl candidate)))))
    (setf (target unit) usable-candidate)

    (dolist (weapon (weapons unit))
      (setf (target weapon)
            (if (and (target unit)
                     (targetable-p weapon (target unit)))
                (target unit)
                nil))))))


(defmethod think ((unit air-unit))
     (when (group unit)
      (setf (desired-rot unit)
            (atan2 (subtract (destination (group unit)) (pos unit)))))
  (call-next-method))
