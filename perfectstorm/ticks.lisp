(in-package :storm)

(defparameter *speed-of-sound* 4)

(defgeneric tick (entity))


(defmethod tick :around ((entity entity))
  ;(error "tick around")
  (unless (invalid entity)
    (call-next-method)))

(defmethod tick :before ((unit ground-unit))
  (setf (pos unit)
        (case (length (new-positions unit))
          (0 (pos unit))
          (1 (first (new-positions unit)))
          (otherwise (centroid-2d (new-positions unit))))

        (new-positions unit)
        ()))

(defmethod tick ((unit ground-unit))
  (incf (rot unit) 0.5)
  (call-next-method))


(defmethod tick ((thing visible-thing))
  (dolist (child (children thing))
    (setf (alpha child)
          (alpha thing))))
;  (call-next-method))


(defmethod tick ((entity moving-entity))
  (when (invalid entity) (format t "invalid tick!~%"))
  (setf (pos entity) (add (pos entity)
                          (velocity entity)))
;  (print-vars (rot entity) (pos (first (weapons entity))))
  (when (selected entity)
    (setf (pos (selected entity))
          (pos entity)
          (alpha (selected entity))
          (min 1 (+ 0.1 (alpha (selected entity)))) ;fade-in when selection is less than 10 ticks old
          (radius (selected entity))
          (* (max (size-x entity)
                  (size-y entity))
             (- 2 (alpha (selected entity)));animation when fade-in in progress
             0.5 ;radius is half the size
             1.2) ;selection indicator must be larger than the unit
          (rot (selected entity))
          (+ 2 (rot (selected entity)))))
  
  (grid-update entity)
  (call-next-method))


(defmethod tick :before ((entity moving-entity))
  (setf (last-pos entity) (pos entity)))

(defmethod tick ((entity volatile-entity))
  (if (< (+ (ttl entity) (creation-time entity))
           *ticks*)
    (destroy entity)
    (call-next-method)))

(defmethod tick ((group group))
  ;(setf (cached-centroid group)
  ;      (centroid (mapcar #'pos (units group)))))
  (when (and (leader group)
             (waypoint-lines group))
    (setf (points (first (waypoint-lines group)))
          (list (pos (leader group))
                (first (waypoints group))))))

(defmethod tick ((unit unit))
  (dolist (weapon (weapons unit))
    (tick weapon))
  (call-next-method))

(defmethod tick ((unit air-unit))
  (setf (rot unit)
        (+ (rot unit)
           (* (max-angular-velocity unit)
              (if (< 180 (mod (- (atan2 (velocity unit))
                                 (desired-rot unit))
                              360))
                  -1
                  1)
              (let* ((turning-radius (* 0.5
                                        (/ (max-speed unit) (* pi (/ 180) (* 1 (max-angular-velocity unit))))))
                     (proximity (/ (distance unit (destination (group unit))) turning-radius)))
                (max 1
                  (min -1
                    (if (< proximity 1)
                      (- (* proximity 2) 1)
                      1))))))
        (velocity unit)
        (rotate (make-point 0 (max-speed unit))
                (rot unit)))
  
  (call-next-method))


(defmethod tick :after ((bullet bullet))
  (let ((path (subtract (pos bullet) (origin bullet))))
  (if (> (absolute path) (range bullet))
        (destroy bullet (setf (pos bullet)
                              (add (origin bullet)
                                   (normalize path (range bullet)))))
      (setf (size-x bullet)
                    (max (* 2 (size-y bullet)) ;bullet should be at least circular
                      (min (full-size-x bullet)
                                  (* 2 (absolute path))))))))
;  (print-vars (size-x bullet)))


(defmethod tick ((weapon weapon))
  (when (autofire weapon)
        (fire weapon)))

(defmethod tick ((rocket rocket))
   (setf (angular-velocity rocket)
	 (crop (angular-velocity rocket) (max-angular-velocity rocket)))
   (setf (velocity rocket) (add (velocity rocket)
                               (rotate (make-point 0 (acceleration rocket))
                                       (rot rocket))))
  (when (> (absolute (velocity rocket)) (max-speed rocket))
    (setf (velocity rocket) (normalize (velocity rocket)
                                       (max-speed rocket))))


  
  (when (or (not (target rocket)) ;if target is lost, take the weapon's next target as new target if available
            (invalid (target rocket)))
    (if (and (target (source-weapon rocket))
             (not (invalid (target (source-weapon rocket)))))
        (setf (target rocket) (target (source-weapon rocket)))))

  (when (target rocket) ;cryptic rocket targeting code follows
    (let* ((target-angle (- 90 (atan2 (subtract (pos (target rocket)) (pos rocket)))))
           (drift-angle (- 90 (atan2 (velocity rocket))))
           (relative-target-angle (- drift-angle target-angle))
           (desired-rot (if (> 90 (abs relative-target-angle)) ; target is in front?
                              (- target-angle relative-target-angle)
                            (- drift-angle 180)))
           (rot-delta (- (rot rocket) desired-rot)))
        (setf (angular-velocity rocket)
              (* (max-angular-velocity rocket)
                 (if (= 180 (mod rot-delta 360))
                     (if (< 180 (mod relative-target-angle 360))
                         1
                         -1)
                   (if (< 180 (mod rot-delta 360))
                        1
                       -1)))))

    (setf (rot rocket) (+ (rot rocket)
                          (angular-velocity rocket))))
 
  (if (or (not (target rocket))
          (< (distance (pos rocket) (pos (target rocket)))
             (trigger-distance rocket)))
        (destroy rocket)
    (call-next-method)))


(defmethod tick ((laser laser-cannon))
  (when (and (laser-beam laser)
             (or
               (not (target laser))
               (> (distance laser (target laser)) (range laser))))
    (detach (laser-beam laser))
    (setf (target laser) nil
          (laser-beam laser) nil))
  (let ((beam (laser-beam laser)))
    (when beam
      (tick beam)
      (deal-damage beam)
      (setf (points beam)
            (list (pos laser) (pos (target laser))))))
                                        ;(setf (pos beam) (convex-combination (pos laser) (pos (target laser)) 0.5)
                                        ;      (x (size beam)) (* 0.5 (absolute (subtract (pos laser) (pos (target laser)))))
                                        ;      (rot beam) (- 90 (atan2p (subtract (pos (target laser)) (pos laser)))))))
  (call-next-method))

(defmethod tick ((beam laser-beam))
  ;(format t "alpha is ~a~%" (alpha beam))
 (setf (alpha beam)
       (min 1
            (+ 0.1 (alpha beam)))
  
        (pos (first (children beam)))
        (pos (source-weapon beam))
        (pos (second (children beam)))
        (pos (target-unit beam))
        (pos (third (children beam)))
        (pos (target-unit beam)))
 (call-next-method))

(defmethod tick :after ((explosion explosion))
  ;ideally, damage should be dealt when the shockwave passes an enemy.
  ;at present, damage is dealt when the shockwave has reached half its maximum radius.
  (when (and (> (* 2 (radius explosion))
                (damage-radius explosion))
             (not (damage-dealt-p explosion))) 
      (deal-damage explosion))
  (when (> (radius explosion) (damage-radius explosion))
      (invalidate  explosion)))
       
(defmethod tick ((explosion explosion))
  (setf (radius explosion)
          (+ (radius explosion) *speed-of-sound*)
        (size-x explosion)
          (/ (radius explosion) (sqrt 2))
        (size-y explosion)
          (size-x explosion)
        (alpha explosion)
          (- 1 (/ (radius explosion) (damage-radius explosion)))
        (alpha (glow explosion))
          (expt (alpha explosion) 4))) ;glow fades away quickly


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buildings
;;

(defmethod cost ((unit-name symbol))
  (sb-mop:finalize-inheritance (find-class unit-name)); has to be done only once. TODO: remove from here
  (slot-value (sb-mop:class-prototype (find-class unit-name)) 'cost))

(defmethod tick ((factory factory))
  (incf (progress factory)
        (production-speed factory))
  (unless (group factory)
    (setf (group factory)
          (make-instance 'group
                         :owner (owner factory)
                         :destination (rallying-point factory))
          (current-cost factory)
          (cost (first (queue factory)))))
                         
  (when (>= (progress factory)
            (current-cost factory))
    (setf (current-cost factory)
          (cost (first (queue factory)))
          (progress factory)
          0)
    (make-instance (first (queue factory))
                   :owner (owner factory)
                   :group (group factory)
                   :pos (pos factory))))