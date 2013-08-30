(in-package :storm)


(defgeneric can-fire-p (weapon))
(defmethod can-fire-p ((weapon weapon))
   (> *ticks*
      (+ (last-fired-time weapon)
         (cooldown-time weapon))))


(defun deflect (source-pos target-pos weapon-speed target-velocity)
  ;seems to return really big values sometimes, but works 99.99% of the time. TODO: use unit tests :)
  (let* ((delta (subtract target-pos source-pos))
         (a     (- (absolute-squared target-velocity)
                   (expt weapon-speed 2)))
         (b     (* 2 (dot-product delta target-velocity)))
         (c     (absolute-squared delta)) ; mitternachtsformel. details siehe blatt papier :)
         (root (- (expt b 2)
                  (* 4 a c))))
    (if (< root 0)
        (error (format nil "target unreachable: root=~a.~%" root))
      (let* ((t1 (/ (+ (- b) (sqrt root))
                    (* 2 a)))
             (t2 (/ (- (- b) (sqrt root))
                    (* 2 a)))
             (time (if (and (< 0 t1) (< 0 t2))
                    (error "target unreachable")
                  (if (and (> 0 t1) (> 0 t2))
                      (if (> t1 t2)
                          t2
                        t1)
                    (if (> t1 t2)
                        t1
                      t2)))))
      ;  (print-vars delta root t1 t2 time)
        (scale (make-point (+ (/ (x delta) time) (x target-velocity))
                           (+ (/ (y delta) time) (y target-velocity)))
               time)))))
        


(defgeneric fire (weapon))

(defmethod fire :around ((weapon weapon))  
  (when (target weapon)
    (if (or (dead-p (target weapon))
            (not (targetable-p weapon (target weapon))))
        (setf (target weapon) nil)
      (when (can-fire-p weapon)
        (call-next-method))))) ;real method gets called only if these checks are passed

(defmethod fire ((weapon weapon))
  (setf (last-fired-time weapon)
        (+ (* (random (cooldown-time weapon)) 0.2) ;make cooldown times a bit fuzzy to avoid largs packs of units firing at the same time
           (- (* (cooldown-time weapon) 0.1))
           *ticks*)))

(defmethod fire ((cannon cannon))
  (let* ((path                        ;(subtract (pos (target cannon))
                                        ;         (pos cannon))))
          (handler-case (deflect (pos cannon) (pos (target cannon)) (initial-speed cannon) (velocity (target cannon)))
            (error () (subtract (pos (target cannon))
                                (pos cannon)))))) ; if target can't be reached, shoot nonetheless. TODO: fixme. move into :around method?
    (unless (> (absolute path) (range cannon))
      (make-instance 'bullet
                     :origin (pos cannon)
                     :pos (pos cannon)
                     :owner (owner cannon)
                     :rot (- 90 (atan2 path))
                     :size-x (projectile-size-x cannon)
                     :size-y (projectile-size-y cannon)
                     :damage (damage cannon)
                     :damage-radius (damage-radius cannon)
                     :with-shockwave (with-shockwave cannon)
                     :velocity (normalize path (initial-speed cannon))
                     :range (absolute path))
      (call-next-method))))

(defmethod fire ((laser laser-cannon))
  (when (or
         (not (laser-beam laser))
         (not (equalp (target-unit (laser-beam laser)) (target laser))))
    (setf (laser-beam laser)
          (make-instance 'laser-beam
                         :owner (owner laser)
                         :source-weapon laser
                         :target-unit (target laser)
                         :dont-add t))
    (dolist (child (children laser))
      (detach child))
    (acquaint laser (laser-beam laser))))
  ;(call-next-method)) ;no cooldown-time. TODO: clean up.
  

(defmethod fire ((launcher rocket-launcher))
  (let ((rocket (make-instance 'rocket
                               :owner (owner launcher)
                               :size-x (projectile-size-x launcher)
                               :size-y (projectile-size-y launcher)
                               :source-weapon launcher
                               :rot (- 90 (atan2 (subtract (pos launcher) (pos (host-unit launcher)))))
                               :damage (damage launcher)
                               :damage-radius (damage-radius launcher)
                               :max-speed (max-rocket-speed launcher)
                               :max-angular-velocity (max-rocket-angular-velocity launcher)
                               :acceleration (rocket-acceleration launcher)
                               :target (target launcher)
                               :trigger-distance (trigger-distance launcher)
                               :pos (pos launcher))))
    (setf (velocity rocket)
          (normalize (rotate (make-point 0 1) (rot rocket))
                     (initial-speed launcher))))
  (call-next-method))





(defgeneric deal-damage (thing))
(defmethod deal-damage ((explosion explosion))
  ;TODO: look anly at destructible entities, use grid
  (setf (damage-dealt-p explosion) t)
  (dolist (unit *units*)
    (let* ((distance (distance (pos unit) (pos explosion)))
           (distance-ratio (/ distance (damage-radius explosion))))
      (when (and (<= distance-ratio 1)
                 (not (eq (owner explosion) (owner unit)))) ;no friendly fire
        (take-damage unit (* (- 1 distance-ratio) (damage explosion)))))))

(defmethod deal-damage ((laser-beam laser-beam))
  (take-damage (target-unit laser-beam) (damage (source-weapon laser-beam))))




  
(defgeneric targetable-p (thing target))
(defmethod targetable-p ((weapon weapon) (thing t))
  nil)

(defmethod targetable-p ((weapon weapon) (entity destructible-entity))
  (and entity
       (not (invalid entity))
       (< (distance weapon entity)
          (range weapon))))
