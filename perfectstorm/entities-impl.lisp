(in-package :storm)


;(defmethod pos :before ((weapon weapon))
;  ())
  ;(setf (pos weapon)
  ;      (add (rotate (scale (relative-pos weapon)
  ;                          (* (max (size-x (host-unit weapon))
  ;                                  (size-y (host-unit weapon)))
  ;                             0.8 ;weapons are mounted within the unit
  ;                             (distance-from-unit-center weapon)))
  ;                   (rot (host-unit weapon)))
  ;           (pos (host-unit weapon)))))

(defmethod pos ((weapon weapon))
  (let* ((host (host-unit weapon))
         (relative-pos (relative-pos weapon))
         (size-x (size-x host))
         (size-y (size-y host)))
    (add (rotate relative-pos
          (rot host))
         (pos (host-unit weapon)))))

(defgeneric max-size (thing))
;returns an upper bound for the diameter

(defmethod max-size ((entity entity))
  (max (size-x entity) (size-y entity)))


(defgeneric ttl-over-p (thing current-time))
(defmethod ttl-over-p ((entity volatile-entity) current-time)
  (> current-time (+ (creation-time entity)
                     (ttl entity))))


(defgeneric dead-p (entity))
(defmethod dead-p ((entity destructible-entity))
  (< (health entity) 0))


(defgeneric invalidate (thing))

(defmethod invalidate ((entity entity))
  ;entity is destroyed or by some other means scheduled to be removed. very temporary implementation (O(n))
  (setf (invalid entity) t)
  (setf *entities* (remove entity *entities*))
  (setf *independent-entities* (remove entity *independent-entities*)))

(defmethod invalidate ((unit unit))
  (dolist (weapon (weapons unit))
    (invalidate weapon))
  (setf *units* (remove unit *units*))
  (call-next-method))

(defmethod invalidate ((unit ground-unit))
  (setf *ground-units* (remove unit *ground-units*))
  (call-next-method))

(defmethod invalidate ((group group))
  (setf *groups* (remove group *groups*)))


;(defmethod invalidate ((unit air-unit))
; *air-units* does not (yet) exist
;  (setf *air-units* (remove unit *air-units*))
;  (call-next-method))

(defmethod invalidate ((entity moving-entity))
  (grid-unregister entity)
  (setf *moving-entities* (remove entity *moving-entities*))
  (call-next-method))

(defgeneric destroy (entity &optional explosion-pos))

(defmethod  destroy ((entity entity) &optional explosion-pos)
  (invalidate  entity))

(defmethod destroy ((unit unit) &optional explosion-pos)
  (if (group unit)
      (leave-group unit (group unit)))
  (call-next-method))

(defmethod destroy ((entity exploding-entity) &optional (explosion-pos (pos entity)))
  (when (not (invalid entity))
    (make-instance 'explosion
                   :pos explosion-pos
                   :damage (damage entity)
                   :damage-radius (damage-radius entity)
                   :drawable (with-shockwave entity)
                   :base-alpha (if (with-shockwave entity) 1 0)
                   :owner (owner entity))
    (call-next-method)))

(defgeneric take-damage (thing amount))
(defmethod  take-damage ((entity destructible-entity) amount)
  (setf (health entity)
        (- (health entity)
           amount))
  (when (and (< (health entity) 0)
             (not (invalid entity)))
    (destroy entity)))

(defmethod distance ((e1 entity) (e2 entity))
  (distance (pos e1) (pos e2)))

(defmethod distance ((e1 entity) (p vektor))
  (distance (pos e1) p))

(defmethod distance ((c1 visible-circle) (c2 visible-circle))
  (- (distance (pos c1) (pos c2))
     (radius c1)
     (radius c2)))


(defun repel-units (units)
  (dolist (unit units)
    (let ((collision-candidates (remove-if-not #'collides (grid-get-neighbour-entities unit))))
      ;;(print-vars (length collision-candidates))
      (dolist (candidate collision-candidates)
        (let* ((dist-squared (distance-squared (pos candidate) (pos unit)))
               (radii  (+ (radius candidate) (radius unit)))
               (radii-squared (sqr radii)))
          (when (and (not (eq unit candidate))
                     (< dist-squared radii-squared))
            (setf (pos unit)
                  (add (pos unit)
                    (normalize (subtract (pos unit) (pos candidate))
                               (- radii (sqrt dist-squared)))))))))))


(defgeneric add-line (thing line))
(defmethod add-line ((node node) (line line))
      (setf (lines node)
          (cons line (lines node))))



(defun make-lines-from-nodes (nodes &key (add-lines nil))
  (let ((sides (length nodes)))
    (loop for node-number from 0 to (- sides 1)
                  collecting (let* ((node1 (nth (mod node-number sides) nodes))
                                    (node2 (nth (mod (+ 1 node-number) sides) nodes))
                                    (line (make-line node1 node2)))
                               (when add-lines
                                 (add-line node1 line)
                                 (add-line node2 line))
                               line))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;printing stuff
;;


(defmethod print-object ((player player) (stream stream))
  (format stream "~a" (name player)))


(defmethod print-object ((thing visible-thing) (stream stream))
  (format stream "~a at ~a with size ~a~%"
          (string-downcase (class-name (class-of thing)))
          (pos thing)
          (make-point (size-x thing) (size-y thing))))



(defmethod print-object ((unit unit) (stream stream))
  ;(format stream "unit"))
  (format stream "~a's ~a (~a/~a)"  (owner unit) (class-name (class-of unit)) (round (health unit)) (round (max-health unit))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; selection
;;

(defgeneric select (thing))

(defmethod select ((entity entity))
  (when (selectable entity)
    (let ((max-size
           (* 1.2 (max (size-y entity) ;selection indicator should be a bit bigger than the selected entity
                       (size-x entity)))))
      (setf (slot-value entity 'selected)
            (make-instance 'selection-indicator
                           :size-x max-size
                           :size-y max-size
                           :pos    (pos entity))))))

(defmethod (setf selected) (new-selected (entity entity))
  (when (selectable entity)
    (if new-selected
      (select entity)
      (setf (slot-value entity 'selected)
            nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; setf methods that don't belong enywhere else
;;



(defmethod (setf rot) (new-rot (thing visible-thing))
  (setf (slot-value thing 'rot)
        new-rot))

(defmethod (setf rot) (new-rot (unit unit))
  (dolist (weapon (weapons unit))
    (setf ;(relative-pos weapon)
          ;(rotate (relative-pos weapon)
          ;        0);(- (rot unit) new-rot))
          (slot-value unit 'rot)
          new-rot)))



(defmethod (setf nodes) (new-nodes (polygon cupes-polygon)) ;update lines and visual stuff according to new nodes.
  (setf (slot-value polygon 'nodes) ;nodes that are only points are now nodes, too
        (loop for node in new-nodes 
              collecting (if (eq (class-of node) (find-class 'vektor))
                             (change-class node 'node)
                              node)))
  
  (dolist (node (nodes polygon))
    (setf (polygons node)
          (cons polygon (polygons node)))) ;node knows its polygons
        
  (let ((sides (length (nodes polygon))))
    (setf (slot-value polygon 'lines)
          (make-lines-from-nodes new-nodes :add-lines t)
                                 
          (slot-value polygon 'visible-lines)
          (loop for line in (lines polygon)
                collecting (make-instance 'visible-line
                                          :size-y 2
                                          :points (list (p1 line) (p2 line))))
          
          (slot-value polygon 'glows)
          (loop for node-number from 0 to (- sides 1)
                collecting (make-instance 'glow
                                          :radius 4
                                          :alpha 1
                                          :pos (nth (mod node-number sides) (nodes polygon)))))
    (acquaint polygon (concatenate 'list (glows polygon) (visible-lines polygon)))))

(defmethod (setf points) (new-points (line visible-line))
  (let ((p1 (first new-points))
        (p2 (second new-points)))
  (setf (pos line) (convex-combination p1 p2 0.5)
        (size-x line) (absolute (subtract p1 p2))
        (rot line) (- 90 (atan2 (subtract p1 p2))))))


(defmethod (setf radius) (new-radius (c visible-circle)) ;this should be used instead of setting size-x/y explicitly
  (setf (slot-value c 'size-x)
        (* 2 new-radius)
        (slot-value c 'size-y)
        (* 2 new-radius)
        (slot-value c 'radius)
        new-radius))


;the next two methods ensure a circle stays a circle
(defmethod (setf size-x) (new-size-x (c visible-circle))
  (setf (radius c)
        (/ 2 new-size-x)))

(defmethod (setf size-y) (new-size-y (c visible-circle))
  (setf (radius c)
        (/ 2 new-size-y)))