(in-package :storm)

(defparameter *entities* ())
(defparameter *moving-entities* ())
(defparameter *independent-entities* ()) ; entities that are not mounted on other entities (and therefore need be drawn explicitly)
(defparameter *thinking-entities* ())

(defparameter *gui-things* ())

(defparameter *groups* ())
(defparameter *units* ())
(defparameter *buildings* ())

(defparameter *ground-units* ())

(defparameter *players* ())

(defclass player ()
    ((name
      :initform (error "no name given")
      :initarg :name
      :accessor name)
     (color
      :initform (error "no color given")
      :initarg :color
      :accessor color)
     (textures
      :initform (make-hash-table)
      :initarg :textures
      :reader textures)
     (cursor
      :initform nil
      :accessor cursor)))

(defmethod initialize-instance :after ((player player) &key)
  (push player *players*)
  (setf (cursor player) (make-instance 'cursor :owner player)
        (alpha (cursor player)) 1
        (rot (cursor player)) 120))

(defclass owned-thing ()
  ((owner
    :initform (error "unowned")
    :initarg :owner
    :accessor owner)))

(defclass group (owned-thing)
  ((units
    :initform ()
    :initarg :units
    :accessor units)
   (leader
    :initform nil
    :initarg :leader
    :accessor leader)
   (waypoints
    :initform ()
    :initarg :waypoints
    :accessor waypoints)
   (waypoint-lines
    :initform ()
    :initarg :waypoint-lines
    :accessor waypoint-lines)
   (cached-centroid
    :initform (make-point 0 0)
    :accessor cached-centroid)))

(defmethod initialize-instance :after ((group group) &key (dont-add nil) (units ()))
  (unless dont-add
    (push group *groups*))
  (dolist (unit units)
    (join-group unit group)))

(defclass composite () ;ye olde composite pattern mixin
  ((children
     :initform ()
     :reader children
     :initarg :children)
   (parent
     :initform nil
     :reader parent
     :initarg :parent)))

(defgeneric detach (child))

(defgeneric acquaint (parent child))

(defmethod detach ((child composite))
  (when (parent child)
    (setf (slot-value (parent child) 'children)
          (remove child (children (parent child))))
    (setf (slot-value child 'parent) nil)))
  

(defmethod acquaint ((parent composite) (children cons))
  (dolist (child children)
    (acquaint parent child)))

(defmethod acquaint ((parent composite) (child composite))
  (when (slot-value child 'parent)
    (detach child))
  (setf (slot-value child 'parent)
        parent)
  (push child (slot-value parent 'children)))



(defclass visible-thing (composite)
  ((pos
     :initform (make-point (random 10) (random 10))
     :initarg :pos
     :accessor pos)
   (rot
     :initform 0
     :reader rot
     :initarg :rot)
   (alpha
     :initform 1
     :accessor alpha
     :initarg :alpha)
   (base-alpha
    :initform 1
    :initarg :base-alpha
    :accessor base-alpha) ; alpha gets multiplied by this
   (size-x ; LENGTH already names an ordinary function or a macro.
     :initform 42
     :accessor size-x
     :initarg :size-x)
   (size-y
     :initform 23
     :accessor size-y
     :initarg :size-y)
   (shader
     :initform nil
     :accessor shader
     :allocation :class)
   (drawable
     :initform t ; if nil, draw may be called, but will not draw anything except when explicitly specified in the body of draw method. useful for container-objects. maybe generalize this using the composite pattern :)
     :accessor drawable
     :initarg :drawable)
   (owned
     :initform nil
     :reader owned)
   (selectable
     :initform nil
     :accessor selectable)
   (drawn-relative-p
     :initform nil
     :initarg :drawn-relative-p
     :accessor drawn-relative-p)
   (invisible
    :initform nil
    :initarg :invisible
    :accessor invisible)
   (layer
    :initform 0.0
    :initarg :layer
    :accessor layer)
   (blend-mode ;todo
    :initform :alpha
    :initarg :blend-mode
    :accessor blend-mode)))

(defclass cursor (owned-thing visible-thing)
  ((owned :initform t)))

(defclass entity (owned-thing visible-thing)
   ((grid-pos 
     :initform (list -1 -1)
     :accessor grid-pos)
    (invalid ;if t, entity is scheduled for removal and won't be drawn or anything else
     :initform nil
     :accessor invalid)
    (owned
     :initform t)
    (selected
     :initform nil
     :reader selected)
    (collides
     :initform nil
     :accessor collides
     :initarg :collides)
    (destructible
     :initform nil
     :accessor destructible)))

(defmethod initialize-instance :after ((entity entity) &key (dont-add nil))
  (unless dont-add
    (push entity *entities*)))
    
(defclass independent-entity (entity)
  ())

(defmethod initialize-instance :after ((entity independent-entity)  &key (dont-add nil))
  (unless dont-add
    (push entity *independent-entities*)))

(defclass destructible-entity (entity)
  ((max-health
    :initform 100
    :initarg :max-health
    :accessor max-health)
   (health
    :initform 100
    :initarg :health
    :accessor health)
   (health-circle-radius
    :initform 0.4
    :reader health-circle-radius)
   (cost
    :initform 23
    :initarg :cost
    :accessor cost
    :allocation :class)
   (destructible
    :initform t
    :accessor destructible)))

(defmethod initialize-instance :after ((entity destructible-entity)  &key)
  (setf (health entity)
        (max-health entity)))

(defclass moving-entity (independent-entity)
  ((velocity
    :initform (scale (make-point (random 5) (random 5)) 0.001)
    :initarg :velocity
    :accessor velocity)
   (max-speed
    :initform 0.2
    :initarg :max-speed
    :accessor max-speed)
   (last-pos
    :initform (make-point 3 4)
    :initarg :last-pos
    :accessor last-pos)))

(defmethod initialize-instance :after ((entity moving-entity)  &key (dont-add nil))
  (grid-register entity)
  (unless dont-add
    (push entity *moving-entities*)))



(defclass visible-line (visible-thing)
  ((color
    :initarg :color
    :initform '(1 1 1 1)
    :accessor color)
   (shader
    :initform :laser-beam)))

(defmethod initialize-instance :after ((line visible-line) &key (points ()))
  (when points
    (setf (points line) points)))

(defclass visible-circle (visible-thing)
  ((radius
    :initform 3.1337
    :initarg :radius
    :reader radius)))

(defmethod initialize-instance :after ((circle visible-circle) &key (radius (radius circle)))
  (setf (radius circle) radius))

(defclass visible-rectangle (visible-thing)
  ())

(defclass exploding-entity (entity)
  ((damage
    :initform 1
    :initarg :damage
    :accessor damage)
   (with-shockwave
    :initform t
    :initarg :with-shockwave
    :accessor with-shockwave)
   (damage-radius
    :initform 20
    :initarg :damage-radius
    :accessor damage-radius)))

(defclass thinking-entity (entity)
  ())

(defmethod initialize-instance :after ((entity thinking-entity)  &key (dont-add nil))
  (unless dont-add
    (push entity *thinking-entities*)))

(defclass unit (thinking-entity moving-entity destructible-entity exploding-entity) ;more complex stuff (pathfinding, ...)
  ((weapons
    :initform ()
    :accessor weapons)
   (range-of-sight ;TODO: use this
    :initform 100
    :initarg :range-of-sight
    :accessor range-of-sight)
   (rot
    :reader rot)
   (group
    :initform nil
    :accessor group)
   (target
    :initform nil
    :initarg :target
    :accessor target)
   (selectable
    :initform t)))

(sb-mop:finalize-inheritance (find-class 'unit))

(defmethod initialize-instance :after ((unit unit) &key (weapons ()) (group ()) (dont-add nil))
  (unless dont-add
    (push unit *units*))
  (setf weapons (append (weapons unit) weapons)
        (weapons unit) () );unit's weapons may be list of sympols (names of weapons)
  (dolist (weapon-name weapons)
    (let ((weapon (apply #'make-instance (cons weapon-name (list :owner (owner unit) :host-unit unit)))))
       (push weapon (weapons unit))))
  (when group
    (join-group unit group))
  (acquaint unit (weapons unit)))
  
(defclass ground-unit (visible-circle unit)
  ((new-positions ;list of new positions created by bumping into other units. the centroid of these positions will be used as the new pos prior to move
    :initform ()
    :accessor new-positions)
   (radius
    :initform 3)
   (health-circle-radius
    :initform 0.4)
   (destination-unit ;unit that will be followed, e.g. group leader
    :initform nil
    :accessor destination-unit
    :initarg :destination-unit)
   (destination ;next waypoint
    :initform nil
    :accessor destination
    :initarg :destination)
   (collides
    :initform t
    :accessor collides
    :initarg :collides)))

(defmethod initialize-instance :after ((unit ground-unit) &key (weapons ()) (dont-add nil))
  (unless dont-add
    (push unit *ground-units*))
  (let ((weapon-count (length (weapons unit))))
    (dotimes (weapon-number weapon-count) ;arrange weapons in a circle
      (setf (relative-pos (nth weapon-number (weapons unit)))
            (rotate (make-point 0.0 (* (distance-from-unit-center (nth weapon-number (weapons unit)))
                                       (radius unit)))
                    (* weapon-number (/ 360  weapon-count)))))))

(defclass air-unit (unit)
  ((max-speed
    :initform 10)
   (max-angular-velocity
    :initform 0.1
    :initarg :max-angular-velocity
    :accessor max-angular-velocity)
   (angular-velocity
    :initform 0
    :initarg :angular-velocity
    :accessor angular-velocity)
   (health-circle-radius
    :initform 1.4)
   (desired-rot
    :initform 0
    :accessor desired-rot)))

(defmethod initialize-instance :after ((unit air-unit) &key (weapons ()) (dont-add nil))
  ;(unless dont-add
  ;  (push unit *air-units*))
  (let ((weapon-count (length (weapons unit))) ;distribute the weapons nicely onto hull and wings
        (hull-weapons ())
        (wing-weapons ()))
    (dotimes (weapon-number weapon-count)
      (let ((current-weapon (nth weapon-number (weapons unit)))
            (next-weapon (when (< (+ 1 weapon-number) weapon-count)
                           (nth (+ 1 weapon-number) (weapons unit)))))
        (if (and next-weapon
                 (eql (class-of current-weapon)
                      (class-of next-weapon)))
            (progn
              (push current-weapon wing-weapons)
              (push next-weapon wing-weapons)
              (incf weapon-number))
            (push current-weapon hull-weapons))))
    (dotimes (weapon-number (length hull-weapons))
      (setf (relative-pos (nth weapon-number hull-weapons))
            (make-point 0
                        (/ weapon-number (length hull-weapons)))))
    (loop for weapon-number from 0 to (- (length wing-weapons) 1) by 2
          do (setf (relative-pos (nth weapon-number wing-weapons))
                   (add (make-point 0 0.3)
                        (normalize (make-point 3 -2)
                                   (- 1 (/ weapon-number (length wing-weapons)))))                      
                   (relative-pos (nth (+ 1 weapon-number) wing-weapons))
                   (add (make-point 0 0.3)
                        (normalize (make-point -3 -2)
                                   (- 1 (/ weapon-number (length wing-weapons)))))))))


(defclass wall (visible-line destructible-entity)
  ())

(defclass volatile-entity (entity)
  ((creation-time
    :initform 0
    :reader creation-time)
   (ttl
    :initform 300
    :initarg :ttl
    :accessor ttl)))

(defmethod initialize-instance :after ((entity volatile-entity) &key)
  (setf (slot-value entity 'creation-time)
        *ticks*))


(defclass rocket (volatile-entity destructible-entity moving-entity exploding-entity)
  ((max-speed
    :initform 30
    :initarg max-speed
    :accessor max-speed)
   (source-weapon
    :initform (error "rocket without source")
    :initarg :source-weapon
    :reader source-weapon)
   (acceleration
    :initform 0.01
    :initarg :acceleration
    :accessor acceleration)
   (target
    :initform (error "no target")
    :initarg :target
    :accessor target)
   (max-angular-velocity
    :initform 0.1
    :initarg :max-angular-velocity
    :accessor max-angular-velocity)
   (angular-velocity
    :initform 0
    :initarg :angular-velocity
    :accessor angular-velocity)
   (trigger-distance
    :initform 1
    :initarg :trigger-distance
    :accessor trigger-distance)))

(defclass bullet (moving-entity exploding-entity)
   ((origin
    :initform (error "no origin given")
    :initarg :origin
    :accessor origin)
   (range ;explodes after covering this distance
    :initform (error "no range given")
    :initarg :range
    :accessor range)
   (full-size-x
    :initform 1
    :accessor full-size-x) ;a bullet starts with length 0 and then grows to full size
   (size-x
    :initform 0)
   (size-y
    :initform 1)))


(defmethod initialize-instance :after ((bullet bullet) &key size-x)
  (setf (full-size-x bullet) size-x
        (size-x bullet) (size-y bullet)))


(defclass laser-beam (visible-line entity)
  ((source-weapon
    :initform (error "laser without source")
    :initarg :source-weapon
    :accessor source-weapon)
   (target-unit
    :initform (error "laser without target")
    :initarg :target-unit
    :accessor target-unit)
   (alpha
    :initform 0)
   (shader
    :initform :laser-beam)
   (size-x
    :initform 3)
   (size-y
    :initform 3)))

(defmethod initialize-instance :after ((beam laser-beam) &key)
  (setf (alpha beam) 0
        (points beam)
        (list (pos (source-weapon beam))
              (pos (target-unit beam))))
  (mapcar (lambda (glow)
            (acquaint beam glow))
          (list
           (make-instance 'glow :radius (* 5 (size-y beam)) :base-alpha 0.2 :pos (pos (target-unit   beam)))
           (make-instance 'glow :radius (* 0.5 (size-y beam)) :base-alpha 1   :pos (pos (target-unit   beam)))
           (make-instance 'glow :radius (* 0.7 (size-y beam)) :base-alpha 1   :pos (pos (source-weapon beam))))))

(defclass weapon (visible-circle entity)
  ((host-unit
    :initform (error "weapon without host")
    :initarg :host-unit
    :accessor host-unit)
   (relative-pos ;position relative to host unit
    :initform (make-point 0.0 0.7)
    :initarg :relative-pos
    :accessor relative-pos)
   (cooldown-time
    :initform 10
    :initarg :cooldown-time
    :accessor cooldown-time)
   (last-fired-time
    :initform 0
    :accessor last-fired-time)
   (target
    :initform ()
    :accessor target)
   (autofire
    :initform t
    :initarg :autofire
    :accessor autofire)
   (behaviour
    :initform :autofire ; :autofire, :hold, ...?
    :accessor behaviour)
   (damage
    :initform 1
    :initarg :damage
    :accessor damage)
   (damage-radius
    :initform 10
    :initarg :damage-radius
    :accessor damage-radius)
   (with-shockwave
    :initform t
    :initarg :with-shockwave
    :accessor with-shockwave)
   (range
    :initform 100
    :initarg :range
    :accessor range)
   (levels
    :initform () ; provides control over what targets can be shot. will include information about wheater ground targets, air targets and ground targets behind walls can be shot at.
    ;todo: think about level stuff
    :initarg :levels
    :accessor levels)
   (distance-from-unit-center
    :initform 0.7
    :initarg :distance-from-unit-center
    :accessor distance-from-unit-center)
   (radius
    :initform 1)
   (drawn-relative-p
     :initform t)))


(defmethod initialize-instance :after ((weapon weapon) &key)
  (setf (last-fired-time weapon)
        (+ *ticks* ;add random delay for aesthetic reasons
           (random (cooldown-time weapon)))))

(defclass projectile-weapon (weapon) ;weapons that fire moving things (i.e. everything except lasers)
  ((initial-speed
    :initform 0.5
    :initarg :initial-speed
    :accessor initial-speed)
   (projectile-size-x
    :initform 0.6
    :initarg :projectile-size-x
    :accessor projectile-size-x)
   (projectile-size-y
    :initform 0.2
    :initarg :projectile-size-y
    :accessor projectile-size-y)))

(defclass rocket-launcher (projectile-weapon)
  ((max-rocket-speed
    :initform 0.5
    :initarg :max-rocket-speed
    :accessor max-rocket-speed)
   (max-rocket-angular-velocity
    :initform 0.1
    :initarg :max-rocket-angular-velocity
    :accessor max-rocket-angular-velocity)
   (rocket-acceleration
    :initform 0.01
    :initarg :rocket-acceleration
    :accessor rocket-acceleration)
   (trigger-distance
    :initform 1
    :initarg :trigger-distance
    :accessor trigger-distance)))

(defclass cannon (projectile-weapon)
  ())

(defclass laser-cannon (weapon)
   ((laser-beam
    :initform nil
    :accessor laser-beam)
   (damage
    :initform 0.1))) ;damage-radius remains unused

(defclass explosion (independent-entity)
  ((damage
    :initform 10
    :initarg :damage
    :accessor damage)
   (damage-dealt-p
    :initform nil
    :accessor damage-dealt-p)
   (damage-radius
    :initform 20
    :initarg :damage-radius
    :accessor damage-radius)
   (radius
    :initform 0.5
    :accessor radius)
   (glow
    :initform nil
    :accessor glow)
   (size-x
    :initform 0.1)
   (size-y
    :initform 0.1)))

(defmethod initialize-instance :after ((explosion explosion) &key (with-shockwave t))
  (setf (glow explosion)
        (make-instance 'glow
                       :pos (pos explosion)
                       :radius (* (damage-radius explosion) 0.4) ;arbitrary values that make it look good
                       :alpha 1
                       :base-alpha 0.9))
  (acquaint explosion (glow explosion))
  (unless with-shockwave
    (setf (drawable explosion)
          nil)))
                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; purely visual objects
;;;

(defclass glow (visible-circle)
  ((radius
     :initform 5)
   (alpha
     :initform 0) ;invisible by default
   (shader
     :initform :glow)
   (base-alpha
     :initform 1)
   (drawn-relative-p
     :initform nil)))

(defclass cupes-polygon (visible-thing)
  ((drawable
    :initform nil)
   (owner
    :initform :gui)
   (nodes
    :initform ()
    :initarg :nodes
    :reader nodes)
   (lines
    :initform ()
    :reader lines)
   (visible-lines
    :initform ()
    :reader visible-lines)
   (glows
    :initform ()
    :reader glows)))


(defmethod initialize-instance :after ((polygon cupes-polygon) &key (nodes ()) (scale-elements 1))
  (setf (nodes polygon) nodes) ;call the setf method below
;;  (push polygon *gui-entities*)) polygon is only drawn explicitly
  (dolist (glow (glows polygon))
    (setf (radius glow) (* (radius glow) scale-elements)))
;;          (size-y glow) (* (size-y glow) scale-elements)))
  (dolist (line (visible-lines polygon))
    (setf (size-y line) (* (size-y line) scale-elements))))

(defclass selection-indicator (visible-circle)
  ((alpha
    :initform 0)
  ; (shader
  ;  :initform :glow)
   (base-alpha
    :initform 1)))

(defclass gui-thing ()
  ((screen-pos
    :initform (make-point 0 0)
    :initarg :screen-pos
    :accessor screen-pos)))

(defclass text (visible-thing gui-thing)
   ((text
    :initform (format nil "foo~%bar")
    :initarg :text
    :accessor text)
   (font
    :initform nil
    :initarg :font
    :accessor font)
   (drawable
    :initform nil)
   (cursor-pos
    :initform -1
    :initarg :cursor-pos
    :accessor cursor-pos)))

(defclass rectangle (visible-thing gui-thing)
  ((color :initform '(1 0 0 1)
          :initarg :color
          :accessor color)))

(defmethod initialize-instance :after ((gui-thing gui-thing) &key)
  (push gui-thing *gui-things*))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; rest
;;

(defclass node (vektor)
  ((polygons
    :initform ()
    :initarg :polygons
    :accessor polygons)
   (lines
    :initform ()
    :initarg :lines
    :accessor lines)))
