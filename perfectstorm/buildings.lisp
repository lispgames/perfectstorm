(in-package :storm)

(defclass building (independent-entity destructible-entity exploding-entity obstacle)
  ())

(defmethod initialize-instance :after ((building building) &key (dont-add nil))
  (unless dont-add
    (push building *buildings*)))

(defclass factory (building)
  ((queue
    :initform ()
    :initarg :queue
    :accessor queue)
   (group  ;created units go in here
    :initform ()
    :initarg :group
    :accessor group)
   (rallying-point
    :initform (make-point 0 0)
    :initarg :rallying-point
    :accessor rallying-point)
   (current-cost
    :initform 1
    :initarg :current-cost
    :accessor current-cost)
   (production-speed
    :initform 1
    :initarg :production-speed
    :accessor production-speed)
   (progress
    :initform 0
    :accessor progress)
   (loop-queue-p
    :initform nil
    :initarg :loop-queue-p
    :accessor loop-queue-p)
   (unit-in-production
    :initform nil
    :initarg :unit-in-production
    :accessor unit-in-production)))

(defmethod initialize-instance :after ((factory factory) &key)
  (setf (slot-value factory 'margin)
	(make-rectangle (pos factory) (size-x factory) (size-y factory))))
	