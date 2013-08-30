(in-package :storm)

(defclass vehicle ()
  ((mass :initform 10
         :reader mass
         :initarg :mass)
   (pos :initform (make-point 0 0)
        :accessor pos
        :initarg :pos)
   (forces :initform ())
   (velocity :initform (make-point 0 0)
             :accessor velocity
             :initarg :velocity)
   (max-force :initform 1
              :reader max-force
              :initarg :max-force)
   (max-speed :initform 10
              :reader max-speed
              :initarg :max-speed)
   (orientation :initform (make-point 1 0) ;heading. will usually be velocity normalized
                :accessor orientation
                :initarg :orientation)))

(defgeneric steer (subject force &optional weight))
(defmethod steer ((vehicle vehicle) (force vektor) &optional (weight 1))
  (push (scale force weight) (slot-value vehicle 'forces)))

(defgeneric locomotion-step (subject))
(defmethod locomotion-step ((vehicle vehicle))
  (with-slots (pos orientation mass forces max-force max-speed velocity) vehicle
    (setf velocity
          (crop (add velocity
                     (scale (crop (add forces) max-force)
                            (/ mass)))
                max-speed)
          forces ()
          pos
          (add pos velocity)
          orientation
          (if (= 0 (absolute-squared velocity))
            orientation
            (normalize velocity)))))