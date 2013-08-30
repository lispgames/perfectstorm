(in-package :storm)

(defclass static-class (sb-mop:standard-class)
  ((static-slots
    :initform ()
    :accessor static-slots)
   (output
    :accessor output)))

(defclass static-direct-slot-definition (sb-mop:standard-direct-slot-definition)
  ((static-p
    :initform nil
    :accessor static-p
    :initarg :static)))

(defclass static-effective-slot-definition (sb-mop:standard-effective-slot-definition)
  ((static-p
    :initform nil
    :accessor static-p
    :initarg :static)))

(defmethod sb-mop:direct-slot-definition-class ((class static-class) &key &allow-other-keys)
  (find-class 'static-direct-slot-definition))

(defmethod sb-mop:effective-slot-definition-class ((class static-class) &key &allow-other-keys)
  (find-class 'static-effective-slot-definition))


(defparameter *output* "")
(defmethod sb-mop:compute-effective-slot-definition ((class static-class) slot-name direct-slot-definitions)
  (declare (ignore slot-name))
  (let ((effective-slotdef (call-next-method))) ;let CLOS do the lifting
    (setf (output class) (format nil "~a" effective-slotdef)); direct-slot-definitions)
    (setf (static-p effective-slotdef)
          (static-p (first direct-slot-definitions))) ;;wtf?
    effective-slotdef))

(defmethod sb-mop:validate-superclass ((class static-class) super)
  t)

(defclass foo ()
  ((bar
    :accessor bar
    :initarg :bar
    :static t))
  (:metaclass static-class))

;
;(slot-value (make-instance 'foo) 'bar)
;(static-slot-value 'foo 'bar)

