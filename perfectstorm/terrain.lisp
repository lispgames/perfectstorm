(in-package :storm)

(defclass obstacle ()
  ((margin
    :initarg :margin
    :reader margin)))

; map class --- but map is in use already
(defclass terrain ()
  ((obstacles
    :initarg :obstacles
    :reader obstacles)
   (quadtree
    :initform nil)
   (grid
    :reader grid)))

(defmethod quadtree ((terrain terrain))
  (when (not (slot-value terrain 'quadtree))
    (setf (slot-value terrain 'quadtree)
	  (make-instance 'quadtree 
			 :bottom-left-corner (make-point -200 -200)
			 :width 400
			 :height 400
			 :objects (mapcar #'margin (obstacles terrain)))))
  (slot-value terrain 'quadtree))