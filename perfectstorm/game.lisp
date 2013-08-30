(in-package :storm)

(defclass game ()
  ((terrain
    :initarg :terrain
    :initform (error "game without terrain")
    :reader terrain)))