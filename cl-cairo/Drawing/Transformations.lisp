;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)

(defcairo "cairo_translate" :void
  :double tx :double ty)

(defcairo "cairo_scale" :void
  :double sx :double sy)

(defcairo "cairo_rotate" :void
  :double angle)

(defcairo "cairo_transform" :void
  matrix-t matrix)


(defun matrix (&optional (+cr+ +cr+))
  (with-foreign-object (matrix 'matrix-t)
    (foreign-funcall "cairo_get_matrix"
                     cairo-t +cr+
                     matrix-t matrix)
    (mem-ref matrix 'matrix-t)))
(defun (setf matrix) (matrix &optional (+cr+ +cr+))
  (foreign-funcall "cairo_set_matrix"
                   cairo-t +cr+
                   matrix-t matrix))
(export 'matrix)


;; TODO: Simplify the stuff below.
(defcfun ("cairo_user_to_device" %user-to-device)
    :void
  (cr cairo-t) (x :pointer) (y :pointer))
(defmacro user-to-device (x y &optional cr)
  "See section in README about return-values."
  (let ((fx (gensym)) (fy (gensym)))
    (if cr
        `(with-foreign-objects ((,fx :double ,x) (,fy :double ,y))
          (%user-to-device ,cr ,fx ,fy)
          (values (mem-ref ,fx :double) (mem-ref ,fy :double)))
        `(with-foreign-objects ((,fx :double ,x) (,fy :double ,y))
          (%user-to-device +cr+ ,fx ,fy)
          (values (mem-ref ,fx :double) (mem-ref ,fy :double))))))
(export 'user-to-device)


(defcfun ("cairo_user_to_device_distance" %user-to-device-distance)
    :void
  (cr cairo-t) (dx :pointer) (dy :pointer))
(defmacro user-to-device-distance (dx dy &optional cr)
  "See section in README about return-values."
  (let ((fdx (gensym)) (fdy (gensym)))
    (if cr
        `(with-foreign-objects ((,fdx :double ,dx) (,fdy :double ,dy))
          (%user-to-device-distance ,cr ,fdx ,fdy)
          (values (mem-ref ,fdx :double) (mem-ref ,fdy :double)))
        `(with-foreign-objects ((,fdx :double ,dx) (,fdy :double ,dy))
          (%user-to-device-distance +cr+ ,fdx ,fdy)
          (values (mem-ref ,fdx :double) (mem-ref ,fdy :double))))))
(export 'user-to-device-distance)


(defcfun ("cairo_device_to_user" %device-to-user)
    :void
  (cr cairo-t) (x :pointer) (y :pointer))
(defmacro device-to-user (x y &optional cr)
  "See section in README about return-values."
  (let ((fx (gensym)) (fy (gensym)))
    (if cr
        `(with-foreign-objects ((,fx :double ,x) (,fy :double ,y))
          (%device-to-user ,cr ,fx ,fy)
          (values (mem-ref ,fx :double) (mem-ref ,fy :double)))
        `(with-foreign-objects ((,fx :double ,x) (,fy :double ,y))
          (%device-to-user +cr+ ,fx ,fy)
          (values (mem-ref ,fx :double) (mem-ref ,fy :double))))))
(export 'device-to-user)


(defcfun ("cairo_device_to_user_distance" %device-to-user-distance)
    :void
  (cr cairo-t) (dx :pointer) (dy :pointer))
(defmacro device-to-user-distance (dx dy &optional cr)
  "See section in README about return-values."
  (let ((fdx (gensym)) (fdy (gensym)))
    (if cr
        `(with-foreign-objects ((,fdx :double ,dx) (,fdy :double ,dy))
          (%device-to-user-distance ,cr ,fdx ,fdy)
          (values (mem-ref ,fdx :double) (mem-ref ,fdy :double)))
        `(with-foreign-objects ((,fdx :double ,dx) (,fdy :double ,dy))
          (%device-to-user-distance +cr+ ,fdx ,fdy)
          (values (mem-ref ,fdx :double) (mem-ref ,fdy :double))))))
(export 'device-to-user-distance)
