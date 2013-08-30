;;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


(cffi:defcfun ("cairo_matrix_init" matrix-init)
    :void
  (matrix matrix-t) 
  (xx :double) (yx :double)
  (xy :double) (yy :double)
  (x0 :double) (y0 :double))
(export 'matrix-init)


(cffi:defcfun ("cairo_matrix_init_identity" matrix-init-identity)
    :void
  (matrix matrix-t))
(export 'matrix-init-identity)


(cffi:defcfun ("cairo_matrix_init_translate" matrix-init-translate)
    :void
  (matrix matrix-t) (tx :double) (ty :double))
(export 'matrix-init-translate)


(cffi:defcfun ("cairo_matrix_init_scale" matrix-init-scale)
    :void
  (matrix matrix-t) (sx :double) (sy :double))
(export 'matrix-init-scale)


(cffi:defcfun ("cairo_matrix_init_rotate" matrix-init-rotate)
    :void
  (matrix matrix-t) (radians :double))
(export 'matrix-init-rotate)


(cffi:defcfun ("cairo_matrix_translate" matrix-translate)
    :void
  (matrix matrix-t) (tx :double) (ty :double))
(export 'matrix-translate)


(cffi:defcfun ("cairo_matrix_scale" matrix-scale)
    :void
  (matrix matrix-t) (sx :double) (sy :double))
(export 'matrix-scale)


(cffi:defcfun ("cairo_matrix_rotate" matrix-rotate)
    :void
  (matrix matrix-t) (radians :double))
(export 'matrix-rotate)


(cffi:defcfun ("cairo_matrix_invert" matrix-invert)
    status-t
  (matrix matrix-t))
(export 'matrix-invert)


(cffi:defcfun ("cairo_matrix_multiply" matrix-multiply)
    :void
  (result matrix-t) (a matrix-t) (b matrix-t))
(export 'matrix-multiply)


;; TODO: This will not work.
(cffi:defcfun ("cairo_matrix_transform_distance" matrix-transform-distance)
    :void
  (matrix matrix-t) (dx :double) (dy :double))
(export 'matrix-transform-distance)


;; TODO: This will not work.
(cffi:defcfun ("cairo_matrix_transform_point" matrix-transform-point)
    :void
  (matrix matrix-t) (x :double) (y :double))
(export 'matrix-transform-point)

