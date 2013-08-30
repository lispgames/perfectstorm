;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)


(defcfun ("cairo_pattern_add_color_stop_rgb" pattern-add-color-stop-rgb)
    :void
  (pattern pattern-t) (offset :double) (red :double) (green :double) (blue :double))
(export 'pattern-add-color-stop-rgb)


(defcfun ("cairo_pattern_add_color_stop_rgba" pattern-add-color-stop-rgba)
    :void
  (pattern pattern-t) (offset :double) (red :double) (green :double) (blue :double) (alpha :double))
(export 'pattern-add-color-stop-rgba)


(defcfun ("cairo_pattern_create_rgb" pattern-create-rgb)
    pattern-t
  (red :double) (green :double) (blue :double))
(export 'pattern-create-rgb)


(defcfun ("cairo_pattern_create_rgba" pattern-create-rgba)
    pattern-t
  (red :double) (green :double) (blue :double) (alpha :double))
(export 'pattern-create-rgba)


(defcfun ("cairo_pattern_create_for_surface" pattern-create-for-surface)
    pattern-t
  (surface surface-t))
(export 'pattern-create-for-surface)


(defcfun ("cairo_pattern_create_linear" pattern-create-linear)
    pattern-t
  (x0 :double) (y0 :double) (x1 :double) (y1 :double))
(export 'pattern-create-linear)


(defcfun ("cairo_pattern_create_radial" pattern-create-radial)
    pattern-t
  (cx0 :double) (cy0 :double) (radius0 :double) (cx1 :double) (cy1 :double) (radius1 :double))
(export 'pattern-create-radial)


(defcfun ("cairo_pattern_destroy" pattern-destroy)
    :void
  (pattern pattern-t))
(export 'pattern-destroy)


(defcfun ("cairo_pattern_reference" pattern-reference)
    pattern-t
  (pattern pattern-t))
(export 'pattern-reference)


(defcfun ("cairo_pattern_status" pattern-status)
    status-t
  (pattern pattern-t))
(export 'pattern-status)


(defun (setf pattern-extend) (extend pattern)
  (foreign-funcall "cairo_pattern_set_extend"
                   pattern-t pattern
                   extend-t extend))
(defun pattern-extend (pattern)
  (foreign-funcall "cairo_pattern_get_extend"
                   pattern-t pattern
                   extend-t))
(export 'extend)


(defun (setf pattern-filter) (filter pattern)
  (foreign-funcall "cairo_pattern_set_filter"
                   pattern-t pattern
                   filter-t filter))
(defun pattern-filter (pattern)
  (foreign-funcall "cairo_pattern_get_filter"
                   pattern-t pattern
                   filter-t))
(export 'pattern-filter)


(defun (setf pattern-matrix) (matrix pattern)
  (foreign-funcall "cairo_pattern_set_matrix"
                   pattern-t pattern
                   matrix-t matrix))
(defun pattern-matrix (pattern)
  (foreign-funcall "cairo_pattern_get_matrix"
                   pattern-t pattern
                   matrix-t))
(export 'pattern-matrix)


(defun pattern-type (pattern)
  (foreign-funcall "cairo_pattern_get_type"
                   pattern-t pattern
                   pattern-type-t))
  