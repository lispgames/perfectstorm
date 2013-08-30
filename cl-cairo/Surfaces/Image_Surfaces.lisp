;;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


(cffi:defcfun ("cairo_image_surface_create" image-surface-create)
    surface-t
  (format format-t) (width :int) (height :int))
(export 'image-surface-create)


;; TODO: 
;; * cairo_image_surface_create_for_data
;; * cairo_image_surface_get_width
;; * cairo_image_surface_get_height


