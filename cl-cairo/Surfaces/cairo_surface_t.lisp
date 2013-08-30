;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)


(defcfun ("cairo_surface_create_similar" surface-create-similar) 
    surface-t
    (other surface-t) (content content-t) (width :int) (height :int))
(export 'surface-create-similar)


(defcfun ("cairo_surface_destroy" surface-destroy)
    :void
  (surface surface-t))
(export 'surface-destroy)


(defcfun ("cairo_surface_finish" surface-finish)
    :void
  (surface surface-t))
(export 'surface-finish)


(defcfun ("cairo_surface_flush" surface-flush)
    :void
  (surface surface-t))
(export 'surface-flush)


#|
TODO: (defcfun ("cairo_surface_get_font_options" surface-get-font-options)
    :void
  (surface surface-t) (options font-options-t))
(export 'surface-get-font-options)
|#


;; TODO: (defcfun ("cairo_surface_set_user_data" surface-set-user-data)
;;       ((surface surface-t)))

;; TODO: (defcfun ("cairo_surface_get_user_data" surface-set-user-data))


(defcfun ("cairo_surface_mark_dirty" surface-mark-dirty)
    :void
  (surface surface-t))
(export 'surface-mark-dirty)


(defcfun ("cairo_surface_mark_dirty_rectangle" surface-mark-dirty-rectangle)
    :void
  (surface surface-t) (x :int) (y :int) (width :int) (height :int))
(export 'surface-mark-dirty-rectangle)


(defcfun ("cairo_surface_reference" surface-reference)
    surface-t
  (surface surface-t))
(export 'surface-reference)


(defcfun ("cairo_surface_set_device_offset" surface-set-device-offset)
    :void
  (surface surface-t) (x-offset :double) (y-offset :double))
(export 'surface-set-device-offset)


(defcfun ("cairo_surface_status" surface-status)
    status-t
  (surface surface-t))
(export 'surface-status)

