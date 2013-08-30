;;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


(cffi:defcfun ("cairo_ps_surface_create" ps-surface-create)
    surface-t
  (filename :string) (width-in-points :double) (height-in-points :double))
(export 'ps-surface-create)


;; TODO: cairo_ps_surface_create_for_stream


(cffi:defcfun ("cairo_ps_surface_set_dpi" ps-surface-set-dpi)
    :void
  (surface surface-t) (x-dpi :double) (y-dpi :double))
(export 'ps-surface-set-dpi)


