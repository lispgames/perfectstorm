;;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


;; TODO: Xlib-types Drawable and Pixmap should maybe be "typedefed"
;;       somewhere for more portability, but I hope this will do for now.


(cffi:defcfun ("cairo_xlib_surface_create" xlib-surface-create)
    surface-t
  (dpy :pointer)
  (drawable :unsigned-long) ;; xlib-type Drawable
  (visual :pointer)
  (width :int)
  (height :int)) 
(export 'xlib-surface-create)


(cffi:defcfun ("cairo_xlib_surface_create_for_bitmap" xlib-surface-create-for-bitmap)
    surface-t
  (dpy :pointer)
  (bitmap :unsigned-long) ;; xlib-type Pixmap
  (screen :pointer)
  (width :int)
  (height :int))
(export 'xlib-surface-create-for-bitmap)


(cffi:defcfun ("cairo_xlib_surface_set_size" xlib-surface-set-size)
    :void
  (surface surface-t)
  (width :int)
  (height :int))
(export 'xlib-surface-set-size)


(cffi:defcfun ("cairo_xlib_surface_set_drawable" xlib-surface-set-drawable)
    :void
  (surface surface-t)
  (drawable :unsigned-long) ;; xlib-type Drawable
  (width :int)
  (height :int)) 
(export 'xlib-surface-set-drawable)
