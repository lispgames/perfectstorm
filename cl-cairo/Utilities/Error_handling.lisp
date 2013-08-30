;;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


(cffi:defcfun ("cairo_status_to_string" status-to-string)
    :string
  (status status-t))
(export 'status-to-string)


(cffi:defcfun ("cairo_debug_reset_static_data" debug-reset-static-data)
    :void)
(export 'debug-reset-static-data)