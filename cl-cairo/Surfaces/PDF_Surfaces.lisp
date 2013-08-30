;;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


(cffi:defcfun ("cairo_pdf_surface_create" pdf-surface-create)
    surface-t
  (filename :string) (width :double) (height :double))
(export 'pdf-surface-create)


;; TODO: Streaming support.
;; I believe these functions accepts streams returned by fopen. -Lars.
;;(cffi:defcfun ("cairo_pdf_surface_create_for_stream" pdf-surface-create-for-stream)
;;    surface-t
;;  (write-func :write-func-t) (closure :void) (width :double) (height :double))


(cffi:defcfun ("cairo_pdf_surface_set_dpi" pdf-surface-set-dpi)
    :void
  (surface surface-t) (x-dpi :double) (y-dpi :double))
(export 'pdf-surface-create)
