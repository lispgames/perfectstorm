;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)

;; TODO:
;; * cairo_read_func_t (type)
;; * cairo_image_surface_create_from_png_stream
;; * cairo_write_func_t (type)
;; * cairo_surface_write_to_png_stream


(cffi:defcfun ("cairo_image_surface_create_from_png"
               image-surface-create-from-png)
    surface-t
  (filename :string))
(export 'image-surface-create-from-png)


(cffi:defcfun ("cairo_surface_write_to_png" surface-write-to-png)
    status-t
  (surface surface-t) (filename :string))
(export 'surface-write-to-png)


(cffi:defcfun ("cairo_image_surface_get_width" image-surface-get-width)
   :int
 (surface surface-t))
(export 'image-surface-get-width)


(cffi:defcfun ("cairo_image_surface_get_height" image-surface-get-height)
   :int
 (surface surface-t))
(export 'image-surface-get-height)

(cffi:defcfun ("cairo_surface_write_to_png_stream" surface-write-to-png-stream)
    status-t
  (surface surface-t) (write-func :pointer) (closure :pointer))
(export 'surface-write-to-png-stream)


(cffi:defcfun ("cairo_image_surface_get_data" surface-get-data)
    :pointer
  (surface surface-t))

(defun surface-get-data-as-array (surface bytes-per-pixel)
    (let* ((width (image-surface-get-width surface))
           (height (image-surface-get-height surface))
           (buffer (make-array (* width height bytes-per-pixel ) :element-type '(unsigned-byte 8) :fill-pointer 0))
           (data (surface-get-data surface)))
      (loop for i from 0 below (* width height bytes-per-pixel) do
        (vector-push-extend (cffi:mem-ref data :uint8 i) buffer))
      buffer))
(export 'surface-get-data-as-array)
    

;; TODO: use cffi foreign lambdas instead of this when they are done
(cffi:defcallback write-stream-to-byte-array status-t ((closure :pointer)
                                                       (data :pointer) (length :int))
  (declare (ignore closure) (special %buffer%))
  (loop for i from 0 below length do
        (vector-push-extend (cffi:mem-ref data :uint8 i) %buffer%))
  :success)


(defun render-surface-as-png-into-byte-vector (surface)
  (let ((%buffer% (make-array 1 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (declare (special %buffer%))
    (surface-write-to-png-stream surface
                                 (cffi:callback write-stream-to-byte-array)
                                 (cffi:null-pointer))
    %buffer%))
(export 'render-surface-as-png-into-byte-vector)



;; TODO:
;; * cairo_write_func_t (type)


