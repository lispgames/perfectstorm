;;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


(cffi:defcfun ("cairo_font_options_create" font-options-create)
    font-options-t)
(export 'font-options-create)


(cffi:defcfun ("cairo_font_options_copy" font-options-copy)
    font-options-t
  (original font-options-t))
(export 'font-options-copy)


(cffi:defcfun ("cairo_font_options_destroy" font-options-destroy)
    :void
  (options font-options-t))
(export 'font-options-destroy)


(cffi:defcfun ("cairo_font_options_status" font-options-status)
    status-t
  (options font-options-t))
(export 'font-options-status)


(cffi:defcfun ("cairo_font_options_merge" font-options-merge)
    :void
  (options font-options-t) (other font-options-t))
(export 'font-options-status)


(cffi:defcfun ("cairo_font_options_hash" font-options-hash)
    :unsigned-long
  (options font-options-t))
(export 'font-options-hash)


(cffi:defcfun ("cairo_font_options_equal" font-options-equal)
    bool-t
    (options font-options-t) (other font-options-t))
(export 'font-options-equal)


;; TODO: get/set-antialias => antialias
(cffi:defcfun ("cairo_font_options_set_antialias" %font-options-set-antialias)
    :void
  (options font-options-t) (aa-type font-antialias-type))
(defmacro font-options-set-antialias (options &optional aa-type)
  (if aa-type
      `(%font-options-set-antialias ,options (cffi:foreign-enum-value 'font-antialias-type ,aa-type))
      `(%font-options-set-antialias ,options ,(cffi:foreign-enum-value 'font-antialias-type :default))))
(export 'font-options-set-antialias)


(cffi:defcfun ("cairo_font_options_set_subpixel_order" %font-options-set-subpixel-order)
    :void
  (options font-options-t) (order subpixel-order-t))
(cffi:defcfun ("cairo_font_options_get_subpixel_order" %font-options-get-subpixel-order)
    subpixel-order-t
  (options font-options-t))
(defmacro font-options-subpixel-order (options &optional (order :default))
  (if order
      `(%font-options-set-subpixel-order ,options (cffi:foreign-enum-value 'subpixel-order-t ,order))
      `(%font-options-get-subpixel-order ,options)))
(export 'font-options-subpixel-order)


(cffi:defcfun ("cairo_font_options_set_hint_style" %font-options-set-hint-style)
    :void
  (options font-options-t) (hint-style font-hint-style-t))
(cffi:defcfun ("cairo_font_options_get_hint_style" %font-options-get-hint-style)
    font-hint-style-t
  (options font-options-t))
(defmacro font-options-hint-style (options &optional (hint-style :default))
  (if hint-style
      `(%font-options-set-hint-style ,options (cffi:foreign-enum-value 'font-hint-style-t ,hint-style))
      `(%font-options-get-hint-style ,options)))
(export 'font-options-hint-style)


(cffi:defcfun ("cairo_font_options_set_hint_metrics" %font-set-hint-metrics)
    :void
  (options font-options-t) (hint-metrics font-hint-metrics-t))
(cffi:defcfun ("cairo_font_options_get_hint_metrics" %font-get-hint-metrics)
    font-hint-metrics-t
  (options font-options-t))
(defmacro font-options-hint-metrics (config &optional hint-metrics)
  (if hint-metrics
      `(%font-set-hint-metrics ,config ,hint-metrics)
      `(%font-get-hint-metrics ,config)))
(export 'font-options-hint-metrics)
