;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)


(defcfun ("cairo_scaled_font_create" scaled-font-create)
    scaled-font-t
  (font-face font-face-t) (font-matrix matrix-t) (ctm matrix-t) (options font-options-t))
(export 'scaled-font-create)


(defcfun ("cairo_scaled_font_reference" scaled-font-reference)
    scaled-font-t
  (scaled-font scaled-font-t))
(export 'scaled-font-reference)


(defcfun ("cairo_scaled_font_destroy" scaled-font-destroy)
    :void
  (scaled-font scaled-font-t))
(export 'scaled-font-destroy)


(defcfun ("cairo_scaled_font_status" scaled-font-status)
    status-t
  (scaled-font scaled-font-t))
(export 'scaled-font-status)


(defcfun ("cairo_scaled_font_extents" %scaled-font-extents)
    :void
  (scaled-font scaled-font-t) (extents font-extents-t))
(defmacro scaled-font-extents (scaled-font)
  (let ((extents (gensym)))
    `(with-foreign-object (,extents font-extents-t)
      (%scaled-font-extents ,scaled-font ,extents))))
(export 'scaled-font-extents)


;; TODO:
#|
(defcfun ("cairo_scaled_font_glyph_extents" %scaled-font-glyph-extents)
    :void
  (scaled-font scaled-font-t) (glyphs glyph-t) (num-glyphs :int) (extents text-extents-t))
|#

