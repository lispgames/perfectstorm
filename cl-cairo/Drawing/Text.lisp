;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)


(defcairo "cairo_select_font_face" :void
  :string family
  font-slant-t slant
  font-weight-t weight)

(defcairo "cairo_set_font_size" :void
  :double size)


(defcairo "cairo_set_font_matrix" :void
  matrix-t matrix)
(defun get-font-matrix (&optional (+cr+ +cr+))
  (with-foreign-object (matrix 'matrix-t)
    (foreign-funcall "cairo_get_font_matrix"
                     cairo-t +cr+
                     matrix-t matrix)
    (mem-ref matrix 'matrix-t)))
(defsetget font-matrix)


(defcairo "cairo_set_font_options" :void
  font-options-t options)
(defun get-font-options (&optional (+cr+ +cr+))
  (with-foreign-object (options 'font-options-t)
    (foreign-funcall "cairo_get_font_options"
                     cairo-t +cr+
                     font-options-t options)
    (mem-ref options 'font-options-t)))
(defsetget font-options)

(defcairo "cairo_show_text" :void
  :string utf8)


;; TODO: cairo_show_glyphs
;; void cairo_show_glyphs (cairo_t *cr, cairo_glyph_t *glyphs, int num_glyphs);
;; User should not need to pass `num_glyphs' here, and what format should
;; `glyphs' be on the Lisp-side?


(defcairo "cairo_get_font_face")
(defcairo "cairo_set_font_face" :void
  font-face-t font-face)
(defsetget font-face)


(defun font-extents (&optional (+cr+ +cr+))
  (with-foreign-object (extents 'font-extents-t)
    (foreign-funcall "cairo_font_extents"
                     font-extents-t extents
                     :void)
    (mem-ref extents 'font-extents-t)))


(defun (setf scaled-font) (scaled-font &optional (+cr+ +cr+))
  (foreign-funcall "cairo_set_scaled_font"
                   cairo-t +cr+
                   scaled-font-t scaled-font))
(export 'scaled-font)
                   

(defun text-extents-of (utf8 &optional (+cr+ +cr+))
  (with-foreign-object (extents 'text-extents-t)
    (foreign-funcall "cairo_text_extents"
                     cairo-t +cr+
                     :string utf8
                     text-extents-t extents)
    (mem-ref extents 'text-extents-t)))
(export 'text-extents)


;; TODO: cairo_glyph_extents

