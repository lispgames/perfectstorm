;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)

;; TODO: Simplify
(defcfun ("cairo_create" %create)
    cairo-t
  (target surface-t))
(defmacro create (&optional surface)
  (if surface
      `(%create ,surface)
      `(%create +surface+)))
(export 'create)


(defcairo "cairo_reference" cairo-t)
(defcairo "cairo_destroy")
(defcairo "cairo_status" status-t)
(defcairo "cairo_save")
(defcairo "cairo_restore")
(defcairo "cairo_get_target" surface-t)
(defcairo "cairo_push_group")

(defcairo "cairo_push_group_with_content" :void
  content-t content)

(defcairo "cairo_pop_group" pattern-t)
(defcairo "cairo_pop_group_to_source")
(defcairo "cairo_get_group_target" surface-t)

(defcairo "cairo_set_source_rgb" :void
  :double red :double green :double blue)

(defcairo "cairo_set_source_rgba" :void
  :double red :double green :double blue :double alpha)

(defcairo "cairo_set_source" :void
  pattern-t source)
(defcairo "cairo_get_source" pattern-t)
(defsetget source)

(defcairo "cairo_set_source_surface" :void
  surface-t surface
  :double x :double y)

(defcairo "cairo_set_antialias" :void
  antialias-t antialias)
(defcairo "cairo_get_antialias" antialias-t)
(defsetget antialias)

;; TODO: (defcairo "cairo_set_dash" :void)

(defcairo "cairo_set_fill_rule" :void
  fill-rule-t fill-rule)
(defcairo "cairo_get_fill_rule" fill-rule-t)
(defsetget fill-rule)

(defcairo "cairo_set_line_cap" :void
  line-cap-t line-cap)
(defcairo "cairo_get_line_cap" line-cap-t)
(defsetget line-cap)

(defcairo "cairo_set_line_join" :void
  line-join-t line-join)
(defcairo "cairo_get_line_join" line-join-t)
(defsetget line-join)

(defcairo "cairo_set_line_width" :void
  :double width)
(defcairo "cairo_get_line_width" :double)
(defsetget line-width)

(defcairo "cairo_set_miter_limit" :void
  :double limit)
(defcairo "cairo_get_miter_limet" :double)
(defsetget miter-limit)

(defcairo "cairo_set_operator" :void
  operator-t op)
(defcairo "cairo_get_operator" operator-t)
(defsetget operator)

(defcairo "cairo_set_tolerance" :void
  :double tolerance)
(defcairo "cairo_get_tolerance" :double)
(defsetget tolerance)

(defcairo "cairo_clip")
(defcairo "cairo_clip_preserve")
(defcairo "cairo_reset_clip")
(defcairo "cairo_fill")
(defcairo "cairo_fill_preserve")


;; TODO: Simplify this
(defcfun ("cairo_fill_extents" %fill-extents)
    :void
  (cr cairo-t) (x1 :pointer) (y1 :pointer) (x2 :pointer) (y2 :pointer))
(defmacro fill-extents (&optional cr)
  "See section in README about return-values."
  (let ((fx1 (gensym)) (fy1 (gensym)) (fx2 (gensym)) (fy2 (gensym)))
    (if cr
        `(with-foreign-objects ((,fx1 :double) (,fy1 :double)
                                     (,fx2 :double 0d0) (,fy2 :double))
          (%fill-extents ,cr ,fx1 ,fy1 ,fx2 ,fy2)
          (values (mem-ref ,fx1 :double) (mem-ref ,fy1 :double)
           (mem-ref ,fx2 :double) (mem-ref ,fy2 :double)))
        `(with-foreign-objects ((,fx1 :double) (,fy1 :double)
                                     (,fx2 :double) (,fy2 :double))
          (%fill-extents +cr+ ,fx1 ,fy1 ,fx2 ,fy2)
          (values (mem-ref ,fx1 :double) (mem-ref ,fy1 :double)
           (mem-ref ,fx2 :double) (mem-ref ,fy2 :double))))))
(export 'fill-extents)


(defcairo "cairo_in_fill" bool-t
  :double x :double y)

(defcairo "cairo_mask" :void
  pattern-t pattern)

(defcairo "cairo_mask_surface" :void
  surface-t surface
  :double surface-x :double surface-y)

(defcairo "cairo_paint")

(defcairo "cairo_paint_with_alpha" :void
  :double alpha)

(defcairo "cairo_stroke")
(defcairo "cairo_stroke_preserve")


;; TODO: Simplify this.
(defcfun ("cairo_stroke_extents" %stroke-extents)
    :void
  (cr cairo-t) (x1 :pointer) (y1 :pointer) (x2 :pointer) (y2 :pointer))
(defmacro stroke-extents (&optional cr)
  "See section in README about return-values."
  (let ((fx1 (gensym)) (fy1 (gensym)) (fx2 (gensym)) (fy2 (gensym)))
    (if cr
        `(with-foreign-objects ((,fx1 :double) (,fy1 :double)
                                     (,fx2 :double) (,fy2 :double))
          (%stroke-extents ,cr ,fx1 ,fy1 ,fx2 ,fy2)
          (values (mem-ref ,fx1 :double) (mem-ref ,fy1 :double)
           (mem-ref ,fx2 :double) (mem-ref ,fy2 :double)))
        `(with-foreign-objects ((,fx1 :double) (,fy1 :double)
                                     (,fx2 :double) (,fy2 :double))
          (%stroke-extents +cr+ ,fx1 ,fy1 ,fx2 ,fy2)
          (values (mem-ref ,fx1 :double) (mem-ref ,fy1 :double)
           (mem-ref ,fx2 :double) (mem-ref ,fy2 :double))))))
(export 'stroke-extents)


(defcairo "cairo_in_stroke" bool-t
  :double x :double y)

(defcairo "cairo_copy_page")
(defcairo "cairo_show_page")
