;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)


(defcairo "cairo_copy_path" path-t)
(defcairo "cairo_copy_path_flat" path-t)

(defcairo "cairo_path_destroy" :void
  path-t path)

(defcairo "cairo_append_path" :void
  path-t path)


;; TODO: Simplify this:
(defcfun ("cairo_get_current_point" %get-current-point)
    :void
  (cr cairo-t) (x :pointer) (y :pointer))
(defmacro get-current-point (&optional cr)
  "See section in README about return-values."
  (let ((fx (gensym)) (fy (gensym)))
    (if cr
        `(with-foreign-objects ((,fx :double) (,fy :double))
          (%get-current-point ,cr ,fx ,fy)
          (values (mem-ref ,fx :double) (mem-ref ,fy :double)))
        `(with-foreign-objects ((,fx :double) (,fy :double))
          (%get-current-point +cr+ ,fx ,fy)
          (values (mem-ref ,fx :double) (mem-ref ,fy :double))))))
(export 'get-current-point)


(defcairo "cairo_new_path")
(defcairo "cairo_new_sub_path")
(defcairo "cairo_close_path")

(defcairo "cairo_arc" :void
  :double xc :double yc
  :double radius
  :double angle1 :double angle2)

(defcairo "cairo_arc_negative" :void
  :double xc :double yc
  :double radius
  :double angle1 :double angle2)

(defcairo "cairo_curve_to" :void
  :double x1 :double y1
  :double x2 :double y2
  :double x3 :double y3)

(defcairo "cairo_line_to" :void
  :double x :double y)

(defcairo "cairo_move_to" :void
  :double x :double y)

(defcairo "cairo_rectangle" :void
  :double x :double y
  :double width :double height)

;; TODO: `cairo_glypth_path'

(defcairo "cairo_text_path" :void
  :string utf8)

(defcairo "cairo_rel_curve_to" :void
  :double dx1 :double dy1
  :double dx2 :double dy2
  :double dx3 :double dy3)

(defcairo "cairo_rel_line_to" :void
  :double dx :double dy)

(defcairo "cairo_rel_move_to" :void
  :double dx :double dy)
    
