;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)


;; Drawing
;;;;;;;;;;

;;; cairo_t
;;;;;;;;;;;

(defctype cairo-t :pointer)
(export 'cairo-t)


(defcenum operator-t
  :clear 
  :source :over :in :out :atop
  :dest :dest-over :dest-in :dest-out :dest-atop
  :xor :add :satuate)
(export 'operator-t)


(defcenum antialias-t
  :default :none :gray :subpixel)
(export 'antialias-t)


(defcenum fill-rule-t
  :winding :even-odd)
(export 'fill-rule-t)


(defcenum line-cap-t
  :butt :round :square)
(export 'line-cap-t)


(defcenum line-join-t
  :miter :round :bevel)
(export 'line-join-t)


;;; Paths
;;;;;;;;;

;; TODO:
;; http://www.cairographics.org/manual/cairo-Paths.html#cairo-path-t
(defctype path-t :pointer)
(export 'path-t) 

;; TODO: 
;; * http://www.cairographics.org/manual/cairo-Paths.html#cairo-path-data-t
;; * http://www.cairographics.org/manual/cairo-Paths.html#cairo-path-data-type-t


;;; Patterns
;;;;;;;;;;;;

(defcenum extend-t
  :none :repeat :reflect)
(export 'extend-t)


(defcenum filter-t
  :fast :good :best :nearest :bilinear :gaussian)
(export 'filter-t)


(defctype pattern-t :pointer)
(export 'pattern-t)

(defcenum pattern-type-t
  :solid :surface :linear :radial)
(export 'pattern-type-t)


;;; Text
;;;;;;;;

(defcstruct font-extents-t
  (ascent :double)
  (descent :double)
  (height :double)
  (max-x-advance :double)
  (max-y-advance :double))
(export 'font-extents-t)


(defcstruct text-extents-t
  (x-bearing :double)
  (y-bearing :double)
  (width :double)
  (height :double)
  (x-advance :double)
  (y-advance :double))
(export 'text-extents-t)


(defcstruct glyph-t
    (index :unsigned-long)
    (x :double) 
    (y :double))
(export 'glyph-t)


(defcenum font-slant-t
  :normal :italic :oblique)
(export 'font-slant-t)


(defcenum font-weight-t
  :normal :bold)
(export 'font-weight-t)


;; Fonts
;;;;;;;;

;;; cairo_font_face_t
;;;;;;;;;;;;;;;;;;;;;

(defctype font-face-t :pointer)
(export 'font-face-t)


;;; Scaled Fonts
;;;;;;;;;;;;;;;;

(defctype scaled-font-t :pointer)
(export 'scaled-font-t)


;;; Font Options
;;;;;;;;;;;;;;;;

(defcenum font-antialias-type
  :default :none :gray :subpixel)
(export 'font-antialias-type)


(defcenum subpixel-order-t
  :default :rgb :bgr :vrgb :vbgr)
(export 'subpixel-order-t)


(defcenum font-hint-style-t
  :default :none :slight :medium :full)
(export 'font-hint-style-t)


(defcenum font-hint-metrics-t
  :default :on :off)
(export 'font-hint-metrics-t)


(defctype font-options-t :pointer)
(export 'font-options-t)


;; FreeType Fonts
;;;;;;;;;;;;;;;;;

(defctype font-t :pointer)
(export 'font-t)


(defctype ft-face-t :pointer)
(export 'ft-face-t)


(defctype fc-pattern-t :pointer)
(export 'fc-pattern-t)


(defctype ft-library-t :pointer)
(export 'ft-library-t)


;; Surfaces
;;;;;;;;;;;

;;; cairo_surface_t
;;;;;;;;;;;;;;;;;;;

(defctype surface-t :pointer)
(export 'surface-t)


(defcenum content-t
  (:color #x1000) (:alpha #x2000) (:color-alpha #x3000))
(export 'content-t)


;;; Image Surfaces
;;;;;;;;;;;;;;;;;;

(defcenum format-t
  :argb32 :rgb24 :a8 :a1)
(export 'format-t)


;; Utilities
;;;;;;;;;;;;

;;; cairo_matrix_t
;;;;;;;;;;;;;;;;;;

(defcstruct matrix-t 
    (xx :double) (yx :double) 
    (xy :double) (yy :double)
    (x0 :double) (y0 :double))
(export 'matrix-t) 


;;; Error handling
;;;;;;;;;;;;;;;;;;

(defcenum status-t
  :success 
  :no-memory 
  :invalid-restore 
  :invalid-pop-group 
  :no-current-point 
  :invalid-matrix
  :invalid-status
  :null-pointer
  :invalid-string 
  :invalid-path-data 
  :read-error
  :write-error
  :surface-finished
  :surface-type-mismatch
  :pattern-type-mismatch
  :invalid-content
  :invalid-format
  :invalid-visual
  :file-not-found
  :invalid-dash)
(export 'status-t)


;;; Types
;;;;;;;;;

(defctype bool-t :int)
(export 'bool-t)


;; TODO: http://www.cairographics.org/manual/cairo-Types.html#cairo-user-data-key-t


