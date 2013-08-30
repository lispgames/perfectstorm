;;;; http://nostdal.org/cl-cairo/ ;;;;


;; TODO module dependencies doesn't seem to work
(defsystem cl-cairo
  :description "Cairo bindings."
  :version "1.0"
  :author "http://lars.nostdal.org/"
  :licence "LLGPL"
  :components ((:file "package")
               (:file "tools" :depends-on ("package"))
               (:file "cl-cairo" :depends-on ("tools"))
	       (:file "types" :depends-on ("cl-cairo"))
	       (:file "Drawing/cairo_t" :depends-on ("types"))
	       (:file "Drawing/Paths" :depends-on ("types"))
	       (:file "Drawing/Patterns" :depends-on ("types"))
	       (:file "Drawing/Transformations" :depends-on ("types"))
	       (:file "Drawing/Text" :depends-on ("types"))
	       (:file "Fonts/cairo_font_face_t" :depends-on ("types"))
	       (:file "Fonts/Scaled_Fonts" :depends-on ("types"))
	       (:file "Fonts/Font_Options" :depends-on ("types"))
	       (:file "Fonts/FreeType_Fonts" :depends-on ("types"))
	       #|(:file "Fonts/Win32_Fonts" :depends-on ("types"))|#
	       (:file "Surfaces/cairo_surface_t" :depends-on ("types"))
	       (:file "Surfaces/Image_Surfaces" :depends-on ("types"))
	       (:file "Surfaces/Glitz_Surfaces" :depends-on ("types"))
	       (:file "Surfaces/PDF_Surfaces" :depends-on ("types"))
	       (:file "Surfaces/PNG_Support" :depends-on ("types"))
	       (:file "Surfaces/PostScript_Surfaces" :depends-on ("types"))
               (:file "Surfaces/SVG_Surfaces" :depends-on ("types"))
	       (:file "Surfaces/Win32_Surfaces" :depends-on ("types"))
	       (:file "Surfaces/XLib_Surfaces" :depends-on ("types"))
	       (:file "Utilities/cairo_matrix_t" :depends-on ("types"))
	       (:file "Utilities/Error_handling" :depends-on ("types"))
	       (:file "Utilities/Version_Information" :depends-on ("types")))
  :depends-on (cffi))
