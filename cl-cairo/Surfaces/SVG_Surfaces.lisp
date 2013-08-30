;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)

(defvar +svg+ nil)
(export '+svg+)


(cffi:defcenum svg-version-t
    :SVG-VERSION-1-1 :SVG-VERSION-1-2)
(export 'svg-version-t)


(cffi:defcfun ("cairo_svg_surface_create" svg-surface-create)
    surface-t
  (filename :string)
  (width-in-points :double)
  (height-in-points :double))
(export 'svg-surface-create)


(cffi:defcfun ("cairo_svg_surface_create_for_stream" svg-surface-create-for-stream)
    surface-t
  (write-func :pointer)
  (closure :pointer)
  (width-in-points :double)
  (height-in-points :double))
(export 'svg-surface-create-for-stream)


(cffi:defcfun ("cairo_svg_surface_restrict_to_version" %svg-surface-restrict-to-version)
    :void
  (surface surface-t)
  (version svg-version-t))
(defmacro svg-surface-restrict-to-version (version &optional svg)
  `(%svg-surface-restrict-to-version ,(if svg svg '+svg+) ,version))
(export 'svg-surface-restrict-to-version)


(cffi:defcfun ("cairo_svg_get_versions" %svg-get-versions)
    :void
  (versions :pointer)
  (num-versions :pointer))
(defmacro svg-get-versions ()
  "Return a list of supported SVG versions.
See section in README about return-values."
  (let ((versions (gensym)) (num-versions (gensym)))
    `(with-foreign-objects ((,versions :pointer) (,num-versions :pointer))
      (%svg-get-versions ,versions ,num-versions)
      (let* ((count (mem-ref ,num-versions :int))
	     (verslist (loop for i below count
			     collect (mem-aref (mem-ref ,versions :pointer) 'svg-version-t i))))
	(values verslist)))))
(export 'svg-get-versions)


(cffi:defcfun ("cairo_svg_version_to_string" svg-version-to-string)
    :string
  (version svg-version-t))
(export 'svg-version-to-string)

