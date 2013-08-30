;;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


(cffi:defcfun ("cairo_ft_font_create" ft-font-create)
    font-t
  (ft-library ft-library-t)
  (pattern fc-pattern-t))
(export 'ft-font-create)


(cffi:defcfun ("cairo_ft_font_create_for_ft_face" ft-font-create-for-ft-face)
    font-t
  (ft-face ft-face-t))
(export 'ft-font-create-for-ft-face)


(cffi:defcfun ("cairo_ft_font_face" ft-font-face)
    ft-face-t
  (ft-font font-t))
(export 'ft-font-face)


(cffi:defcfun ("cairo_ft_font_pattern" ft-font-pattern)
    fc-pattern-t
  (ft-font font-t))
(export 'ft-font-pattern)
