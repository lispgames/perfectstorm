;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)


(defcfun ("cairo_font_face_reference" font-face-reference)
    font-face-t
  (font-face font-face-t))
(export 'font-face-reference)


(defcfun ("cairo_font_face_destroy" font-face-destroy)
    :void
  (font-face font-face-t))
(export 'font-face-destroy)


(defcfun ("cairo_font_face_status" font-face-status)
    status-t
  (font-face font-face-t))
(export 'font-face-status)


;; TOOD: cairo_font_face_get_user_data


;; TODO: cairo_font_face_set_user_data

