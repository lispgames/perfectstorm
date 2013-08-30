;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)


(defun version-encode (major minor micro)
  (+ (* major 10000) (* minor 100) micro))
(export 'version-encode)


(defcfun ("cairo_version" version)
    :int)
(export 'version)


(defcfun ("cairo_version_string" version-string)
    :string)
(export 'version-string)
