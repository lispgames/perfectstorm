;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)


;; NON-PORTABLE CODE STARTS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ignore-errors
  #-win32 (cffi:load-foreign-library "/usr/lib/libcairo.so")
  #+win32 (cffi:load-foreign-library "C:\\Workspace\\Repository\\lisp\\cairo\\bin\\libcairo-2.dll")
  (load-foreign-library "/usr/lib/libsvg-cairo.so"))

;;  NON-PORTABLE CODE ENDS  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar +cr+ nil "Current cairo context.")
(export '+cr+)

(defvar +surface+ nil)
(export '+surface+)


(defun deg-to-rad (deg)
  (* deg (/ pi 180)))
(export 'deg-to-rad)
