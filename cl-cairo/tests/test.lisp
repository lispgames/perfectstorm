;;;; http://nostdal.org/cl-cairo/ ;;;;

;(in-package :cairo)


(let ((test-file (concatenate 'string (directory-namestring (user-homedir-pathname))
                              "cl-cairo-test-cupe.png")))
  (defun test ()
    (let* ((width 64d0) (height 64d0)
	   (surface (cairo:image-surface-create :argb32 64 64))
	   (cairo:+cr+ (cairo:create surface)))
      
      ;; clear image-buffer
      (cairo:rectangle 0d0 0d0 width height)
      (cairo:set-source-rgba 0.0d0 0.0d0 0.5d0 0.2d0)
      (format t "I will now fill this area, (fill-extents) -> ~A~%" 
	      (multiple-value-list (cairo:fill-extents)))
      (cairo:fill)
      
      ;; draw something
      (cairo:set-source-rgb 1d0 0.5d0 0d0)
      (cairo:arc 32d0 32d0 25d0 0.0d0 PI)
      (format t "(get-current-point) -> ~A~%" (multiple-value-list (cairo:get-current-point)))
      (cairo:stroke)
      
      ;; write to file, and close down
      (cairo:surface-write-to-png surface test-file)
      (cairo:destroy)
      (cairo:surface-destroy surface))))
