;;;; http://nostdal.org/cl-cairo/ ;;;;

(in-package :cairo)

(let ((test-file (concatenate 'string (directory-namestring (user-homedir-pathname)) "cl-cairo-pdf-test.pdf")))

  (defun pdf-test ()
    (let* ((width 500d0) (height 500d0)
           (surface (pdf-surface-create test-file width height))
           (+cr+ (create surface)))

      ;; clear image-buffer
      (rectangle 0d0 0d0 width height)
      (set-source-rgb 0.5d0 0.5d0 0.8d0)
      (format t "I will now fill this area, (fill-extents) -> ~A~%" 
	      (multiple-value-list (fill-extents)))
      (fill)

      (select-font-face "sans" :normal :bold)
      (scale 5d0 5d0)
      (rotate (deg-to-rad 10d0))
      
      ;; shadow of text
      (set-source-rgb 0.3d0 0.3d0 0.3d0)
      (move-to 21d0 26d0)
      (show-text "cl-graph")
      
      ;; text
      (set-source-rgb 0d0 1d0 0.5d0)
      (move-to 20d0 25d0)
      (show-text "cl-graph")

      (show-page)
      (destroy)
      (surface-destroy surface))))
(export 'pdf-test)