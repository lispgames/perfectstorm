;;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


(let ((test-file (concatenate 'string (directory-namestring (user-homedir-pathname)) "cl-cairo-font-test.png")))


  (defun font-test ()
    (let* ((width 420d0) (height 290d0)
	   (surface (image-surface-create :argb32 (round width) (round height)))
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
        
      (let ((text "cl-cairo"))
        ;; shadow of text
        (set-source-rgb 0.3d0 0.3d0 0.3d0)
        (move-to 21d0 26d0)
        (show-text text)
        
        ;; text
        (set-source-rgb 0d0 1d0 0.5d0)
        (move-to 20d0 25d0)
        (show-text text))

      ;; write to file, and close down
      (surface-write-to-png surface test-file)
      (destroy)
      (surface-destroy surface)))
  (export 'font-test))

(font-test) ;; Uncomment this and play with values.