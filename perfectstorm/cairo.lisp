(in-package :storm)

(defparameter *relative-line-width* 0.05)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun numbers-to-doubles (tree)
                                        ;traverses a nested list, coercing all numbers into double-floats. useful for the stupid cairo bindings
    (loop for element in tree
          collecting
          (cond
            ((listp element) (numbers-to-doubles element))
            ((numberp element) (coerce element 'double-float))
            (t element)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; font building
;;
;;


(defparameter *char-width*  16)
(defparameter *char-height* 32)
(defparameter *char-textures* (make-array 256))


(defun make-chars (width height color)
  (loop for char-code from 32 to 126 do
      (let* ((surface (cairo:image-surface-create :argb32 width height))
             (width (coerce width 'double-float))
             (height (coerce height 'double-float))
             (cairo:+cr+ (cairo:create surface)) ; set cairo default surface
             (texture-name (first (gl:gen-textures 1))) ;create new opengl texture "name" (i.e. get an unused texture id)
             (line-width (* (min width height) *relative-line-width*))) ;some arbitray line width that scales with the image
        (format t "~a" (code-char char-code))
        (cairo:set-line-width line-width)




        (set-source-color color)
        (cairo:select-font-face "Luxi Mono" :normal :bold)
        (cairo:set-font-size (* height 0.7))
        (cairo:move-to (* width 0.05) (* height 0.7))
        (cairo:show-text (format nil "~a" (code-char char-code)))

        
        (gl:bind-texture :texture-2d texture-name)
        (gl:tex-image-2d :texture-2d 0
                         :rgba width height 0 :bgra :unsigned-byte 
                         (cairo:surface-get-data-as-array surface 4))

        (gl:tex-parameter :texture-2d :texture-min-filter :linear )
        (gl:tex-parameter :texture-2d :texture-mag-filter :linear )
        (setf (aref *char-textures* char-code) texture-name)
    
        (cairo:surface-destroy surface))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helper functions and macros
;;


(defmacro def-texture-method (type &body body)
  (with-gensyms (surface-symbol texture-name-symbol data-symbol)
    `(defmethod make-texture ((type (eql ',type)) width height &optional color)
                                        ;(declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
      (let* ((,surface-symbol (cairo:image-surface-create :argb32 width height))
             (width (coerce width 'double-float))
             (height (coerce height 'double-float))
             (cairo:+cr+ (cairo:create ,surface-symbol)) ; set cairo default surface
             (,texture-name-symbol (first (gl:gen-textures 1))) ;create new opengl texture "name" (i.e. get an unused texture id)
             (line-width (* (min width height) *relative-line-width*))) ;some arbitray line width that scales with the image
        (cairo:set-line-width line-width)
        ,@(numbers-to-doubles body)
        (gl:bind-texture :texture-2d ,texture-name-symbol)
        (let ((,data-symbol (cairo:surface-get-data-as-array ,surface-symbol 4)))
          (gl:tex-image-2d :texture-2d 0
                           :rgba width height 0 :bgra :unsigned-byte 
                           ,data-symbol)
         ; (make-mipmaps width height ,data-symbol)
         ; ,(when (eql type 'glow)
         ;        `(print ,data-symbol))
          (glu:build-2d-mipmaps :texture-2d :rgba width height :bgra :unsigned-byte ,data-symbol)
           

          (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
          (gl:tex-parameter :texture-2d :texture-mag-filter :linear )
          (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
          (gl:tex-parameter :texture-2d :texture-wrap-t :repeat))
    
        (cairo:surface-destroy ,surface-symbol)
        ,texture-name-symbol))))


(defmacro double (form)
  `(coerce ,form 'double-float))

(defun mix-colors (color-with-factor color) ;colors represented as (r g b a factor) and (r g b a). make classes for these?
  (let ((factor (car (last color-with-factor)))
        (base-color (butlast color-with-factor)))
    (mapcar (lambda (value)
              (double (crop value 1 0)))
            (mapcar #'+ base-color (mapcar (lambda (value)
                                             (* factor value))
                                           color)))))

(defun set-source-color (color)
  (let ((color (numbers-to-doubles color)))
    (cairo:set-source-rgba (first color) (second color) (third color) (fourth color))))


  

(defmacro with-gradient ((gradient-name type anchor-definitions definition) &body body)
  (with-gensyms (offset-name r-name g-name b-name a-name)
    (let ((new-anchor-definitions
           (loop for definition in anchor-definitions
                 collecting `(double ,definition))))
      `(let ((,gradient-name
              (,(case type
                      (:radial 'cairo:pattern-create-radial)
                      (:linear 'cairo:pattern-create-linear))
                ,@new-anchor-definitions)))
        (mapcar (lambda (base-color)
                  (destructuring-bind (,offset-name ,r-name ,g-name ,b-name ,a-name) (cons (first base-color)
                                                                                           (mix-colors (rest base-color) color))
                    (cairo:pattern-add-color-stop-rgba ,gradient-name ,offset-name ,r-name ,g-name ,b-name ,a-name)))
         (numbers-to-doubles ,definition))
        ,@body))))


(defun to-absolute-coords (width height coord-list &key (border 0.0d0))
  (let ((inner-width  (* width  (- 1 (* 2 border))))
        (inner-height (* height (- 1 (* 2 border)))))
    (mapcar (lambda (coords)
              (list (+ (* width border)
                       (* inner-width  (first coords)))
                    (+ (* height border)
                       (* inner-height (second coords)))))
            coord-list)))


(defun relative-path (width height surface path-elements &key (border 0.0d0))
  (let ((cairo:+cr+ surface)
        (path (to-absolute-coords width height path-elements :border border)))
    (cairo:move-to (first (first path)) (second (first path)))
    (dolist (element (rest path))
      (cairo:line-to (first element) (second element)))
    (cairo:close-path)))

(defun relative-arc (width height cx cy radius angle1 angle2)
  (let* ((center (make-point (* cx width) (* cy height)))
         (start (add center
                     (rotate (make-point 0 radius) angle1))))
   ; (cairo:move-to (x start) (y start))
    (cairo:arc (x center) (y center) (coerce radius 'double-float) (coerce angle1 'double-float) (coerce angle2 'double-float))))




(defun mirror-coords (coords) ;for symmetrical paths
  (append coords (mapcar (lambda (p)
                           (list (first p) (- 1 (second p))))
                         (reverse coords))))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; actual texture definitions
;;
;;
;; the secrets of the gradient color matrix (or of mixed colors in general)
;; '((offset r g b a factor)
;;   ( ...                 ))
;; offset:  [0..1] location of the color stop within the gradient
;; r,g,b,a: base color
;; factor:  [-1..1] argument color is multiplied by this factor and then added to the base color
;;

(def-texture-method weapon
  (with-gradient (weapon-gradient :radial ((/ width 2) (/ height 2)
                                           0
                                           (/ width 2) (/ height 2)
                                           (/ width 2)) ; will break if width != height
                                  '((0    0   0   0   0     0.3)
                                    (0.15 0   0   0   0     1  )
                                    (0.6  0.5 0.5 0.5 0.5   0.5)
                                    (0.7  1   1   1   0.8   0.2)
                                    (0.8  0   0   0   0     0  )))
    (cairo:rectangle  0.0d0  0.0d0 (- width 0) height)
    (cairo:set-source weapon-gradient)
                   
    (cairo:fill)))

(def-texture-method ground-unit
  (with-gradient (ground-unit-gradient :radial ((/ width 2) (/ height 2)
                                               0
                                               (/ width 2) (/ height 2)
                                               (/ width 2))  ; will break if width != height
                             '((0    0   0   0   0   0.5)
                               (0.5  0   0   0   0   0.5)
                               (0.8  0   0   0   0   1  )
                               (0.95 0   0   0   0   1  )
                               (1.0  0   0   0   1   0  )))
      
                    (cairo:rectangle  0.0d0  0.0d0 (- width 0) height)
                    (cairo:set-source ground-unit-gradient)
                    
                    (cairo:fill)))


(def-texture-method air-unit
  (let ((exhaust-scale 0.5d0))
    (cairo:scale 1.0d0 exhaust-scale)
    (with-gradient (ground-unit-gradient :radial ((* width 0.7 ) (/ height 2)
                                                  0
                                                  (* width 0.7) (/ height 2)
                                                  (* width 0.3))
                                         '((0    1   1   1   1   0  )
                                           (0.15 1   1   1   1   0  )
                                           (0.2  1   1   0.5 1   0  )
                                           (0.4  1   0.8 0.3 1   0  )
                                           (0.7  1   0.5 0   0.6 0  )
                                           (1.0  0.5 0   0   0   0  )))
    
    
      (cairo:translate 0.0d0 (* height  0.5)); (/ exhaust-scale))))
      (cairo:rectangle  (* width 0.0d0)  (* height 0.0d0) width height)

      (cairo:set-source ground-unit-gradient)
                    
      (cairo:fill))

  (cairo:translate 0.0d0 (* height  -0.5)); (/ exhaust-scale))))
  (cairo:scale (/ 1.0d0) (/ exhaust-scale)))
  (let ((path (mirror-coords
               '((0.08 0.5  )
                 (0.15 0.56 )
                 (0.23 0.57 )
                 (0.6  0.9  )
                 (0.4  0.925)
                 (0.6  0.95 )
                 (0.8  0.95 )
                 (0.7  0.9  )
                 (0.55 0.57 )
                 (0.65 0.55 )))))

    (relative-path width height cairo:+cr+ path)
    (set-source-color (mix-colors '(0 0 0 0.5 0.5) color))
    (cairo:fill)
    (relative-path width height cairo:+cr+ path)
    (set-source-color color)
    (cairo:stroke)))
                                

(def-texture-method bullet
  (with-gradient (bullet-gradient :radial ((* width 0.5) (* height 0.5)
                                           0
                                           (* width 0.74) (/ height 2)
                                           (/ width 4)) ; will break if width != 2* height
                                  '((0    1.0 0.9 0.6 1.0   0)
                                    (0.2  1.0 0.9 0.2 1.0   0)
                                    (0.7  1.0 0.9 0.2 0.6   0)
                                    (1.0  1.0 0.9 0.2 0.0   0)))
    (cairo:rectangle  0.0d0  0.0d0 (- width 0) height)
    (cairo:set-source bullet-gradient)
                   
    (cairo:fill)))


(def-texture-method explosion
  (with-gradient (explosion-gradient :radial ((/ width 2) (/ height 2)
                                               0
                                               (/ width 2) (/ height 2)
                                               (/ width 2))  ; will break if width != height
                             '((0    0   0   0   0   0)
                               (0.2  0.1 0.0 0.0 1   0)
                               (0.7  0.4 0.4 0.0 1   0)
                               (0.9  0.8 0.5 0.4 1   0)
                               (0.95 1.0 0.8 0.7 1   0)
                               (1.0  0   0   0   1   0)))
      
                    (cairo:rectangle  0.0d0  0.0d0 (- width 0) height)
                    (cairo:set-source explosion-gradient)
                    
                    (cairo:fill)))



(defparameter *beam-gradient-table*
             '((0    0   0   0   0     0)
               (0.2  0.1 0.1 0.1 0.5   0.2)
               (0.4  0.2 0.2 0.2 0.8   0.3)
               (0.5  0.9 0.9 0.9 0.9   0.2)
               (0.6  0.2 0.2 0.2 0.8   0.3)
               (0.8  0.1 0.1 0.1 0.5   0.2)
               (1    0   0   0   0     0)))
             

(def-texture-method laser-beam
  (with-gradient (beam-gradient :linear (0 0
                                           0 height)
                                *beam-gradient-table*)
    (cairo:rectangle  0.0d0  0.0d0 width height)
    (cairo:set-source beam-gradient)
                   
    (cairo:fill)))

(def-texture-method laser-beam-cap
  (with-gradient (beam-gradient :radial ((/ width 2) (/ height 2)
                                         0.0d0
                                         (/ width 2) (/ height 2)
                                         (/ width 2))
                                *beam-gradient-table*)
    (cairo:rectangle  0.0d0  0.0d0 width height)
    (cairo:set-source beam-gradient)
    (cairo:fill)
    (cairo:rectangle  0.0d0  0.0d0 (/ width 2) height)
    (set-source-color '(0 0 0 1))
    (cairo:fill)))


(def-texture-method visible-line
  (with-gradient (line-gradient :linear (0 0
                                           0 height)
                                '((0    0   0   0   0     0)
                                  (0.5  1.0 1.0 1.0 1.0   0)
                                  (1    0   0   0   0     0)))
    (cairo:rectangle  0.0d0  0.0d0 (- width 0) height)
    (cairo:set-source line-gradient)
                   
    (cairo:fill)))

(def-texture-method glow
  (with-gradient (glow-gradient :radial ((/ width 2) (/ height 2)
                                         0.0d0
                                         (/ width 2) (/ height 2)
                                         (/ width 2)) ; will break if width != height
                                '((0    1   1   1   1     0)
                                  (0.05 1   1   1   1     0)
                                  (0.15 0.8 0.8 0.8 0.9   0.3)
                                  (0.9  0.2 0.2 0.0 0.2   0.1)
                                  (1.0  0   0   0.0 0     0.1)))
    (cairo:rectangle  0.0d0  0.0d0 (- width 0) height)
    (cairo:set-source glow-gradient)
                   
    (cairo:fill)))


(def-texture-method selection-indicator
  (with-gradient (gradient :radial ((/ width 2) (/ height 2)
                                               0
                                               (/ width 2) (/ height 2)
                                               (/ width 2))  ; will break if width != height
                             '((0    0   0   0   0   0  )
                               (0.7  0   0   0   0   0  )
                               (0.8  0   0   0   0   1  )
                               (0.95 0   0   0   0   1  )
                               (1.0  0   0   0   1   0  )))
      
                    (cairo:rectangle  0.0d0  0.0d0 (- width 0) height)
                    (cairo:set-source gradient)
                    
                    (cairo:fill)))
  
(def-texture-method rocket
  (let ((exhaust-scale 0.5d0))
    (cairo:scale 1.0d0 exhaust-scale)
    (with-gradient (ground-unit-gradient :radial ((* width 0.7 ) (/ height 2)
                                                  0
                                                  (* width 0.7) (/ height 2)
                                                  (* width 0.3))
                                         '((0    1   1   1   1   0  )
                                           (0.15 1   1   1   1   0  )
                                           (0.2  1   1   0.5 1   0  )
                                           (0.4  1   0.8 0.3 1   0  )
                                           (0.7  1   0.5 0   0.6 0  )
                                           (1.0  0.5 0   0   0   0  )))
    
    
      (cairo:translate 0.0d0 (* height  0.5)); (/ exhaust-scale))))
      (cairo:rectangle  (* width 0.0d0)  (* height 0.0d0) width height)

      (cairo:set-source ground-unit-gradient)
                    
      (cairo:fill))

  (cairo:translate 0.0d0 (* height  -0.5)); (/ exhaust-scale))))
  (cairo:scale (/ 1.0d0) (/ exhaust-scale)))
  (let ((path (mirror-coords
               '((0.05 0.5  )
                 (0.1 0.57 )
                 (0.15 0.6  )
                 (0.5  0.6  )
                 (0.7  0.8  )
                 (0.9  0.9  )
                 (0.8  0.7  )
                 (0.7  0.65 )))))

    (relative-path width height cairo:+cr+ path)
    (set-source-color (mix-colors '(0 0 0 0 1) color))
    (cairo:fill)
    (relative-path width height cairo:+cr+ path)
    (set-source-color color)
    (cairo:stroke)))

(def-texture-method full-health-element
  (set-source-color '(0 255 0 1))
  (relative-path width height cairo:+cr+
                 '((0 0)
                   (0 1)
                   (1 0.7)
                   (1 0.3))
                 :border *relative-line-width*)
  (cairo:fill))

(def-texture-method empty-health-element
  (set-source-color '(1 0 0 1))
  (relative-path width height cairo:+cr+
                 '((0 0)
                   (0 1)
                   (1 0.7)
                   (1 0.3))
                 :border *relative-line-width* )
  (cairo:fill))



(def-texture-method cursor
  (let ((path (mirror-coords
               '((0.52 0.5 )
                 (0.75 0.7 )
                 (0.7  0.55)
                 (0.9  0.55)
                 (0.95 0.5 )))))
    
    (cairo:set-line-width (/ line-width 2))

    (relative-path width height cairo:+cr+ path)
    (set-source-color (mix-colors '(0 0 0 0.5 0.5) color))
    (cairo:fill)
    (relative-path width height cairo:+cr+ path)
    (set-source-color color)
    (cairo:stroke)))





(def-texture-method factory
  (let ((path    '((0 0)
                   (1 0)
                   (1 1)
                   (0 1))))
    (relative-path width height cairo:+cr+ path :border *relative-line-width*)
    (set-source-color (mix-colors '(0 0 0 0.5 0.5) color))
    (cairo:fill)

    (set-source-color color)

    (relative-path width height cairo:+cr+ path :border *relative-line-width*)
    (cairo:stroke)

    (relative-path width height cairo:+cr+ path :border (* 3 *relative-line-width*))
    (cairo:stroke)))


    





;;;;;;;;;;;;;;;;;;
;;
;; mip mapping. the following code is redundant since cl-opengl-thomas has been patched to
;; support mip mapping. but i can't delete it, it makes me sad :(
;;


(defun shrink (width height data) ;shrink an rgba image of the given size by one half in each dimension
  (let* ((new-width (round (/ width 2)))
         (new-height (round (/ height 2)))
         (new-length (* new-width new-height 4))
         (new-data (make-array  new-length
                               :element-type 'unsigned-byte
                               :initial-element 0
                               :fill-pointer new-length)))
        (labels ((get-pixel-index (x y width)
                   (* 4 (+ x
                           (* width y))))
                 
                 (average-pixel-component (list-of-coords color-index)
                   (let ((average 0))
                     (dolist (coords list-of-coords)
                       (destructuring-bind (x y) coords
                         (setf average
                               (+ average (aref data (+ color-index (get-pixel-index x y (round width))))))))
                     (round (/ average (length list-of-coords))))))
          
          (dotimes (new-x new-width)
            (dotimes (new-y new-height)
              (let ((source-coords (mapcar (lambda (coords)
                                             (list (+ (* 2 new-x) (first coords)) (+ (* 2 new-y) (second coords))))
                                           '((0 0) (0 1) (1 0) (1 1)))))
                (dotimes (color-index 4)
                  (setf (aref new-data (+ color-index (get-pixel-index new-x new-y new-width)))
                        (average-pixel-component source-coords color-index)))))))
    new-data))


(defun make-mipmaps (width height data &key (level 1)) ;creates and stores mipmaps of a texture. assumes texture is already bound
  (when (and
          (> (round width) 1)
          (> (round height) 1))
    (format t "~%making mipmap of size ~a,~a~%" width height)
    (let* ((new-width (round (/ width 2)))
           (new-height (round (/ height 2)))
           (new-data (shrink width height data)))
      (gl:tex-image-2d :texture-2d level   
                       :rgba new-width new-height 0 :bgra :unsigned-byte
                       new-data)
      (make-mipmaps new-width new-height new-data :level (+ level 1)))))

                     
                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; texture generation control
;;

(defparameter *texture-sizes*
  '(ground-unit         (128 128)
    air-unit            (128 128)
    factory             (128 128)
    weapon              ( 64  64)
    bullet              (128 128)
    explosion           (256 256)
    laser-beam          ( 32   1)
    laser-beam-cap      ( 32  32)
    visible-line        ( 32   1) 
    glow                ( 32  32)
    selection-indicator (128 128)
    rocket              ( 64  64)
    full-health-element ( 32  32)
    empty-health-element( 32  32)
    cursor              ( 64  64)))

(defparameter *player-textures*
  '(ground-unit air-unit weapon bullet explosion laser-beam laser-beam-cap glow selection-indicator rocket cursor factory))

(defparameter *world-textures*
  '(visible-line glow selection-indicator full-health-element empty-health-element)) ;later: walls, gui stuff



        
(defun generate-textures ()
  (format t "generating textures:~%")
  (setf *textures* (make-hash-table))
  (dolist (player *players*)
    (format t "player ~a: " (name player))
    (dolist (texture *player-textures*)
      (destructuring-bind (height width) (getf *texture-sizes* texture)
        (format t "~a..." texture)
        (setf (gethash texture (textures player)) (make-texture texture width height (color player)))))
    (format t "~%"))
  (force-output )
  (format t "world: ")
  
  (dolist (texture *world-textures*)
      (destructuring-bind (height width) (getf *texture-sizes* texture)
          (format t "~a..." texture)
          (setf (gethash texture *textures*) (make-texture texture width height '(1 1 1 1)))))
  (format t "~%fonts: ")
  (make-chars *char-width* *char-height* '(0.5 0.5 1 1))
  (force-output )
  ;(format t "~% initializing wobble shader ~%")
  ;(force-output )
  ;(init-shaders)
  (format t "~%done~%")
  (force-output ))