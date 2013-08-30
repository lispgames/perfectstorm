(in-package :storm)


(defparameter *textures* (make-hash-table))
(defparameter *shaders* ())
;;
;;scrolling/zooming is handled by opengl, no manual translations are performed. see set-projection
(defparameter *scroll* (make-point 50 50)) ;"translate" the projection center by this value
(defparameter *zoom* 50) ;distance between the screen center and left/right clipping planes in opengl distance units(?). larger value means more of the plane is visible (zoomed out). cannot be negative. top/bottom clipping planes are determined by aspect ratio.
(defparameter *aspect-ratio* 1)
(defparameter *viewport-size* (make-point 23 23))
(defparameter *coord-swap* t)
(defparameter *show-all-health-circles* nil)

(defparameter *current-shader* 0)
(defparameter *current-blend-mode* :alpha) ;:alpha and :normal are possible values

(defun gl-vertex (point)
  (gl:vertex (x point) (y point) 0))

(defun set-projection ()
  ;sets the projection matrix appropriately
  ;needs to be called when window is being resized or *zoom*/*scroll* have changed 
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let* ((x (x *scroll*))
         (y (y *scroll*))
         (left   (- x *zoom*))
         (right  (+ x *zoom*))
         (bottom (+ y (/ *zoom* *aspect-ratio*)))
         (top    (- y (/ *zoom* *aspect-ratio*))))
    (gl:ortho left right (if *coord-swap* top bottom) (if *coord-swap* bottom top) -1 1)) ;switch top and bottom to swap coordinate system
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun draw-all-entities ()
  (gl:load-identity)
  (dolist (entity *independent-entities*)
      (draw entity)))

(defun draw-gui ()
  (gl:load-identity)
  (gl:use-program 0)

  ;(gl:blend-func :src-alpha :one-minus-src-alpha)
  (dolist (thing *gui-things*)
      (draw thing))

  ;(gl:blend-func :src-alpha :one)
  (when *selection-rectangle*
    (draw  *selection-rectangle*)))

(defun bind-texture (thing identifier use-shader)
  
  (let ((texture (if (or
                      (not (owned thing))
                      (symbolp (owner thing)))
                     (gethash identifier *textures*) ;does not belong to a real player
                   (gethash identifier (textures (owner thing))))))
    (when texture
      (gl:active-texture 0)
      (gl:bind-texture :texture-2d texture)
      (when use-shader
        (gl:uniformi (gl:get-uniform-location *current-shader* "base-texture") 0)))
      
    texture))
  ;|#())


(defmacro defdrawmethod (class &rest args)
                                        ;produces a (defmethod draw ((class class)) ...)
                                        ;it executes a body and afterwards (unless :no-texture is provided) binds the appropriate texture and shader.
 `(defmethod draw ((,class ,class))
    (declare (special %texture-chosen%)) ; see corresponding :around method below.
    ,@args       ;body. (don't bother removing a possible :no-texture)
    (let* ((shader-name (shader ,class))
           (shader (or (and shader-name
                            (getf *shaders* (shader ,class)))
                       0)))
      ,(when (or (not args)
                 (not (eq (first args) :no-texture)))
             `(when (and (not %texture-chosen%) ;if a more specific texture has been found, don't overwrite it
                     (drawable ,class)
                     (bind-texture ,class ',class shader-name))
               (setf %texture-chosen% t)))
      (unless *disable-shaders*
        (when (not (= *current-shader* (or shader 0)))
          (gl:use-program shader)
          (setf *current-shader* shader)))
      (when (drawable ,class)
        (call-next-method)))))

(defun set-blend-mode (mode)
  (unless (eq mode *current-blend-mode*)
    (case mode
      (:alpha (gl:blend-func :src-alpha :one))
      (:normal (gl:blend-func :src-alpha :src-color))
      (otherwise (error "unknown blend mode")))
    (setf *current-blend-mode* mode)))


(defgeneric draw (thing))

(defmethod draw :around ((thing visible-thing))
  (unless (invisible thing)
    (let ((%texture-chosen% nil))
      (declare (special %texture-chosen%)) ;keeps track of wheather a texture has been set or not. allows for fallback textures
      (let ((translation (if (drawn-relative-p thing)
                             (relative-pos thing)
                             (pos thing))))
        (gl:with-pushed-matrix
          (if (drawn-relative-p thing)
              (gl:rotate (+ (rot (parent thing)) 90)  0 0 1) ; TODO: why do we have to add 90 here? wtf?
              (gl:load-identity))
          (gl:translate (x translation)
                        (y translation)
                        (layer thing))
          (set-blend-mode (blend-mode thing))
          (call-next-method) ;an appropriate translation matrix is present on top of the stack.
          (dolist (child (children thing)) ;draw sub-entities such as weapons or glows or stuff
            (draw child)))))))

(defmethod draw :around ((entity entity))
  (call-next-method)
  (when (selectable entity)
    (when (selected entity)  ;TODO: destructability is assumed blindly
      (draw (selected entity))
      (draw-line-strip (cons (pos entity)
                             (waypoints (group entity)))
                       3
                       :color '(0 0 1 0.8)
                       :bind-texture t))
      ;(mapcar #'draw (waypoint-lines (group entity))))
    (when (or (selected entity)
              *show-all-health-circles*)
      (draw-health-circle (pos entity)
                          (* (max-size entity)
                             (health-circle-radius entity))
                          (/ (health entity) (max-health entity))))))


(defparameter *quad-dl* 1337)


(defun make-quad-dl () ;todo: fix mirroring.
  (setf *quad-dl*
        (gl:gen-lists 1))
  (gl:with-new-list (*quad-dl* :compile-and-execute)
    (gl:with-primitives :quads 
      (gl:tex-coord 0  0)
      (gl:Vertex    -0.5 0.5 0)
    
      (gl:tex-coord 1  0)
      (gl:Vertex   0.5 0.5 0)
      
      (gl:tex-coord 1  1)
      (gl:Vertex   0.5 -0.5 0)
      
      (gl:tex-coord 0  1)
      (gl:Vertex   -0.5 -0.5 0))))

(defun draw-quad ()
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (gl:call-list *quad-dl*))

(defmethod draw :before ((thing visible-thing))
  (gl:color 1 1 1 (* (alpha thing) (base-alpha thing))))

(defmethod draw ((thing visible-thing))
  (declare (special %texture-chosen%))
  (unless %texture-chosen%
    (gl:bind-texture :texture-2d (aref *char-textures* (char-code #\?))))
 ; (print-vars entity)
  (gl:with-pushed-matrix 
   ; (gl:scale (size-x thing) (size-y thing) 1)
    (gl:rotate (rot thing) 0 0 1)
    (gl:scale (size-x thing) (size-y thing) 1)
    (draw-quad)))

(defmethod draw ((rectangle rectangle)) ;todo: no copypaste :)
  (gl:with-pushed-matrix 
    (gl:rotate (rot rectangle) 0 0 1)
    (gl:scale (size-x rectangle) (size-y rectangle) 1)
    (destructuring-bind (r g b a) (color rectangle)
      (gl:color r g b (* a (alpha rectangle) (base-alpha rectangle)))
    (draw-quad))))

(defmacro make-defdrawmethods (drawable-things)
    (let ((definitions (loop for item in drawable-things
                             collecting `(defdrawmethod ,item))))
      `(eval-when (:compile-toplevel :load-toplevel)
         ,@definitions)))

(make-defdrawmethods (weapon air-unit ground-unit explosion bullet laser-beam rocket cupes-polygon selection-indicator factory))

(defdrawmethod visible-line
    (apply #'gl:color (color visible-line)))

(defdrawmethod glow
  (let ((factor (+ 0.2 (* 0.8 (alpha glow)))))
    (gl:scale factor factor 1))) ;make glow smaller when its alpha value is low

(defdrawmethod text :no-texture
  (gl:with-pushed-matrix
    (gl:scale (* 0.005 5 *zoom*) (* 0.005 10 *zoom*) 1) ;TODO: hackhackhack
    (gl:push-matrix)
    (gl:color 1 1 1 (* (alpha text) (base-alpha text))) 
    (loop for char across (text text)
          for index from 0 to (length (text text))
          do (let ((texture (aref *char-textures* (char-code char))))
               (if (char= char #\newline)
                   (progn
                     (gl:pop-matrix) ;get old state (beginning of line)
                     (gl:translate 0 -1 0) ;move one line down
                     (gl:push-matrix))  ;start again
                   (if (not (= texture 0))
                       (progn
                         (gl:bind-texture :texture-2d texture)
                         (draw-quad)
                         (when (= index (cursor-pos text))
                           (gl:bind-texture :texture-2d 0)
                           (gl:color 0.5 0.5 1 (+ 0.4 (* (sin (/ *gl-ticks* 10)) 0.1)))
                           (draw-quad)
                           (gl:color 1 1 1 1))
                         (gl:translate 1 0 0))))))
    (gl:pop-matrix)))

(defdrawmethod cursor
    (gl:scale (* 0.003 *zoom*) (* 0.003 *zoom*) 1)) ;TODO: hackhackhack

(defun draw-line-strip (points width &key (closed nil) (color nil) (bind-texture t))
  (when bind-texture
    (gl:bind-texture :texture-2d (gethash 'visible-line *textures*)))
  (when color
    (apply #'gl:color color))
  (when (< 1 (length points))
    (flet ((draw-pair (base-point offset)
              (gl:tex-coord 0 0)
              (gl-vertex (add base-point offset))
              (gl:tex-coord 0 1)
              (gl-vertex (subtract base-point offset))))
  (let ((first (first points))
        (last (and closed (first (last points)))) ; the last point will only be needed twice when the line strip is :closed t
        (prev nil)
        (current nil)
        (first-u nil)
        (next nil))
    (gl:with-primitives :quad-strip
    (loop for i from 0 below (+ 1 (length points))
          do (progn
               (setf prev current
                     current next
                     next (and points (pop points)));this pop is non-destructive
               (when current
                 (let ((u (cond ((or closed
                                     (and prev next))
                                 (let* ((prev (or prev last))
                                        (next (or next first))
                                        (d0 (subtract prev current))
                                        (d2 (subtract next current))
                                        (denominator (- (* (x d0) (y d2))
                                                        (* (y d0) (x d2)))))
                                   (if (= 0 denominator)
                                       (let ((v (normalize d2 width)))
                                         (make-point (y v) (- (x v))))
                                     (scale (add (scale d0 (absolute d2))
                                                 (scale d2 (absolute d0)))
                                            (/ width denominator)))))
                                (t
                                 (let* ((neighbour (or prev next))
                                        (v (normalize (subtract neighbour current) (* (if prev -1 1) width))))
                                   (make-point (y v) (- (x v))))))))
                   (when (< (* 4 width) (absolute u))    ;for very acute angles, u can become very large. as in this case the line strip
                     (setf u (normalize u (* 4 width)))) ;can't be drawn nicely anyway, we simply crop u.
                   (unless first-u (setf first-u u))
                   (draw-pair current u)))))
    (when closed
      (draw-pair first first-u))))))) ;close the polygon by going back to the start


(defparameter *num-health-elements* 12)



(defun draw-health-circle (pos radius health)
  (gl:use-program 0) ; is this necessary?
  (flet ((draw-circle-element (radius alpha)
           (gl:with-pushed-matrix 
             (gl:translate (* radius 0.5) 0 0)
             (gl:scale 2
                       (max 1 (* radius 0.2))
                       1)
             (gl:color 1 1 1 alpha)
             (draw-quad))))
    (gl:with-pushed-matrix
      (gl:translate (x pos) (y pos) 0)

      (gl:bind-texture :texture-2d (gethash 'full-health-element *textures*))
      (let ((alpha 1))
        (dotimes (element-index *num-health-elements*)
          (when (and (< element-index (* health *num-health-elements*)) ;health-element ist not completely full
                     (> (+ element-index 1) (* health *num-health-elements*)))
            (setf alpha
                  (- (* health *num-health-elements*) element-index))
            (gl:with-pushed-matrix
              (gl:rotate (/ 360 *num-health-elements*) 0 0 1)
              (draw-circle-element radius alpha))
            (gl:bind-texture :texture-2d (gethash 'empty-health-element *textures*))
            (setf alpha (- 1 alpha)))
          (gl:rotate (/ 360 *num-health-elements*) 0 0 1)
          (draw-circle-element radius alpha)
          (setf alpha 1))))))


(defun read-file (filename)
  (with-open-file (stream filename)
    (let ((strings ()))
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (setf strings
              (cons (concatenate 'string line (string #\nul)) strings)))
      (reverse strings))))


(defparameter *shader-files*
  '((:laser-beam    "dummy.vert"   "laser.frag"  )
    (:glow          "dummy.vert"   "glow.frag"   )))

(defun init-shaders ()
  (dolist (shader-spec *shader-files*)
    
    (bind:bind ((vertex-shader (gl:create-shader :vertex-shader))
           (fragment-shader (gl:create-shader :fragment-shader))
           (program (gl:create-program))
           ((name vertex-path fragment-path) shader-spec))
           
      
      (gl:shader-source vertex-shader (read-file vertex-path))
      (gl:shader-source fragment-shader (read-file fragment-path))

      (gl:compile-shader vertex-shader)
      (gl:compile-shader fragment-shader)

                                        ;(format t "~a ~%" (gl:get-shader-info-log vertex-shader))

                                        ;(format t "~a ~%" (gl:get-shader-info-log fragment-shader))
    
      (gl:attach-shader program vertex-shader)
      (gl:attach-shader program fragment-shader)
   
      (gl:link-program program)

      (format t "~a~%" (gl:get-program-info-log program))
                                        ;(gl:use-program program)
      (setf (getf *shaders* name) program))))
   

    
