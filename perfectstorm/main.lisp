(in-package :storm)

(defparameter *grid-size* 10)
(defparameter *map-size-x* 600)
(defparameter *map-size-y* 600)

(defparameter *think-counter* 0)
(defparameter *fps-counter* 0)
(defparameter *last-fps-time* 1)

(defparameter *fps-average* 0)
(defparameter *gl-ticks* 0)


(defparameter *ticks-per-think* 10)
(defparameter *max-ips* 60) ; ips = idles per second
(defparameter *last-idle* 0)
(defparameter *ticks* 0) ; global tick counter
(defparameter *zoom-factor* 1.2) ;multiply zoom by this value for each zoom level
(defparameter *desired-zoom* 50)
(defparameter *scroll-speed* 0.05) ;will be multiplied by *zoom* to obtain actual offset

(defparameter *mouse-pos* (make-point 500 400))

(defparameter *lmb-pressed* nil)
(defparameter *drag-start* nil)
(defparameter *show-console* nil)

(defparameter *init-done* nil)

(defparameter *gui-owner* nil) ;holds the player who controls the gui

(defparameter *selected-entities* ())
(defparameter *selection-rectangle* ())

(defparameter *fps-display* nil)
(defparameter *info-display* nil)
(defparameter *console* nil)

(defparameter *stopped* nil)
(defparameter *textures-done* nil)

(defparameter *disable-shaders* t)

(defparameter *quadtree* nil)
(defparameter *game* nil)

;(defun mouse->gl (mouse-coords)
;  (multiple-value-bind (gl-x gl-y)
;      (glu:un-project (x mouse-coords) (y mouse-coords) 1)
;    (make-point gl-x (- (* 2 (y *scroll*)) gl-y)))) ;urgs.


(defun mouse->gl (mouse-coords)
  (let ((screen-coords (add (scale *viewport-size* -0.5)
                            mouse-coords)))
  (add *scroll*
       (make-point
         (* 2 *zoom* (x screen-coords) (/ (x *viewport-size*)))
         (* 2 -1 *zoom* (y screen-coords) (/ (x *viewport-size*)))))))


(defclass storm-window (glut:window)
  ()
  (:default-initargs
   :pos-x 100 :pos-y 100 :width 1000 :height 800
   
   :mode '(:double :rgb) :title "woohooo \\o/"))

(defmethod glut:display-window :after ((w storm-window))
  (gl:clear-color 0 0 0 1)
  (gl:shade-model :smooth)
  (gl:disable :lighting)
  (gl:enable :depth-test)
  (gl:hint :perspective-correction-hint :nicest)
  (gl:enable :blend :texture-2d)
  (gl:enable :fragment-shader)
  (gl:enable :vertex-shader)
  ;(gl:enable :fragment-program-arb)
  ;(gl:enable :vertex-program-arb)
  (gl:blend-func :src-alpha :one) ;additive blending for nice glowing stuff
  (gl:enable :color-material)

  ;(glut:set-cursor :cursor-none)

  (gl:color-material :front-and-back :ambient-and-diffuse)
  (gl:enable :light0)
  (gl:light :light0 :position '(-5 -5 5 10))
  (make-quad-dl)

 ; (init-shaders)
 ; (format t "shaders initialized.")
 ; (gl:blend-func :src-alpha :src-color)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (glut:swap-buffers)
  (glut:post-redisplay))
;  (generate-textures))



(defmethod glut:reshape ((w storm-window) width height)
  (gl:viewport 0 0 width height)
  (setf *aspect-ratio* (/ width height)
	*viewport-size* (make-point width height))
  (set-projection))
  

(defmethod glut:display ((w storm-window))
 ; (format t ".")
  (incf *gl-ticks*)
  (unless *disable-shaders*
    (dolist (shader (remove-if-not #'integerp *shaders*))
      (gl:use-program shader)
      (gl:uniformi (gl:get-uniform-location shader "TIME_FROM_INIT") *gl-ticks*)
      (gl:use-program *current-shader*)))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (when *textures-done*
    (setf (text *fps-display*) ;polygon count does not yet count sub-elements of visible things
          (format nil "FPS:~1$" *fps-average*)); (+ (length *entities*) (length (text *fps-display*)) (if *selection-rectangle* 8 0))))
    (draw-all-entities)
    (draw-gui)
    (incf *fps-counter*))
  (glut:swap-buffers))

  
(defmethod glut:idle ((w storm-window))
                                        ;(gl:use-program *program*)
                                        ;TODO
                                        ;(gl:uniformi (gl:get-uniform-location *program* "TIME_FROM_INIT") *gl-ticks*);(coerce (+ 0.5 (* 0.5 (sin (/ *ticks* 10)))) 'double-float))
                                        ;(gl:use-program 0)

                                        ; (dolist (shader (remove-if-not #'integerp *shaders*))
                                        ;   (gl:use-program shader)
                                        ;   (gl:uniformi (gl:get-uniform-location shader "TIME_FROM_INIT") *gl-ticks*)
                                        ;   (gl:use-program *current-shader*))
  

  (when (not *textures-done*)
    (generate-textures)
    (setf *textures-done* t))
  (when (not *init-done*)
    (init)
    (setf *init-done* t))

  
  (let ((now (get-internal-real-time)))
    (when (> now
             (+ *last-idle*
                (/ 1000 *max-ips*)))
      (setf *last-idle* (get-internal-real-time))
      (update-zoom)

      (when (not *stopped*)
        (incf *think-counter*)
        (incf *ticks*)
        (repel-units *ground-units*)    ;crude collision avoidance
        (dolist (indy *independent-entities*)
          (unless (invalid indy)
            (tick indy)))
        (dolist (group *groups*)
          (tick group))
        (when (> *think-counter* *ticks-per-think*)
          (setf *think-counter* 0)
          (dolist (entity *thinking-entities*)
            (think entity))
          (dolist (group *groups*)
            (think group)))
                                        ; (when (> now
                                        ;          (+ last-fps-time
                                        ;             1000))
                                        ;   (setf (text *fps-display*)
                                        ;         (format nil "FPS: ~a" fps-counter))
                                        ;   (setf fps-counter 0)
                                        ;   (setf last-fps-time now)))
      
        (if (= (length *selected-entities*) 1)
            (setf (text *info-display*) (format nil "~a" (first *selected-entities*)))))
      (setf *fps-average*
            (+ (* *fps-average*
                  (max 0
                       (- 1 (/ (- now *last-fps-time*) 1000))))
               *fps-counter*))
      (setf *fps-counter* 0)
      (setf *last-fps-time* now)
      
      
      (glut:post-redisplay))))

(defparameter *before-cursor* "")
(defparameter *after-cursor* "")
(defparameter *max-buffer* 15)
(defparameter *console-prompt* "STORM> ")

(defparameter *console-buffer* ())
(defparameter *console-history* ())
;(defparameter *cursor-pos* 0)

(defun toggle-console ()
  (setf *show-console* (not *show-console*)
        (invisible *console*) (not *show-console*))
  (update-console))

(defun eval-console ()
  (let* ((newline-p nil)
         (new-output (format nil "~a" (handler-case (eval (read (make-string-input-stream (concatenate 'string *before-cursor* *after-cursor*))))
                                        ;  (error (e)
                                        ;         (format nil "~a" e))))
                             (end-of-file (eof)
                                          (setf *before-cursor*
                                                  (concatenate 'string *before-cursor* (format nil "~%") *after-cursor*)
                                                newline-p t))
                             (error (e)
                                    (format nil "~a" e))))))
    (unless newline-p
      (push (concatenate 'string *console-prompt* *before-cursor* *after-cursor*) *console-buffer*)
      (push new-output *console-buffer*)
      (setf *before-cursor* ""
            *after-cursor* ""))))

(defun console-input (key)
  (case key
    (#\Return (eval-console))
    (#\Backspace (setf *before-cursor* (subseq *before-cursor*  0 (max 0 (- (length *before-cursor*) 1)))))
    (#\` (toggle-console))
    (#\^ (toggle-console))
    (otherwise (setf *before-cursor* (concatenate 'string  *before-cursor* (string key)))))
  (update-console))

(defun bounded-subseq (sequence start &optional (end 0 end-p))
  (let* ((length (length sequence))
         (start (max 0 (min start length)))
         (end (max 0 (min end length))))
    (if end-p
      (subseq sequence start end)
      (subseq sequence start))))

(defun number-of-newlines (string)
  (loop for char across string
        count (char= #\newline char)))

(defun update-console-text ()
  (setf (text *console*) (concatenate 'string
                                      (format nil "~{~a~%~}"  (reverse *console-buffer*))
                                      (format nil "~%~a" *console-prompt*)
                                      *before-cursor*
                                      *after-cursor*
                                      " "))
  (setf (cursor-pos *console*)
        (- (length (text *console*)) (length *after-cursor*) 1))
  (text *console*))
                                      

(defun update-console ()
    (loop while (> (number-of-newlines (update-console-text)) *max-buffer*)
      do (setf *console-buffer* (butlast *console-buffer*))))
      
(defun cursor-left ()
  (setf *after-cursor* (concatenate 'string (bounded-subseq *before-cursor* (- (length *before-cursor*) 1)) *after-cursor*)
        *before-cursor* (bounded-subseq *before-cursor* 0 (- (length *before-cursor*) 1)))
  (update-console))

(defun cursor-right ()
  (setf *before-cursor* (concatenate 'string *before-cursor* (bounded-subseq *after-cursor* 0 1))
        *after-cursor* (bounded-subseq *after-cursor* 1))
  (update-console))

(defun history-up ()) ;todo
(defun history-down ())

(defun handle-key (key x y)
  (flet ((adjust-scroll (x y)
           (setf *scroll*
                 (add *scroll* (normalize (make-point x y)
                                          (* *zoom*  *scroll-speed*))))))
    (if *show-console*
        (case key
          (#\Esc (glut:destroy-current-window))
          (#\` (toggle-console))
          (#\^ (toggle-console))
          (:key-left (cursor-left))
          (:key-right (cursor-right))
          (:key-up (history-up))
          (:key-down (history-down))
          (otherwise (console-input key)))
        (case key
          (#\Esc (glut:destroy-current-window))
          (#\f (glut:full-screen))

          (#\w (adjust-scroll  0  1))
          (#\a (adjust-scroll -1  0))
          (#\s (adjust-scroll  0 -1))
          (#\d (adjust-scroll  1  0))
          (#\+ (zoom-in (make-point x y)))
          (#\- (zoom-out (make-point x y)))
          (#\h (setf *show-all-health-circles*
                     (not *show-all-health-circles*)))
          (#\v (progn
                 (setf *disable-shaders*
                       (not *disable-shaders*))
                 (if *disable-shaders*
                     (gl:disable :fragment-shader :vertex-shader)
                     (gl:enable :fragment-shader :vertex-shader))))
          (#\Space (setf *stopped*
                         (not *stopped*)))
          (#\` (toggle-console))
          (#\^ (toggle-console))
          (#\c (progn
                 (setf *coord-swap* (not *coord-swap*))
                 (print-vars *coord-swap*)))))
    (set-projection)
    (update-gui)))



(defmethod glut:special ((w storm-window) key x y)
   (handle-key key x y))
  

(defmethod glut:keyboard ((w storm-window) key x y)
  (handle-key key x y))

(defun zoom-in (pos)
  (setf *desired-zoom* (/ *desired-zoom* *zoom-factor*)))

(defun zoom-out (pos)
  (setf *desired-zoom* (* *desired-zoom* *zoom-factor*)))

(defun update-zoom () ;smooth zooming
  (let ((zoom-fault (/ (- *desired-zoom* *zoom*) *zoom*)))
    (when (> (abs zoom-fault) 0.05)
      (zoom (+ 1 (/ zoom-fault 5)) *mouse-pos*))))

(defun zoom (factor pos) ; supreme zoom commander!
  (let ((old-pos (mouse->gl pos)))
    (setf *zoom* (* *zoom* factor))
    (set-projection)
    (setf *scroll* (subtract *scroll*
                             (scale (subtract (mouse->gl pos) old-pos)
                                    (if (< factor 1)
                                        1
                                        0.5)))))
  (set-projection)
  (update-gui)
  (glut:post-redisplay))



(defun remove-selection ()
  (dolist (entity *selected-entities*)
    (setf (selected entity) nil))
  (setf (text *info-display*) ""
        *selection-rectangle* nil
        *selected-entities*   ()))

(defun left-click (pos)
  (selection-box (subtract pos (make-point 2 2)) ;TODO: make nice
                 (add      pos (make-point 2 2))))

(defun right-click (pos)
  (when *selected-entities*
    (dolist (entity *selected-entities*)
      ;TODO: make sure entity is a unit
      (leave-group entity (group entity)))
    (issue-move-order (make-instance 'group
                                    :owner *gui-owner*
                                    :units (copy-seq *selected-entities*))
                      pos)))
                                    
    


(defun selection-box (start end)
  (remove-selection)
  (let ((entities (remove-if-not (lambda (entity)
                                   (and (selectable entity)
                                        (eql (owner entity) *gui-owner*)))
                                 (grid-get-entities-in-rectangle start end))))
    (dolist (entity entities)
      (select entity))
    (setf *selected-entities* entities)
    (if (= (length entities) 1)
        (setf (text *info-display*) (format nil "~a" (first entities))))))

(defmethod glut:mouse (window button state x y)
  (let* ((mouse-coords (make-point x y))
         (gl-coords (mouse->gl mouse-coords)))
    (case button
      (:wheel-up (when (eql state :up) (zoom-in mouse-coords)))
      (:wheel-down (when (eql state :up) (zoom-out mouse-coords)))
      (:left-button (progn
                      (setf *lmb-pressed* (eql state :down))
                      (if *lmb-pressed*
                            (setf *drag-start* gl-coords)
                          (if (< (absolute (subtract gl-coords *drag-start*))
                                 5) ;a box smaller than this is no box (user single-clicked) TODO: change to mouse-coords
                              (left-click gl-coords)
                              (selection-box *drag-start* gl-coords)))))
      (:right-button (when (eql state :up)
                       (right-click gl-coords))))))


(defun update-cursor (pos)
  (setf (pos (cursor *gui-owner*)) (mouse->gl pos)))

(defmethod glut:passive-motion ((w storm-window) x y)
  (setf *mouse-pos* (make-point x y))
  (update-cursor *mouse-pos*))


(defmethod glut:motion ((w storm-window) x y) ;gets executed when mousebutton (which one? any?) is pressed and mouse is moved.
  (setf *mouse-pos* (make-point x y))
  (update-cursor *mouse-pos*)
  (let ((drag-end (mouse->gl *mouse-pos*)))
    (setf *selection-rectangle*
          (if *lmb-pressed*
              (make-instance 'cupes-polygon :nodes
                             (list *drag-start*
                                   (make-point (x *drag-start*) (y drag-end))
                                   drag-end
                                   (make-point (x drag-end) (y *drag-start*)))
                             :scale-elements (/ *zoom* 100))
              nil))))
                                                 

(defun initialize-state ()
  (setf *units* ()
        *ground-units* ()
        *entities* ()
        *independent-entities* ()
        *moving-entities* () 
        *thinking-entities* ()
        *buildings* ()
        *gui-things* ()
        *textures* (make-hash-table)
        *init-done* nil
        *groups* ()
        *players* ()
        *ticks* 0
        *gl-ticks* 0
        *zoom* 100
        *desired-zoom* 100
        *gui-owner* nil
        *selection-rectangle* nil
        *selected-entities* ()
        *scroll* (make-point 0 0)
        *textures-done* nil
        *fps-display* (make-instance 'text :text "dumdidum" :screen-pos (make-point 10 20))
        *info-display* (make-instance 'text :text "" :screen-pos (make-point 10 300))
        *console* (make-instance 'text :text "" :screen-pos (make-point 150 20))
        *show-console* nil
        *before-cursor* ""
        *after-cursor* ""
        *console-buffer* ()
        vektor:*angle-mode* :deg))


(defparameter *initial-units-syb*
  ;(unit-name count x y)
  '((kronleuchter 3   100 0  )
    (starfury     0  100 100)))

(defparameter *initial-units-cupe*
  ;(unit-name count x y)
  '((dicke-berta   2  -100 0  )
    (kronleuchter2 0 -100 0  )
    (killbox       10 -100 100)))


(defun create-initial-units ()
  (let* ((syb (make-instance 'player :name "syb" :color '(0   0.2 1  1)))
         (cupe  (make-instance 'player :name "cupe"  :color '(0.9 0.5 0  1)))
         (cupe-group (make-instance 'group :owner cupe))
         (syb-group (make-instance 'group :owner syb)))
    (grid-init)
    (setf *gui-owner* cupe)
    (flet ((create-units (specs)
             (destructuring-bind (owner group name count x y) specs
               (dotimes (i count)
                 (make-instance name
                                :owner owner
                                :pos (make-point (+ x (random 200) (- 100))
                                                 (+ y (random 200) (- 100)))
                                :group group)))))
      (loop for (owner group units) in `(,(list cupe cupe-group *initial-units-cupe*)
                                         ,(list syb  syb-group  *initial-units-syb* ))
            do (mapcar #'create-units
                       (mapcar (lambda (specs)
                                 (append (list owner group) specs))
                               units))))))
    ;(make-instance 'universal-factory :pos (make-point -100 0) :owner cupe :queue '(killbox) :loop-queue-p t :rallying-point (make-point 40 40))))

(defun create-terrain-1 ()
  (make-instance 
   'terrain
   :obstacles (list 
	       (make-instance 'obstacle
			      :margin (make-instance 'polygon :corners (list (make-point 50 30)
									     (make-point 30 50)
									     (make-point 50 70)
									     (make-point 70 50))))
	       (make-instance 'obstacle
			      :margin (make-instance 'polygon :corners (list (make-point   0 80)
									     (make-point  10 50)
									     (make-point -10 10))))
	       (make-instance 'obstacle
			      :margin (make-instance 'polygon :corners (list (make-point  -40 -40)
									     (make-point -160 30)
									     (make-point -150 -100)
									     (make-point -100 -50))))
	       (make-instance 'obstacle
			      :margin (make-instance 'polygon :corners (list (make-point  15  0)
									     (make-point 40  0)
									     (make-point 25 -30)
									     (make-point 80 -40)
									     (make-point 60 10)
									     (make-point 90 -30)
									     (make-point 100 -100)
									     (make-point 30 -80)))))))

(defun create-game-1 ()
  (make-instance 'game
		 :terrain (create-terrain-1)))

(defun storm ()
  (initialize-state)
  (setf *game* (create-game-1))
  (setf *quadtree* (quadtree (terrain *game*)))
  (create-initial-units)
  (push *quadtree* *gui-things*)
 ; (push (cursor *gui-owner*) *gui-things*) ;disabling the default cursor does not work yet (-> memory faults)
  (glut:display-window (make-instance 'storm-window)))
  

(defun init () ; gets called once, when glut:idle is called the first time
  (setf *desired-zoom* 200))