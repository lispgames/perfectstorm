(in-package :toolbox)

;;
;; a mapcar that wraps
;;

(defun mapwrap (fn list &rest lists)
  (let* ((input-lists (cons list lists))
         (length (apply #'max (mapcar #'length input-lists)))
         (lists (make-list length :initial-element ())))
    (loop for i from 0 to (- length 1)
          do (format t "~a~%" i)
          do (setf lists
                   (mapcar (lambda (list input-list)
                             (or (cdr list)
                                 (copy-list input-list)))
                           lists
                           input-lists))
          collect (apply fn (mapcar #'first lists)))))



;;
;; cycle something by a given offset
;;

(defgeneric cycle  (thing &optional i j))
(defgeneric cycle! (thing &optional i j))

(defmethod cycle ((list cons) &optional (i 1) (j 0))
  (declare (ignore j))
  (let ((i (mod i (length list))))
    (append (subseq list i) (subseq list 0 i))))



(defun atan2-inner (x y) ;named -inner because interfaces might want to have the atan2 name
  "somewhat awful but stolen, tested and working atan2"
  (if (= y 0.0)
      (if (< x 0.0)
      (+ PI (/ PI 2.0))
    (/ PI 2.0))
    (if (= x 0.0)
    (if (> y 0.0)
        0.0
      PI)
      (let ((at (atan (/ x y))))
    (if (> x 0.0)
        (if (> y 0.0)
        at
          (+ PI at))
      (if (> y 0.0)
          (+ PI PI at)
        (+ PI at)))))))


;;
;; two useful macro-writing macros from pcl:
;;

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro once-only ((&rest names) &body body) ;stolen from pcl
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))


(defun choose (list)
  (when list
    (nth (random (length list)) list)))

(defun elt-mod (sequence index)
  (elt sequence (mod index (length sequence)))) 

(defun sqr (number)
  (* number number))

;(defun crop (number max &optional (min (- max)))
;  (max (min number max) min))

(defmacro setf-crop (place max &optional (min `(- ,max)))
  `(setf ,place (crop ,place ,max ,min)))

(defmacro print-vars (&rest rest)
  `(format t ,(format nil "~{~a:~~a~^, ~}~~%" rest) ,@rest)) ;self-explanatory :)

