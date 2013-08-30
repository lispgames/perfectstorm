(in-package :storm)

; sybs dirty little secrets 

(defclass range ()
  ((left-bound
    :initform (error "Missing left-bound.")
    :initarg :left-bound)
   (open-to-left
    :initform nil
    :initarg :open-to-left)
   (right-bound
    :initform (error "Missing right-bound.")
    :initarg :right-bound)
   (open-to-right
    :initform nil
    :initarg :open-to-right)))

(defmethod in-range-p ((x number) (r range))
  "Checks if the range r contains x."
  (with-slots 
   (left-bound open-to-left right-bound open-to-right) r
   (and
    (if open-to-left  (> x left-bound) (>= x left-bound))
    (if open-to-right (< x right-bound) (<= x right-bound)))))
  

; (in-range-p x #R{a b})
(set-dispatch-macro-character #\# #\R
  (lambda (stream sub-character number)
    (declare (ignore sub-character number))
    (let* ((open-to-left (char= #\{ (read-char stream))) ; TODO error handling
	   (inner-expression (read stream))
	   (left-bound (first inner-expression))
	   (right-bound (second inner-expression))
	   (open-to-right (char= #\} (read-char stream)))) ; TODO error handling
      `(make-instance 'range 
		      :left-bound ,left-bound
		      :open-to-left ,open-to-left
		      :right-bound ,right-bound
		      :open-to-right ,open-to-right))))