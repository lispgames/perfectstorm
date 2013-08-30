(in-package :storm)

(defmacro swap (a b)
  `(let ((tmp ,a))
     (setf ,a ,b)
     (setf ,b tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A simple heap implementation.
;
; Operations:
;
; (make-heap :compare-function :initial-elements)
;    Creates a new heap object. 
;    
;    compare-function: A function that compares two elements in the following way:
;                      (compare-function a b) < 0 <=> a < b
;                      (compare-function a b) = 0 <=> a = b
;                      (compare-function a b) > 0 <=> a > b
;                      The default is #'- (fine for numbers).
;
;    initial-elements: May contain a sequence of elements that will be inserted 
;                      after the heap was created.
;
; (insert! heap element) 
;    Inserts element into heap (destructive!). O(log(n))
;
; (top heap)             
;    Returns the top-element of the heap (which is the smallest - in 
;    terms of the compare-function - element the heap contains).
;
; (delete-top! heap)     
;    Deletes the top-element from the heap.
;
; (empty? heap)
;    Is the heap empty (T if it is, NIL otherwise)?
;
; compare-function: A function that compares two elements in the following way:
;                   (compare-function a b) < 0 <=> a < b
;                   (compare-function a b) = 0 <=> a = b
;                   (compare-function a b) > 0 <=> a > b

(defparameter *heap-initial-array-length* 16)

(defclass heap ()
  ((data
    :reader data)
   (size
    :initform 0
    :reader size)
   (compare-function
    :initarg :compare-function
    :initform (error "Necessary compare-function is missing.")
    :reader compare-function)))

(defun make-heap (&key (compare-function #'-) (initial-elements ()))
  (make-instance 'heap :compare-function compare-function :initial-elements initial-elements))

; interface methods
(defgeneric insert! (container element))
(defgeneric top (sorted-container))
(defgeneric delete-top! (sorted-container))
(defgeneric empty? (container))

; "private" methods
(defgeneric delete-index! (container index))
(defgeneric sift-up! (heap index))
(defgeneric heapify! (heap index))

(defmethod initialize-instance :after ((heap heap) &key (initial-elements ()))
  (setf (slot-value heap 'data)
	(make-array *heap-initial-array-length* :adjustable t))
  (dolist (element initial-elements)
    (insert! heap element)))

(defmethod insert! ((heap heap) element)
  ; is the data array big enough?
  (when (> (+ (size heap) 1) (length (data heap)))
    ; no => heap has to grow
    (setf (slot-value heap 'data) (adjust-array (data heap) (* 2 (length (data heap))))))

  (incf (slot-value heap 'size))
  (setf (aref (data heap) (- (size heap) 1)) element)
  (sift-up! heap (- (size heap) 1)))

(defmethod top ((heap heap))
  (if (< 0 (size heap))
      (aref (data heap) 0)
    (error "Heap is empty.")))

(defmethod delete-top! ((heap heap))
  (if (< 0 (size heap))
      (delete-index! heap 0)
    (error "Heap is empty.")))

(defmethod empty? ((heap heap))
  (= (size heap) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the following methods are for "private" use only
;
; (arguments will not be checked for validity)

(defmethod delete-index! ((heap heap) index)
  ; replace element at `index` by last element
  (setf (aref (data heap) index) 
	(aref (data heap) (- (size heap) 1)))
  (decf (slot-value heap 'size))

  ; is the data array too big?
  (when (< (size heap) (/ (length (data heap)) 4))
    ; yes => heap has to shrink
    (setf (slot-value heap 'data) (adjust-array (data heap) (/ (length (data heap)) 2))))

  (heapify! heap index))

(defmethod sift-up! ((heap heap) index)
  (let ((parent-index (- (floor (/ (+ index 1) 2)) 1)))
    (when (and (>= parent-index 0)
	       (< (funcall (compare-function heap)
			   (aref (data heap) index)
			   (aref (data heap) parent-index)) 0))
      (swap (aref (data heap) index) (aref (data heap) parent-index))
      (sift-up! heap parent-index)))

  heap)

(defmethod heapify! ((heap heap) parent-index)
  (let* ((max-index parent-index)
	 (left-child-index (+ (* 2 parent-index) 1))
	 (right-child-index (+ left-child-index 1)))
    (when (and (< left-child-index (size heap))
	       (< (funcall (compare-function heap)
			   (aref (data heap) left-child-index)
			   (aref (data heap) parent-index)) 0))
      (setf max-index left-child-index))

    (when (and (< right-child-index (size heap))
	       (< (funcall (compare-function heap)
			   (aref (data heap) right-child-index)
			   (aref (data heap) max-index)) 0))
      (setf max-index right-child-index))

    (when (not (= max-index parent-index))
      (swap (aref (data heap) max-index) (aref (data heap) parent-index))
      (heapify! heap max-index)))

  heap)