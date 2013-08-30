;;; http://nostdal.org/cl-cairo/

(in-package :cairo)


(defmacro coercelet ((vars type) &body body)
  `(let ,(mapcar (lambda (var)
                   (if (atom var)
                       `(,var (coerce ,var ',type))
                       `(,(first var) (coerce ,(second var) ',type))))
                 vars)
    ,@body))




(defmacro save-restore (&body body)
  `(progn
    (save)
    ,@body
    (restore)))
(export 'save-restore)



(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))



(defmacro with-cairo-png ((context filename format-t width height) &body body)
  `(let ((+cr+ ,context)
         (+file+ (fopen ,filename "w")))
    (unwind-protect
         (progn
           (set-target-png +cr+ +file+ ,format-t ,width ,height)
           ,@body)
      (progn
        (destroy)
        (fclose +file+)))))



(defmacro sym->str (sym)
  ;; TODO: I've only confirmed that this works correctly when *readtable-case* is :invert.
  (let ((str (gensym)))
  `(let ((,str (string ',sym)))
     (cond
       ((every #'upper-case-p (remove-if-not #'alpha-char-p ,str))
        (string-downcase ,str))
       ((every #'lower-case-p (remove-if-not #'alpha-char-p ,str))
        (string-upcase ,str))
       (t
        ,str)))))



(defun sym->strf (sym)
  (let ((str (string sym)))
     (cond
       ((every #'upper-case-p (remove-if-not #'alpha-char-p str))
        (string-downcase str))
       ((every #'lower-case-p (remove-if-not #'alpha-char-p str))
        (string-upcase str))
       (t
        str))))



(defun str->sym (str)
  "Use `',(str->sym \"blah\") to return a quoted symbol."
  ;; TODO: I've only confirmed that this works correctly when *readtable-case* is :invert.
  (read-from-string str))



(defmacro mksym (&rest from)
  (let ((res ""))
    (dolist (elt from)
      (if (typep elt 'symbol)
          (setf res (mkstr res (sym->strf elt)))
          (setf res (mkstr res elt))))
    (str->sym res)))


(defun mksymf (&rest from)
  (let ((res ""))
    (dolist (elt from)
      (if (typep elt 'symbol)
          (setf res (mkstr res (sym->strf elt)))
          (setf res (mkstr res elt))))
    (str->sym res)))


(defun group (source n)
  (when (zerop n) (error "Zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))



(defmacro defcairo (foreign-name &optional return-type &rest args)
  "Functions that take `cairo_t*' as first argument."
  (let* ((lisp-name (str->sym (subseq (substitute #\- #\_ foreign-name) 6)))
         (lisp-args (mapcar #'second (group args 2))))
    `(progn
       (defun ,lisp-name (,@lisp-args &optional (+cr+ +cr+))
         ,(if args
              `(foreign-funcall ,foreign-name
                                cairo-t +cr+
                                ,@args
                                ,(if return-type
                                     return-type
                                     :void))
              `(foreign-funcall ,foreign-name
                                cairo-t +cr+
                                ,(if return-type
                                     return-type
                                     :void))))
       (export ',lisp-name))))


(defmacro defsetget (name &optional getter setter)
  "Define set- and getter-functions for name."
  `(progn
     (defun ,name (&optional (+cr+ +cr+))
       (,(mksymf "get-" name) +cr+))
     (defun (setf ,name) (,name &optional (+cr+ +cr+))
       (,(mksymf "set-" name) ,name +cr+))
     (export ',name)))


