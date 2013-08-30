;;;; http://nostdal.org/cl-cairo/ ;;;;

(defpackage :cairo-gtk
  (:use :cairo :cl :cffi)
  (:shadowing-import-from :cairo :fill))
(in-package :cairo-gtk)


(load-foreign-library "libgtk-x11-2.0.so")

;; All this nasty stuff should reside in some GTK-wrapper library.
(defctype gint :int) 
(defctype gboolean :boolean) 
(defctype gpointer :pointer) 
(defctype gconstpointer :pointer) 
(defctype gchar :char) 
(defctype gchararray :string) 
(defctype guchar :unsigned-char) 
(defctype guint :unsigned-int) 
(defctype gshort :short) 
(defctype gushort :unsigned-short)
(defctype glong :long) 
(defctype gulong :unsigned-long) 
(defctype gint8 :int8) 
(defctype guint8 :uint8)
(defctype gint16 :short) 
(defctype guint16 :unsigned-short) 
(defctype gint32 :int32) 
(defctype guint32 :uint32) 
(defctype gfloat :float) 
(defctype gdouble :double) 
(defctype gsize :unsigned-int) 
(defctype gssize :int)

(defcstruct TypeInstance
  (g-class :pointer))

(defcstruct GObject
  (g-type-instance TypeInstance)
  (ref-count guint)
  (qdata :pointer)) ;; TODO: type


(defcstruct Object
  (parent-instance GObject)
  (flags guint32))


(defcstruct GtkRequisition
  (width gint)
  (height gint))


(defcstruct GtkAllocation
  (x gint)
  (y gint)
  (width gint)
  (height gint))


(defcstruct GtkWidget
  (object Object)
  (private-flags guint16)
  (state guint8)
  (saved-state guint8)
  (name :string) ;; gchar *name;
  (style :pointer)
  (requisition GtkRequisition)
  (allocation GtkAllocation)
  (window :pointer) 
  (parent :pointer))



(defcallback drawable-expose :void ((drawable :pointer) (data :pointer))
  (declare (ignore data))
  ;; Convert drawables window-member to cairo-context.
  (let ((cairo::+cr+ (foreign-funcall "gdk_cairo_create"
                               :pointer (foreign-slot-value drawable 'GtkWidget 'window)
                               :pointer)))
    ;; Now we switch to regular Lisp without the FFI stuff and use cl-cairo.
    ;; Drawe some text.
    (set-source-rgb 1d0 0d0 0d0)
    (select-font-face "sans" :normal :bold)
    (scale 5d0 5d0)
    (move-to 10d0 10d0)
    (show-text "Hello Bill Winkler!")
    (stroke)

    ;; Draw an arc.
    (arc 10d0 10d0 10d0 0d0 90d0)
    (stroke)
    
    (destroy)))



(defun test ()
  ;; Initiate GTK.
  (foreign-funcall "gtk_init" :pointer (null-pointer) :pointer (null-pointer)
                   :void)
  
  ;; Create GTK window and drawable widget.
  (let ((window (foreign-funcall "gtk_window_new" :int 0 :pointer))
        (drawable (foreign-funcall "gtk_drawing_area_new" :pointer)))
    ;; Connect expose-event to function that will draw.
    (foreign-funcall "g_signal_connect_data"
                     :pointer drawable
                     :string "expose-event"
                     :pointer (get-callback 'drawable-expose)
                     :pointer (null-pointer)
                     :pointer (null-pointer)
                     :int 0)
    ;; Setup and start.
    (foreign-funcall "gtk_container_add" :pointer window :pointer drawable :void)
    (foreign-funcall "gtk_widget_show_all" :pointer window :void)
    (foreign-funcall "gtk_main" :void)))
(export 'test)


