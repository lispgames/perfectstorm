;;;;;;;;;;;
;;;;gui stuff
;;;;
;;;;

(in-package :storm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;text handling
;;

(defun update-gui () ;to be called whenever scrolling or zooming has occured
  (dolist (thing *gui-things*)
    (update-pos thing)))
  

(defgeneric update-pos (thing))
(defmethod update-pos ((thing t))
  )
(defmethod update-pos ((thing gui-thing))
  (setf (pos thing)
        (mouse->gl (screen-pos thing))))

(defmethod (setf screen-pos) :after (new-pos (thing gui-thing))
  (update-pos thing))

(defmethod (setf text) :after (new-text (text text))
  (update-pos text))

