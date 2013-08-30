(in-package :storm)


;;;;
;;;; unit and weapon models
;;;;

;;; TODO: make nice macro


(defmacro defentity (name superclass initial-values)
  `(defclass ,name ,(list superclass)
     ,(loop for (name form) in initial-values
            collecting (list name :initform form))))


;; weapons

(defentity ak-47 cannon
  ((cooldown-time 12)
   (initial-speed 4)
   (range 65)
   (damage-radius 5)
   (damage 15)
   (projectile-size-x 15)
   (projectile-size-y 1)
   (with-shockwave nil)
   (radius 0.8)))

(defentity wattebauschwerfer cannon
  ((cooldown-time  100)
   (initial-speed  1)
   (range  100)
   (damage-radius  80)
   (damage  200)
   (projectile-size-x 10)
   (projectile-size-y 5)
   (radius 4)))

(defentity flashlight laser-cannon
  ((range 70  )
   (damage 3  )
   (radius 2)))

(defentity bfg rocket-launcher
  ((cooldown-time 50)
   (range 70)
   (trigger-distance 2)
   (damage-radius 15)
   (damage 100)
   (radius 1)
   (projectile-size-x 3)
   (projectile-size-y 2)
   (initial-speed 0.01)
   (max-rocket-speed 0.5)
   (max-rocket-angular-velocity 3)
   (rocket-acceleration 0.05)))

;; units

(defentity killbox ground-unit
  ((max-health 1000)
   (damage 1)
   (damage-radius 15)
   (weapons '(ak-47 ak-47 ak-47))
   (radius 4)))
   ;(cost 100)))


(defentity dicke-berta ground-unit
  ((max-health 6000)
   (damage 1)
   (damage-radius 15)
   (weapons '(wattebauschwerfer wattebauschwerfer wattebauschwerfer))
   (radius 8)
   (cost 1000)))


(defentity kronleuchter ground-unit
  ((max-health 10000)
   (weapons '(flashlight flashlight flashlight flashlight flashlight))
   (radius 6)
   (cost 800)))

(defentity kronleuchter2 ground-unit
  ((max-health 50000000)
   (weapons '(flashlight flashlight flashlight))
   (radius 10)
   (cost 800)))


(defentity todesstern ground-unit
  ((max-health 50000)
   (weapons '(flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight flashlight))
   (radius 30)
   (cost 50000)))



(defentity weissichnich ground-unit
  ((max-health 4000)
   (weapons '(bfg flashlight))
   (cost 200)))

(defentity starfury air-unit
  ((max-health 2000)
   (damage 0)
   (max-speed 1)
   (max-angular-velocity 3)
   (damage-radius 30)
   (size-x 7)
   (size-y 10)
   (weapons ()); '(ak-47 ak-47 flashlight))
   (cost 200)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buildings
;;


(defentity universal-factory factory
  ((production-speed 0.2)
   (size-x 30)
   (size-y 40)))