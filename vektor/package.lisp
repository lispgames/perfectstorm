(defpackage :vektor
  (:use :cl :asdf)
  (:export
   :make-vektor :make-point :print-object :make-matrix :vektor=
   :data
   :composition-by-element
   :add :subtract :scale :dot-product :normalize :convex-combination :affine-combination :centroid :update
   :norm :norm-squared :absolute :absolute-squared
   :distance :distance-squared :project
   :multiply :det :make-rot-matrix-3d
   :atan2 :rotate-z
   :cross-product :crop
   :*angle-mode*
   #:vektor #:vektor-3d #:matrix
   :x :y :z :w
   :addf :subtractf :scalef :normalizef))