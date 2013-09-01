(defpackage :storm-asd
  (:use :cl :asdf))

(in-package :storm-asd)

(defsystem perfectstorm
  :name "perfectstorm"
  :depends-on (cl-opengl
               cl-glut
               cl-glu
               cl-cairo2
               infix
               metabang-bind
               toolbox
               vektor)
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
	       (:file "heap" :depends-on ("package"))
               (:file "main" :depends-on ("geometry" "opengl" "cairo" "gui" "entities-impl" "package" "util" "units" "pathfinding" "quadtree"))
               (:file "weapons" :depends-on ("entities-classes" "package" "geometry" "util"))
               (:file "ticks" :depends-on ("entities-classes" "package" "geometry" "util" "buildings"))
               (:file "thinking" :depends-on ("entities-classes" "package" "geometry" "util"))
               (:file "grid" :depends-on ("entities-classes" "package" "geometry" "util"))
               (:file "group" :depends-on ("entities-classes" "package" "geometry" "util"))
               (:file "entities-impl" :depends-on ("geometry" "entities-classes" "package" "util"))
               (:file "units" :depends-on ("entities-impl" "weapons" "thinking" "grid" "ticks"))
               (:file "pathfinding" :depends-on ("geometry" "entities-impl" "quadtree" "heap"))
               (:file "entities-classes" :depends-on ("package"))
               (:file "geometry" :depends-on ("package" "util"))
	       (:file "quadtree" :depends-on ("geometry" "util"))
               (:file "gui" :depends-on ("package" "entities-impl" "entities-classes" "opengl" "cairo"))
               (:file "opengl" :depends-on ("package" "entities-impl" "pathfinding" "buildings"))
               (:file "cairo" :depends-on ("package" "opengl" "entities-impl" "pathfinding"))
	       (:file "game" :depends-on ("package"))
	       (:file "terrain" :depends-on ("package"))
	       (:file "buildings" :depends-on ("package" "entities-classes" "terrain"))))
