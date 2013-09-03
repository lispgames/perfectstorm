(defpackage :vektor-asd
  (:use :cl :asdf))

(in-package :vektor-asd)

(defsystem vektor
  :name "vektor"
  :depends-on ()
  :components
  ((:module "vektor"
            :components
            ((:file "package")
             (:file "vektor" :depends-on ("package") )))))
