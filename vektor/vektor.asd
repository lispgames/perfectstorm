;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :vektor-asd
  (:use :cl :asdf))

(in-package :vektor-asd)

(defsystem vektor
  :name "vektor"
  :depends-on ()
  :components ((:file "package")
               (:file "vektor" :depends-on ("package") )))