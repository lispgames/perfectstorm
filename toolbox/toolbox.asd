;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :toolbox-asd
  (:use :cl :asdf))

(in-package :toolbox-asd)

(defsystem toolbox
  :name "toolbox"
  :depends-on ()
  :components ((:file "package")
               (:file "toolbox" :depends-on ("package") )))
