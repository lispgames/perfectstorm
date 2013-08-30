
(in-package #:asdf)

(defsystem cl-cairo-tests
    :description "Cairo bindings; test package"
    :version "unknown"
    :author ""
    :licence "unknown"
    :components
    ((:file "tests/test")
     )
    :depends-on (cl-cairo))
