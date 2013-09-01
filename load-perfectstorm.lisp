;;; Run this as a script to load perfectstorm and the libraries it uses.


(push "./vektor/" asdf:*central-registry*)
(push "./toolbox/" asdf:*central-registry*)
(push "./perfectstorm/" asdf:*central-registry*)

(asdf:load-system :vektor)
(asdf:load-system :toolbox)
(asdf:load-system :perfectstorm)
