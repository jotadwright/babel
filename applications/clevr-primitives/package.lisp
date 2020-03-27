(in-package :cl-user)

(defpackage :clevr-primitives
  (:documentation "The IRL primitives for CLEVR")
  (:use :common-lisp :utils :irl-2 :clevr-world)
  (:shadowing-import-from :clevr-world :size))

(in-package :clevr-primitives)

(export '(*clevr-primitives*))

(def-irl-primitives clevr-primitives
  :ontology *clevr-ontology*
  :primitive-inventory *clevr-primitives*)
