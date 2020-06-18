(in-package :cl-user)

(defpackage :hybrid-primitives
  (:documentation "The IRL primitives for CLEVR using modular neural networks")
  (:use :common-lisp
        :utils
        :irl
        :web-interface
        :clevr-world)
  (:import-from :jonathan
   :to-json :parse)
  (:import-from :drakma
   :http-request))
