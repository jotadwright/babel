(in-package :cl-user)

(defpackage :hybrid-primitives
  (:documentation "The IRL primitives for CLEVR using modular neural networks")
  (:use :common-lisp
        :utils
        :irl
        :clevr-world)
  (:import-from :cl-json
   :encode-json-alist-to-string
   :decode-json-from-string)
  (:import-from :drakma
   :http-request)
  (:import-from :parse-float
   :parse-float))
