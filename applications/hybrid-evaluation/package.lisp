(in-package :cl-user)

(defpackage :hybrid-evaluation
  (:documentation "Evaluating the Hybrid IRL primitives on the CLEVR dataset")
  (:use :common-lisp
        :utils
        :monitors
        :irl
        :clevr-world
        :clevr-grammar
        :hybrid-primitives)
  (:import-from :fcg
   :comprehend :trace-fcg)
  (:import-from :jonathan
   :to-json :parse))
