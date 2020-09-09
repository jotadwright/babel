(in-package :cl-user)

(defpackage :hybrid-evaluation
  (:documentation "Evaluating the Hybrid IRL primitives on the CLEVR dataset")
  (:use :common-lisp
        :utils
        :monitors
        :web-interface
        :irl
        :clevr-world
        :clevr-grammar
        :hybrid-primitives)
  (:import-from :fcg
   :comprehend :trace-fcg)
  (:import-from :jonathan
   :to-json :parse)
  (:shadowing-import-from :fcg :size))
