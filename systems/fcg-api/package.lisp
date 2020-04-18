(defpackage fcg-api
  (:use :cl :utils)
  (:import-from :fcg
   :hash
   :--
   :*fcg-constructions*
   :form
   :meaning
   :meets
   :precedes
   :de-render
   :coupled-feature-structure)
  (:export
   :define-constructicon
   :define-cxn
   :comprehend
   :produce))
