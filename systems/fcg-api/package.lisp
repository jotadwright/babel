(defpackage fcg-api
  (:use :cl)
  (:import-from :fcg
   :hash
   :--
   :*fcg-constructions*
   :form
   :meaning
   :meets
   :precedes)
  (:export
   :define-constructicon
   :define-cxn
   :comprehend
   :produce))
