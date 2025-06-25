(in-package :cl-user)

(defpackage :geoquery-lsfb-grammar
  (:documentation "A package for the geoquery-lsfb grammar")
  (:use
   :common-lisp
   :utils
   :web-interface
   :monitors
   :irl
   :fcg
   :slp)
  (:import-from :cl-user
   *babel-corpora*))
