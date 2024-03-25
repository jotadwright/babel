(in-package :cl-user)

(defpackage :geoquery-lsfb
  (:documentation "Package for learning grammars from the geoquery-LSFB dataset")
  (:use :common-lisp
        :utils
        :web-interface
        :monitors
        :plot-raw-data
        :experiment-framework
        :irl
        :fcg
        :slp)
  (:import-from :cl-json
   :decode-json-from-string
   :encode-json-to-string
   :encode-json-alist-to-string))
