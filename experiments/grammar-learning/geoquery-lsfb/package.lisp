(in-package :cl-user)

(defpackage :geoquery-lsfb
  (:documentation "Package for learning grammars from the geoquery-LSFB dataset")
  (:use :common-lisp
        :web-interface
        :monitors
        :plot-raw-data
        :experiment-framework
        :irl
        :fcg
        :xmls-system
        :au-benchmark
        :grammar-learning
        :cl-ppcre)
  (:import-from :cl-json
   :decode-json-from-string
   :encode-json-to-string
   :encode-json-alist-to-string)
  (:import-from :utils
   :babel-pathname
   :make-const
   :string-append
   :variablify
   :variable-p
   :last-elt))
