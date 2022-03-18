(in-package :cl-user)

(defpackage :mwm
  (:documentation "Multidimensional Word Meaning experiment")
  (:use :common-lisp
        :utils
        :web-interface
        :monitors
        :plot-raw-data
        :experiment-framework
        :irl
        :fcg
        :clevr-world)
  (:import-from :cl-mop
                :slot-names
                :map-slots)
  (:import-from :cl-json
   :decode-json-from-string
   :encode-json-to-string
   :encode-json-alist-to-string)
  (:shadowing-import-from :fcg :size :attributes))
