(in-package :cl-user)

(defpackage :qc
  (:documentation "Query composition")
  (:use :common-lisp
        :utils
        :web-interface
        :monitors
        :plot-raw-data
        :experiment-framework
        :test-framework
        :irl
        :fcg)
  (:import-from :cl-mop
                :slot-names
                :map-slots)
  (:import-from :cl-json
   :decode-json-from-string
   :encode-json-to-string
   :encode-json-alist-to-string)
  (:shadowing-import-from :fcg :size :attributes))
