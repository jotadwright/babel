(in-package :cl-user)

(defpackage :mwm-evaluation
  (:documentation "Integrating the multi-dimensional concepts in vqa")
  (:use :common-lisp
        :utils
        :monitors
        :plot-raw-data
        :experiment-framework
        :web-interface
        :fcg
        :irl
        :clevr-world
        :clevr-grammar
        :mwm)
  (:import-from :cl-json
   :decode-json-from-string
   :encode-json-to-string
   :encode-json-alist-to-string)
  (:import-from :cl-store :restore)
  (:shadowing-import-from :fcg :size :attributes))
  
