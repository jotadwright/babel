(in-package :cl-user)

(defpackage :robot-concept-learning
  (:documentation "Grounded Concept Learning using the Nao robot")
  (:use :common-lisp
        :utils
        :web-interface
        :monitors
        :plot-raw-data
        :experiment-framework
        :irl
        :fcg
        :robot-interface
        :clevr-world)
  (:import-from :cl-json
   :decode-json-from-string
   :encode-json-to-string
   :encode-json-alist-to-string)
  (:import-from :nao-interface :nao))
