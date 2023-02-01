(in-package :cl-user)

(defpackage :ont-alignment
  (:documentation "Language games for ontology alignment experiment")
  (:use :common-lisp
        :utils
        :test-framework
        :experiment-framework
        :monitors
        :plot-raw-data
        :irl
        :fcg))
