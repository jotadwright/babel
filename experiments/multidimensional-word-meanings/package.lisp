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
        :clevr)
  (:import-from :cl-mop
                :slot-names
                :map-slots))