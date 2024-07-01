(in-package :cl-user)

(defpackage :slp
  (:documentation "Package for everything related to processing sign language forms")
  (:use :common-lisp
        :utils
        :web-interface
        :monitors
        :plot-raw-data
        :experiment-framework
        :irl
        :fcg
        :xmls-system))
