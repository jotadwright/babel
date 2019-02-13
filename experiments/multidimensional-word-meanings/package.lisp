(in-package :cl-user)

(defpackage :multidimensional-word-meanings
  (:documentation "How can agents learn what channels to use for what words")
  (:use :common-lisp :utils
                     :experiment-framework
                     :web-interface
                     :monitors
                     :plot-raw-data
                     :irl)
  ;; add more :import-from statements here
  (:nicknames :mwm))