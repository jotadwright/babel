(in-package :cl-user)

(defpackage :emergent-rl
  (:documentation "Language games x Reinforcement Learning experiment")
  (:use :common-lisp
        :utils
        :test-framework
        :experiment-framework
        :monitors
        :plot-raw-data
        #+hunchentoot-available-on-this-platform :web-interface))
