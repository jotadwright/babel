;;;; /package.lisp

(in-package :cl-user)

(defpackage :demo-wtnschp
  (:use :common-lisp
        :utils
        :experiment-framework
        :test-framework
        :monitors
        :plot-raw-data
        :tasks-and-processes
        :meta-layer-learning
        :irl
        :fcg
        #+:hunchentoot-available-on-this-platform :web-interface
        :nao-interface
        :robot-interface
        :printer-interface)
  (:documentation "Demo with the Nao robot for Science Festival"))