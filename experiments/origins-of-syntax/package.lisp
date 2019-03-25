(in-package :cl-user)

(defpackage :origins-of-syntax
  (:documentation "A package for experiments on the origins of syntax using anti-unification and type hierarchies in FCG.")
  (:use :common-lisp :utils :test-framework :irl :fcg :type-hierarchies :meta-layer-learning :experiment-framework :monitors :plot-raw-data #+:hunchentoot-available-on-this-platform :web-interface))
