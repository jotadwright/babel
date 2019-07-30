;;;; /package.lisp

(in-package :cl-user)

(defpackage :robot-origins-of-syntax
  (:use :common-lisp
        :utils
        :experiment-framework
        :test-framework
        :monitors
        :plot-raw-data
        :tasks-and-processes
        :meta-layer-learning
        #+:hunchentoot-available-on-this-platform :web-interface
        :irl
        :fcg
        :nao-interface
        :robot-interface
        :scene-generator
        :type-hierarchies)
  (:nicknames :roos)
  (:documentation "Mini Talking Heads + Origins of Syntax experiment on human-robot"))