;;;; /package.lisp

(in-package :cl-user)

(defpackage :robot-colour-game
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
        :scene-generator)
  (:documentation "Colour Naming Game with 2 robots"))