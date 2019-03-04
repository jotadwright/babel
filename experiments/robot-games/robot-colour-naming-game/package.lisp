(in-package :cl-user)

(defpackage :robot-colour-naming-game-experiment
  (:use :common-lisp
        :utils
        :experiment-framework
        :test-framework
        :monitors
        :plot-raw-data
        :irl
        :fcg
        #+:hunchentoot-available-on-this-platform :web-interface
        :nao-interface
        :robot-interface
        :scene-generator)
  (:documentation "Grounded Colour Naming Game with robots")
  (:nicknames :roconaga))