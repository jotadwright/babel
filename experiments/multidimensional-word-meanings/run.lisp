
;(ql:quickload :multidimensional-word-meanings)

(in-package :mwm)

(defparameter *configuration*
  (make-configuration
   :entries
   '((game-mode . :tt)
     (determine-interacting-agents-mode . :default)
     (channels :x-pos :y-pos :width :height)
     (who-aligns . :none))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)
     