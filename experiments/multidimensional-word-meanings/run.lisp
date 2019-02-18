
;(ql:quickload :multidimensional-word-meanings)

(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)

(defparameter *TT-configuration*
  (make-configuration
   :entries
   '((game-mode . :tt)
     (determine-interacting-agents-mode . :default)
     (conceptualisation-strategy . :discrimination)
     (who-aligns . :none))))

(defparameter *P-configuration*
  (make-configuration
   :entries
   '((game-mode . :p)
     (determine-interacting-agents-mode . :default)
     (conceptualisation-strategy . :discrimination)
     (who-aligns . :both)
     (population-size . 2))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *P-configuration*))

(run-interaction *experiment*)
     