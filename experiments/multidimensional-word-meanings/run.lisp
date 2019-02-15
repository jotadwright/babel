
;(ql:quickload :multidimensional-word-meanings)

(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)

(defparameter *configuration*
  (make-configuration
   :entries
   '((game-mode . :tt)
     (determine-interacting-agents-mode . :default)
     (conceptualisation-strategy . :discrimination)
     (who-aligns . :none))))

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)
     