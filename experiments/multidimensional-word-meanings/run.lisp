
;(ql:quickload :multidimensional-word-meanings)

(in-package :mwm)

(defparameter *configuration*
  (make-configuration
   :entries
   '((game-mode . :tt)
     (determine-interacting-agents-mode . :default)
     (conceptualisation-strategy . :nearest)
     (who-aligns . :none))))

(activate-monitor trace-interaction-in-web-interface)

(defparameter *experiment*
  (make-instance 'mwm-experiment :configuration *configuration*))

(run-interaction *experiment*)
     