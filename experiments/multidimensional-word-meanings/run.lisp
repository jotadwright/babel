
(ql:quickload :mwm)
(in-package :mwm)

(activate-monitor trace-interaction-in-web-interface)

(defparameter *experiment*
  (make-instance 'mwm-experiment))

(run-interaction *experiment*)
