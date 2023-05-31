(ql:quickload :pattern-finding)
(in-package :pattern-finding)

(activate-monitor trace-fcg)

(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment)))

(run-interaction *experiment*)