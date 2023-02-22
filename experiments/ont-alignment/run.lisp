(ql:quickload :ont-alignment)
(in-package :ont-alignment)

(defparameter *experiment*
  (make-instance 'ont-alignment-experiment))

(run-interaction *experiment*)
