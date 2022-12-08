(ql:quickload :naming-game)
(in-package :naming-game)

(defparameter *experiment-configurations*
  '((:alignment-strategy . :lateral-inhibition)
    (:who-aligns . :both)
    (:initial-score . 0.5)
    (:li-incf . 0.1)
    (:li-decf . 0.1)
    (:determine-interacting-agents-mode . :default)))

(defparameter *experiment* (make-instance 'experiment :entries *experiment-configurations*))
(make-agents *experiment*)
(make-world *experiment*)
(run-series *experiment* 1000)


