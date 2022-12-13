(ql:quickload :naming-game)
(in-package :naming-game)

(progn 
  (activate-monitor trace-interaction)
  (activate-monitor trace-experiment)
  (activate-monitor record-communicative-success)
  (activate-monitor record-lexicon-size)
  )


(defparameter *all-words* '())

(defparameter *experiment-configurations*
  '((:alignment-strategy . :lateral-inhibition)
    (:who-aligns . :both)
    (:record-every-x-interactions . 100)
    (:initial-score . 0.5)
    (:li-incf . 0.1)
    (:li-decf . 0.1)
    (:determine-interacting-agents-mode . :default)))

(progn
  (defparameter *experiment* (make-instance 'experiment :entries *experiment-configurations*))
  (make-agents *experiment*)
  (make-world *experiment*))

(run-series *experiment* 3000)


