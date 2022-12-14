(ql:quickload :naming-game)
(in-package :naming-game)

(progn
  (deactivate-all-monitors)
  (activate-monitor trace-interaction)
  (activate-monitor trace-experiment)
  (activate-monitor record-communicative-success)
  (activate-monitor record-lexicon-size)
  (activate-monitor display-communicative-success)
  (activate-monitor record-lexicon-size)
  (activate-monitor export-communicative-success)
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

(defparameter *experiment*
  ;; agents and world are now set in the initialize-instance :after method
  (make-instance 'naming-game-experiment :entries *experiment-configurations*))

(run-series *experiment* 1000)

(run-batch-for-different-configurations
 :experiment-class 'naming-game-experiment
 :number-of-interactions 3000
 :number-of-series 1
 :named-configurations '((test ((:alignment-strategy . :lateral-inhibition)
                                (:who-aligns . :both)
                                ;(:record-every-x-interactions . 100)
                                (:initial-score . 0.5)
                                (:li-incf . 0.1)
                                (:li-decf . 0.1)
                                (:determine-interacting-agents-mode . :default))))
 :monitors '("display-communicative-success"
             "export-communicative-success")
 :output-dir (babel-pathname :directory '("tutorial" "naming-game" "raw-data")))


