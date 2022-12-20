(ql:quickload :naming-game)
(in-package :naming-game)

;-------------------------;
;run batches of experiment;
;-------------------------;

(defparameter *all-words* '())

(Run-Batch-for-different-configurations
 :experiment-class 'naming-game-experiment
 :number-of-interactions 5000
 :number-of-series 1
 :named-configurations '((test ((:alignment-strategy . :lateral-inhibition)
                                (:who-aligns . :both)
                                (:trace-every-x-interactions . 100)
                                (:initial-score . 0.5)
                                (:li-incf . 0.1)
                                (:li-decf . 0.1)
                                (:determine-interacting-agents-mode . :default))))
 :monitors '("display-communicative-success"
             "export-communicative-success"
             "display-lexicon-size"
             "export-lexicon-size")
 :output-dir (babel-pathname :directory '("tutorial" "naming-game" "raw-data")))

(raw-files->evo-plot
 :raw-file-paths '(("tutorial" "naming-game" "raw-data" "test" "communicative-success")
                   ("tutorial" "naming-game" "raw-data" "test" "lexicon-size")
		   )
  :average-windows 100
  :plot-file-name "naming-game-results" 
  :plot-directory '("tutorial" "naming-game" "graphs")
  :captions '("Communicative success")
  :y1-min 0
  :y1-max 1
  :y2-min 0
  :y2-max 50
  :x-label "# Games"
  :y1-label "communicative sucess"
  :y2-label "lexicon size"
  :use-y-axis '(1 2)
  :error-bars '(:percentiles (25 75))
  :error-bar-modes '(:filled)
  :open t
  )

#| 
(deactivate-all-monitors)

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

(run-series *experiment* 5000)
|#
