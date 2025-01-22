(ql:quickload :naming-game)
(in-package :naming-game)

;-------------------------;
;run batches of experiment;
;----------(---------------;

(defparameter *all-words* '())


(run-batch-for-different-configurations
 :experiment-class 'naming-game-experiment
 :number-of-interactions 5000
 :number-of-series 10
 :named-configurations '((lateral-inhibition ((:alignment-strategy . :lateral-inhibition)
                                              (:who-aligns . :hearer)
                                              (:trace-every-x-interactions . 100)
                                              (:initial-score . 0.5)
                                              (:li-incf . 0.1)
                                              (:li-decf . 0.1)
                                              (:determine-interacting-agents-mode . :default)))
                         (no-alignment ((:alignment-strategy . :no-aligment)
                                        (:trace-every-x-interactions . 100)
                                        (:determine-interacting-agents-mode . :default))))
 :monitors '("export-communicative-success"
             "export-lexicon-size"
             "export-conventionality")
            ;; "display-conventionality"
            ;; "display-communicative-success"
            ;; "display-lexicon-size"
 :output-dir (babel-pathname :directory '("tutorial" "naming-game" "raw-data")))

(raw-files->evo-plot
 :raw-file-paths '(("tutorial" "naming-game" "raw-data" "lateral-inhibition" "communicative-success")
                   ("tutorial" "naming-game" "raw-data" "lateral-inhibition" "conventionality")
                   ("tutorial" "naming-game" "raw-data" "lateral-inhibition" "lexicon-size"))
  :average-windows 100
  :plot-file-name "naming-game-results-lateral-inhibition" 
  :plot-directory '("tutorial" "naming-game" "graphs")
  :captions '("Communicative success" "Conventionality" "Lexicon size")
  :y1-min 0
  :y1-max 1
  :y2-min 0
  :y2-max 75
  :x-label "# communicative interactions"
  :y1-label "communicative sucess, conventionality"
  :y2-label "lexicon size"
  :use-y-axis '(1 1 2)
  :error-bars '(:percentiles (5 95))
  :error-bar-modes '(:filled)
  :open t
  :key-location "inside")

(raw-files->evo-plot
 :raw-file-paths '(("tutorial" "naming-game" "raw-data" "no-alignment" "communicative-success")
                   ("tutorial" "naming-game" "raw-data" "no-alignment" "conventionality")
                   ("tutorial" "naming-game" "raw-data" "no-alignment" "lexicon-size"))
  :average-windows 100
  :plot-file-name "naming-game-results-no-alignment" 
  :plot-directory '("tutorial" "naming-game" "graphs")
  :captions '("Communicative success" "Conventionality" "Lexicon size")
  :y1-min 0
  :y1-max 1
  :y2-min 0
  :y2-max 75
  :x-label "# communicative interactions"
  :y1-label "communicative sucess, conventionality"
  :y2-label "lexicon size"
  :use-y-axis '(1 1 2)
  :error-bars '(:percentiles (5 95))
  :error-bar-modes '(:filled)
  :open t
  :key-location "inside")


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
