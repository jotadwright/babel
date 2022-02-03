(ql:quickload :mwm)
(in-package :mwm)

(run-parallel-batch-for-different-configurations 
 :asdf-system "mwm"
 :package "mwm"
 :experiment-class "mwm-experiment"
 :number-of-interactions 2500
 :number-of-series 3
 :monitors (list "export-communicative-success"
                 "export-lexicon-size"
                 "export-communicative-success-given-conceptualisation"
                 "export-learner-concepts")
 ;; default configuration settings
 :shared-configuration nil
 ;; configurations
 :configurations '(
                   (baseline-simulated
                    ((:experiment-type . :baseline)
                     (:world-type . :simulated)
                     (:determine-interacting-agents-mode . :tutor-speaks)))
                   (baseline-simulated-bidirectional
                    ((:experiment-type . :baseline)
                     (:world-type . :simulated)
                     (:determine-interacting-agents-mode . :default)))
                   (baseline-extracted
                    ((:experiment-type . :baseline)
                     (:world-type . :extracted)
                     (:determine-interacting-agents-mode . :tutor-speaks)))
                   (baseline-extracted-bidirectional
                    ((:experiment-type . :baseline)
                     (:world-type . :extracted)
                     (:determine-interacting-agents-mode . :default)))
                   )
 ;; output directory
 :output-dir (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "raw-data")))

(create-graph-for-single-strategy
 :experiment-name "baseline-simulated"
 :measure-names '("communicative-success"
                  ;"communicative-success-given-conceptualisation"
                  "lexicon-size")
 :plot-file-name "baseline-simulated"
 :average-windows '(100 1)
 :y-axis '(1 2)
 :y1-min 0 :y1-max 1
 :y2-max 30
 :xlabel "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "concept repertoire size")
 :open nil)

(create-graph-for-single-strategy
 :experiment-name "baseline-simulated-bidirectional"
 :measure-names '("communicative-success"
                  "communicative-success-given-conceptualisation"
                  "lexicon-size")
 :plot-file-name "baseline-simulated-bidirectional"
 :average-windows '(100 100 1)
 :y-axis '(1 1 2)
 :y1-min 0 :y1-max 1
 :y2-max 30
 :xlabel "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "communicative success given conceptualisation"
             "concept repertoire size")
 :open nil)

(create-graph-mixing-strategies
 :experiment-measure-conses
 '(("baseline-simulated" . "communicative-success")
   ("baseline-simulated-bidirectional" . "communicative-success")
   ("baseline-simulated-bidirectional" . "communicative-success-given-conceptualisation"))
 :plot-file-name "baseline-simulated-comparison"
 :xlabel "Number of Games"
 :y1-label "Communicative Success"
 :captions '("communicative success"
             "communicative success (bidirectional)"
             "communicative success given conceptualisaiton (bidirectional)")
 :window 100 :y-max 1)

(create-graph-for-single-strategy
 :experiment-name "baseline-extracted"
 :measure-names '("communicative-success"
                  ;"communicative-success-given-conceptualisation"
                  "lexicon-size")
 :plot-file-name "baseline-extracted"
 :average-windows '(100 1)
 :y-axis '(1 2)
 :y1-min 0 :y1-max 1
 :y2-max 30
 :xlabel "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "concept repertoire size")
 :open nil)

(create-graph-for-single-strategy
 :experiment-name "baseline-extracted-bidirectional"
 :measure-names '("communicative-success"
                  "communicative-success-given-conceptualisation"
                  "lexicon-size")
 :plot-file-name "baseline-extracted-bidirectional"
 :average-windows '(100 100 1)
 :y-axis '(1 1 2)
 :y1-min 0 :y1-max 1
 :y2-max 30
 :xlabel "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "communicative success given conceptualisation"
             "concept repertoire size")
 :open nil)

(create-graph-mixing-strategies
 :experiment-measure-conses
 '(("baseline-extracted" . "communicative-success")
   ("baseline-extracted-bidirectional" . "communicative-success")
   ("baseline-extracted-bidirectional" . "communicative-success-given-conceptualisation"))
 :plot-file-name "baseline-extracted-comparison"
 :xlabel "Number of Games"
 :y1-label "Communicative success"
 :captions '("communicative success"
             "communicative success (bidirectional)"
             "communicative success given conceptualisaiton (bidirectional)")
 :window 100 :y-max 1)