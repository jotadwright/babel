(ql:quickload :mwm)
(in-package :mwm)

(progn
(run-parallel-batch-for-different-configurations 
 :asdf-system "mwm"
 :package "mwm"
 :experiment-class "mwm-experiment"
 :number-of-interactions 5000
 :number-of-series 10
 :monitors (list "export-communicative-success"
                 "export-lexicon-size"
                 "export-communicative-success-given-conceptualisation"
                 "export-learner-concepts-to-pdf"
                 "export-learner-concepts-to-store"
                 ;"export-experiment-configurations"
                 )
 ;; default configuration settings
 :shared-configuration '((:initial-certainty . 0.5)
                         (:certainty-incf . 0.1)
                         (:certainty-decf . -0.1)
                         (:remove-on-lower-bound . nil)
                         (:lexical-variation . nil)
                         (:concept-history-length . 100))
 ;; configurations
 :configurations '(
                   (baseline-simulated
                    ((:experiment-type . :baseline)
                     (:world-type . :simulated)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:alignment-filter . :all)))
                   (baseline-simulated-bidirectional
                    ((:experiment-type . :baseline)
                     (:world-type . :simulated)
                     (:determine-interacting-agents-mode . :default)
                     (:alignment-filter . :all)))
                   (baseline-extracted
                    ((:experiment-type . :baseline)
                     (:world-type . :extracted)
                     (:determine-interacting-agents-mode . :tutor-speaks)
                     (:alignment-filter . :all)))
                   (baseline-extracted-bidirectional
                    ((:experiment-type . :baseline)
                     (:world-type . :extracted)
                     (:determine-interacting-agents-mode . :default)
                     (:alignment-filter . :all)))
                   )
 ;; output directory
 :output-dir (babel-pathname :directory '("experiments" "multidimensional-word-meanings" "raw-data")))

(create-graph-for-single-strategy
 "baseline-simulated"
 '("communicative-success" "lexicon-size")
 :plot-file-name "baseline-simulated"
 :average-windows '(100 1)
 :use-y-axis '(1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "concept repertoire size")
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

(create-graph-for-single-strategy
 "baseline-simulated-bidirectional"
 '("communicative-success"
   "communicative-success-given-conceptualisation"
   "lexicon-size")
 :plot-file-name "baseline-simulated-bidirectional"
 :average-windows '(100 100 1)
 :use-y-axis '(1 1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "communicative success given conceptualisation"
             "concept repertoire size")
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

(create-graph-mixing-strategies
 '(("baseline-simulated" . "communicative-success")
   ("baseline-simulated-bidirectional" . "communicative-success")
   ("baseline-simulated-bidirectional" . "communicative-success-given-conceptualisation")
   ("baseline-simulated-bidirectional" . "lexicon-size"))
 :plot-file-name "baseline-simulated-comparison"
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success (always listener)"
             "communicative success (both roles)"
             "communicative success given conceptualisation (both roles)"
             "concept repertoire size")
 :average-windows '(100 100 100 1)
 :use-y-axis '(1 1 1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :end 5000
 :fsize 12 
 :open nil)

(create-graph-for-single-strategy
 "baseline-extracted"
 '("communicative-success"
   "lexicon-size")
 :plot-file-name "baseline-extracted"
 :average-windows '(100 1)
 :use-y-axis '(1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :xwlabel "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "concept repertoire size")
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

(create-graph-for-single-strategy
 "baseline-extracted-bidirectional"
 '("communicative-success"
   "communicative-success-given-conceptualisation"
   "lexicon-size")
 :plot-file-name "baseline-extracted-bidirectional"
 :average-windows '(100 100 1)
 :use-y-axis '(1 1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success"
             "communicative success given conceptualisation"
             "concept repertoire size")
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :fsize 12
 :open nil)

(create-graph-mixing-strategies
 '(("baseline-extracted" . "communicative-success")
   ("baseline-extracted-bidirectional" . "communicative-success")
   ("baseline-extracted-bidirectional" . "communicative-success-given-conceptualisation")
   ("baseline-extracted-bidirectional" . "lexicon-size"))
 :plot-file-name "baseline-extracted-comparison"
 :x-label "Number of Games"
 :y1-label "Communicative Success"
 :y2-label "Number of Concepts"
 :captions '("communicative success (always listener)"
             "communicative success (both roles)"
             "communicative success given conceptualisation (both roles)"
             "concept repertoire size")
 :average-windows '(100 100 100 1)
 :use-y-axis '(1 1 1 2)
 :y1-min 0 :y1-max 1
 :y2-min 0 :y2-max 30
 :error-bars '(:percentile 5 95)
 :error-bar-modes '(:lines)
 :key-location "bottom"
 :end 5000
 :fsize 12
 :open nil)

)
