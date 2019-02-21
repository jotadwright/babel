
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; TRACE FCG
(activate-monitor trace-fcg)
;(deactivate-monitor trace-fcg)

;; TRACE IRL
(activate-monitor trace-irl-in-web-browser)
;(deactivate-monitor trace-irl-in-web-browser)

;; TRACE INTERACTIONS
(activate-monitor trace-clevr-interaction)
;(deactivate-monitor trace-clevr-interaction)

;; FOR PRINTING DOTS
(activate-monitor print-a-dot-for-each-interaction)
;(deactivate-monitor print-a-dot-for-each-interaction)

;; Run a single/a series of interaction(s)
(defparameter *zero-hop-configuration*
  (make-configuration
   :entries
   `((context-size . 4)
     (available-primitives . (count! exist query get-context unique filter))
     (determine-interacting-agents-mode . :tutor-learner)
     (learning-strategy . :keep-samples)
     (alignment-strategy . :no-alignment))))

(defparameter *single-or-configuration*
  (make-configuration
   :entries
   `((context-size . 4)
     (available-primitives . (count! filter get-context relate union! unique))
     (determine-interacting-agents-mode . :tutor-learner)
     (learning-strategy . :keep-samples)
     (alignment-strategy . :no-alignment)
     (questions-file . ,(merge-pathnames
                         (make-pathname :directory '(:relative "CLEVR" "CLEVR-learning-data" "base-single-or-questions")
                                        :name "CLEVR_questions_num_objects_X_per_line_enhanced" :type "json")
                         cl-user:*babel-corpora*)))))

(defparameter *experiment*
  (make-instance 'vqa-experiment :configuration *zero-hop-configuration*))

(run-interaction *experiment*)

(run-series *experiment* 50)

(deactivate-all-monitors)

;; Run batches for different configurations
(run-experiments `(
                   (sample-strategy-learner-hears
                    ((:available-primitives . (count! exist query
                                                      get-context unique filter))
                     (:determine-interacting-agents-mode . :default)
                     (:learning-strategy . :keep-samples)
                     (:alignment-strategy . :no-meaning-competitors-punished)))
                   )
                 :number-of-interactions 100
                 :number-of-series 1
                 :monitors '("export-communicative-success"
                             ;"export-lexicon-size"
                             ;"export-ontology-size"
                             ;"export-meanings-per-form"
                             ;"export-forms-per-meaning"
                             ;"export-lexicon-change"
                             ;"export-ontology-change"
                             ;"export-memory-size"
                             ;"export-memory-entry-per-form"
                             ;"export-program-correctness"
                             ;"export-avg-cxn-score"
                             ;"export-incorrect-program-questions"
                             ;"export-form-competitors"
                             ))

;; plots for single strategy
(let* ((context-size 4)
       (experiment-name "test"))
  (create-graph-for-single-strategy
   :experiment-name experiment-name
   :measure-names '("communicative-success"
                    "program-correctness"
                    "avg-cxn-score")
   :y-axis '(1 1 1)
   :y1-max 1
   :xlabel "# Games"
   :y1-label "Success")
  (create-graph-for-single-strategy
   :experiment-name experiment-name
   :measure-names '("lexicon-size"
                    "ontology-size")
   :y-axis '(1 1)
   :xlabel "# Games"
   :y1-label "Size")
  #|
  (create-graph-for-single-strategy
   :experiment-name experiment-name
   :measure-names '("meanings-per-form"
                    "forms-per-meaning")
   :y-axis '(1)
   :xlabel "# Games"
   :y1-label "Competition")
  (create-graph-for-single-strategy
   :experiment-name experiment-name
   :measure-names '("lexicon-change"
                    "ontology-change")
   :y-axis '(1)
   :xlabel "# Games"
   :y1-label "Frequency of Change")
  (create-graph-for-single-strategy
   :experiment-name experiment-name
   :measure-names '("memory-size"
                    "memory-entry-per-form")
   :captions '("sample-buffer-size" "samples-per-form")
   :y-axis '(2 1)
   :xlabel "# Games"
   :y1-label "Samples per form"
   :y2-label "Sample buffer size")
  |#
  )
                    

;; comparing on context size
(let* ((context-sizes '(4 5 6))
       (experiment-names (loop for size in context-sizes
                               collect (format nil "ctx-~a-sample-strategy" size))))
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "communicative-success"
   :y-max 1 :y1-label "Success")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "lexicon-size"
   :y-max nil :y1-label "cxn-inventory size")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "ontology-size"
   :y-max nil :y1-label "Number of chunks")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "program-correctness"
   :y-max 1 :y1-label "Correctness")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "avg-cxn-score"
   :y-max 1 :y1-label "cxn score"))

;; comparing strategies
(let ((experiment-names '("sample-strategy-no-alignment" "sample-strategy-learner-speaks"))
      (captions '("learner-hears" "learner-speaks")))
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "communicative-success"
   :captions captions
   :y-max 1 :y1-label "Success")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "lexicon-size"
   :captions captions
   :y-max nil :y1-label "cxn-inventory size")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "ontology-size"
   :captions captions
   :y-max nil :y1-label "Number of chunks")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "program-correctness"
   :captions captions
   :y-max 1 :y1-label "Correctness")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "avg-cxn-score"
   :captions captions
   :y-max 1 :y1-label "cxn score")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "forms-per-meaning"
   :captions captions
   :y-max nil :y1-label "forms/meaning")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "meanings-per-form"
   :captions captions
   :y-max nil :y1-label "meanings/form")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "lexicon-change"
   :captions captions
   :y-max 1 :y1-label "lexicon change")
  (create-graph-comparing-strategies
   :experiment-names experiment-names
   :measure-name "ontology-change"
   :captions captions
   :y-max 1 :y1-label "ontology-change"))
  
