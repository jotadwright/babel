
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
   `((data-sets . ("num_objects_4"))
     (available-primitives . (count! exist query get-context unique filter))
     (determine-interacting-agents-mode . :default)
     (who-aligns? . :learner)
     (learning-strategy . :lateral-inhibition)
     (alignment-strategy . :lateral-inhibition))))

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
  (make-instance 'holophrase-experiment :configuration *zero-hop-configuration*))

(run-interaction *experiment*)

(run-series *experiment* 100)

(deactivate-all-monitors)

;; Run batches for different configurations
(run-experiments `(
                   (test
                    ((data-sets . ("num_objects_4"))
                     (available-primitives . (count! exist query get-context unique filter))
                     (determine-interacting-agents-mode . :none)
                     (who-aligns? . :learner)
                     (learning-strategy . :lateral-inhibition)
                     (alignment-strategy . :lateral-inhibition)))
                   )
                 :number-of-interactions 500
                 :number-of-series 1
                 :monitors '("export-communicative-success"
                             "export-lexicon-size"
                             "export-ontology-size"
                             "export-meanings-per-form"
                             "export-forms-per-meaning"
                             ;"export-lexicon-change"
                             ;"export-ontology-change"
                             ;"export-memory-size"
                             ;"export-memory-entry-per-form"
                             "export-program-correctness"
                             "export-avg-cxn-score"
                             ;"export-incorrect-program-questions"
                             ;"export-form-competitors"
                             "expand-percentage-of-unique-questions-learned"
                             ))

;;; single strategy
(create-graph-for-single-strategy
 :experiment-name "learner-never-speaks"
 :measure-names '("communicative-success"
                  "lexicon-size"
                  "ontology-size"
                  "avg-cxn-score")
 :y-axis '(1 2 2 1)
 :y1-max 1
 :xlabel "# Games"
 :y1-label "Success")

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("meanings-per-form")
 :y-axis '(1)
 :xlabel "# Games"
 :y1-label "Meaning/Form")

;;; comparing strategies
(create-graph-comparing-strategies
 :experiment-names '("50k-learner-never-speaks"
                     "50k-default-lateral-inhibition")
 :measure-name "avg-cxn-score"
 :y-max nil
 :xlabel "#Games"
 :y1-label "cxn score"
 :captions '("learner-never-speaks" "lateral-inhibition"))