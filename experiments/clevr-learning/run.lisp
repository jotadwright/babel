
(ql:quickload :clevr-learning)
(in-package :clevr-learning)

;; TRACE FCG
(activate-monitor trace-fcg)
;(deactivate-monitor trace-fcg)

;; TRACE IRL
(activate-monitor trace-irl)
(activate-monitor trace-irl-verbose)
;(deactivate-monitor trace-irl)
;(deactivate-monitor trace-irl-verbose)

;; TRACE INTERACTIONS
(activate-monitor trace-clevr-interaction)
;(deactivate-monitor trace-clevr-interaction)

;; FOR PRINTING DOTS
(activate-monitor print-a-dot-for-each-interaction)
;(deactivate-monitor print-a-dot-for-each-interaction)

;; Run a single/a series of interaction(s)
(defparameter *val-set-configuration-li*
  (make-configuration
   :entries
   '((data-sets . ("val"))
     (available-primitives . (count! equal-integer less-than greater-than
                                     equal? exist filter get-context
                                     query relate same unique))
     (determine-interacting-agents-mode . :default)
     (who-aligns? . :learner)
     (learning-strategy . :lateral-inhibition)
     (alignment-strategy . :lateral-inhibition)
     (learner-speaks-after-interaction . 500))))

(defparameter *val-set-configuration-store-samples*
  (make-configuration
   :entries
   '((data-sets . ("val"))
     (available-primitives . (count! equal-integer less-than greater-than
                                     equal? exist filter get-context
                                     query relate same unique))
     (determine-interacting-agents-mode . :default)
     (who-aligns? . :learner)
     (learning-strategy . :keep-samples)
     (alignment-strategy . :no-alignment)
     (learner-speaks-after-interaction . 500))))

(defparameter *experiment*
  (make-instance 'holophrase-experiment
                 :configuration *val-set-configuration-store-samples*))

(run-interaction *experiment*)

(run-series *experiment* 10)

(let ((learner (find 'learner (population *experiment*) :key #'role)))
  (loop for chunk in (get-data (ontology learner) 'programs)
        do (add-element (make-html chunk))))

(deactivate-all-monitors)

;; Run batches for different configurations
(run-experiments `(
                   (test
                    ((data-sets . ("val"))
                     (available-primitives . (count! exist query get-context unique filter))
                     (determine-interacting-agents-mode . :default)
                     (who-aligns? . :learner)
                     (learning-strategy . :lateral-inhibition)
                     (alignment-strategy . :lateral-inhibition)
                     (learner-speaks-after-interaction . 500)))
                   )
                 :number-of-interactions 1000
                 :number-of-series 1
                 :monitors '("export-communicative-success"
                             "export-lexicon-size"
                             "export-ontology-size"
                             ;"export-meanings-per-form"
                             ;"export-forms-per-meaning"
                             ;"export-program-correctness"
                             "export-avg-cxn-score"
                             ;"expand-percentage-of-unique-questions-learned"
                             ;"export-lexicon-change"
                             ;"export-ontology-change"
                             ))

;;; single strategy
(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("communicative-success"
                  "lexicon-size"
                  "ontology-size"
                  "avg-cxn-score")
 :y-axis '(1 2 2 1)
 :y1-max 1
 :xlabel "# Games"
 :y1-label "Success/Score"
 :y2-label "Size")

(create-graph-for-single-strategy
 :experiment-name "test"
 :measure-names '("meanings-per-form")
 :y-axis '(1)
 :xlabel "# Games"
 :y1-label "Meaning/Form")

;;; comparing strategies
(create-graph-comparing-strategies
 :experiment-names '("100k-learner-never-speaks"
                     "100k-default-lateral-inhibition")
 :measure-name "unique-question-percentage"
 :y-max 1
 :xlabel "#Games"
 :y1-label "unique questions learned"
 :captions '("learner-never-speaks" "lateral-inhibition"))
