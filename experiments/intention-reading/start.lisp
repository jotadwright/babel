
(ql:quickload :intention-reading)
(in-package :intention-reading)

;;;; MONITORS
;;;; --------
(progn
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  (activate-monitor trace-tasks-and-processes))

(progn
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor display-metrics))

(deactivate-all-monitors)

(progn
  (deactivate-monitor trace-fcg)
  (deactivate-monitor trace-irl)
  (deactivate-monitor trace-interactions-in-wi))

(progn
  (deactivate-monitor display-communicative-success))

;;;; SINGLE EXPERIMENT
;;;; -----------------

(defparameter *experiment*
   (make-instance 'clevr-learning-experiment
                  :entries '((:determine-interacting-agents-mode . :tutor-learner)
                             (:question-sample-mode . :all)
                             ;(:questions-per-challenge . 1000)
                             (:scenes-per-question . 50)
                             (:confidence-threshold . 1.1)
                             (:tutor-sample-mode . :smart)
                             (:cxn-decf-score . 0.4)
                             (:cxn-inhibit-score . 0.1)
                             (:primitives . :symbolic)
                             (:learner-cxn-supplier . :hashed-and-scored)
                             (:alignment-strategy . :lateral-inhibition)
                             (:hide-type-hierarchy . nil)
                             (:remove-cxn-on-lower-bound . t)
                             (:composer-strategy . :store-past-scenes)
                             (:th-link-repair-mode-comprehension . :no-path-required)
                             (:th-link-repair-mode-formulation . :path-required))))


(clear-page)
(run-interaction *experiment*)
(run-series *experiment* 500)


;; disable meta-layer and interpretation goal test
(let ((grammar (grammar (learner *experiment*))))
  (set-configuration grammar :parse-goal-tests
                     (butlast (get-configuration grammar :parse-goal-tests)))
  (set-configuration grammar :use-meta-layer nil)
  (comprehend "What  material is the big blue object?"
              :cxn-inventory grammar)
  (set-configuration grammar :parse-goal-tests
                     (append (get-configuration grammar :parse-goal-tests)
                             '(:correct-interpretation)))
  (set-configuration grammar :use-meta-layer t))

(formulate-all '((bind color-category cat-1 red)
                 (filter set-1 set-2 cat-1)
                 (get-context set-2)
                 (count answer set-1))
               :cxn-inventory (grammar (learner *experiment*)))

(add-element (make-html (grammar (learner *experiment*))))

(add-element
 (make-html
  (categorial-network (grammar (learner *experiment*)))
  :weights? t))

(let ((lexical-cxns (find-all 'lexical (constructions (grammar (learner *experiment*)))
                              :key #'get-cxn-type)))
  (loop for cxn in (sort lexical-cxns #'> :key #'cxn-score)
        do (add-element (make-html cxn)))
  (length lexical-cxns))

(let ((item-based-cxns
       (find-all 'item-based (constructions (grammar (learner *experiment*)))
                 :key #'get-cxn-type)))
  (loop for cxn in (sort item-based-cxns #'> :key #'cxn-score)
        do (add-element (make-html cxn)))
  (length item-based-cxns))

(let ((holophrase-cxns
       (find-all 'holophrase (constructions (grammar (learner *experiment*)))
                 :key #'get-cxn-type)))
  (loop for cxn in (sort holophrase-cxns #'> :key #'cxn-score)
        do (add-element (make-html cxn)))
  (length holophrase-cxns))

      

;;;; EXPERIMENT WITH CONFIGURATIONS
;;;; ------------------------------

;; test
(run-experiments '(
                   (formulation-all-composer-solutions-smart
                    ((:determine-interacting-agents-mode . :default)
                     (:question-sample-mode . :all)    
                     (:scenes-per-question . 50)
                     (:confidence-threshold . 1.1)
                     (:tutor-sample-mode . :smart)
                     (:cxn-decf-score . 0.3)
                     (:cxn-inhibit-score . 0.1)
                     (:primitives . :symbolic)
                     (:learner-cxn-supplier . :hashed-and-scored)
                     (:alignment-strategy . :lateral-inhibition)
                     (:hide-type-hierarchy . t)))
                   )
                 :number-of-interactions 100000
                 :number-of-series 1
                 :monitors (append (get-all-lisp-monitors)
                                   (list "print-a-dot-for-each-interaction")))

(run-experiments '(
                   (test 
                    ((:determine-interacting-agents-mode . :default)
                     (:question-sample-mode . :all)
                     ;(:questions-per-challenge . 5000)
                     (:scenes-per-question . 20)
                     (:confidence-threshold . 1.1)
                     (:tutor-sample-mode . :random)
                     (:cxn-decf-score . 0.3)
                     (:primitives . :symbolic)
                     (:learner-cxn-supplier . :hashed-and-scored)
                     (:alignment-strategy . :lateral-inhibition)
                     (:hide-type-hierarchy . t)))
                   )
                 :number-of-interactions 50000
                 :number-of-series 1
                 :monitors (append (get-all-lisp-monitors)
                                   (get-all-export-monitors)
                                   (list "print-a-dot-for-each-interaction"
                                         "display-metrics")))



;;;; CREATING GRAPHS
;;;; ---------------

(create-all-graphs-for-single-experiment "th-link-mixed-mode-random-tutor")

(create-all-graphs-comparing-experiment '("th-link-mixed-mode-random-tutor"
                                          "th-link-mixed-mode-smart-tutor")
                                        '("baseline tutor"
                                          "probabilistic tutor"))

(create-graph-for-single-strategy 
 "th-link-mixed-mode-random-tutor"
 '("communicative-success" "lexicon-size")
 :average-windows '(100 1)
 :use-y-axis '(1 2) :y1-min 0 :y1-max 1 :y2-max nil
 :x-label "Number of Communicative Interactions"
 :y1-label "Communicative Success"
 :y2-label "Construction Inventory Size"
 :captions '("communicative success" "construction inventory size")
 :open t :logscale "x 2")

(create-graph-for-single-strategy
 :experiment-name "th-link-mixed-mode-random-tutor"
 :measure-names '("number-of-holophrase-cxns"
                  "number-of-item-based-cxns"
                  "number-of-lexical-cxns")
 :average-windows '(1 1 1)
 :y-axis '(1) :y1-min 0 :y1-max nil
 :xlabel "Number of Games"
 :y1-label "Number of Constructions"
 :captions '("holophrase" "item-based" "lexical")
 :end 10000 :open nil)  

(create-graph-for-single-strategy
 :experiment-name "th-link-mixed-mode-random-tutor"
 :measure-names '("repair-add-holophrase"
                  "repair-holophrase-to-item-based"
                  "repair-lexical-to-item-based"
                  "repair-item-based-to-lexical"
                  "repair-add-th-links")
 :average-windows '(250 250 250 250 250)
 :y-axis '(1 1 1 1 1) :y1-max 0.5
 :xlabel "Number of Games"
 :y1-label "Active in Fraction of Games"
 :captions '("learn holophrase" "generalise over holophrase"
             "partial analysis (lexical)"
             "partial analysis (item-based)"
             "extend categorial network")
 :open nil)

(create-graph-for-single-strategy
   :experiment-name "100k-composer-strategy-store-scenes-random-tutor"
   :measure-names '("failed-formulation-th-link"
                    "successful-formulation-th-link")
   :average-windows 1
   :y-axis '(1 1) :y1-min 0 :y1-max nil
   :xlabel "Number of Games"
   :y1-label "Success"
   :open nil)

(loop for experiment in '(default-configurations
                          cxn-supplier-with-scores
                          random-tutor-sample-mode)
      ;do (loop for i below 10
      do (raw-files->evo-plot
          :raw-file-paths
          `(("experiments" "clevr-learning" "raw-data" "latest"
             ,(downcase (mkstr experiment)) "communicative-success")
            ("experiments" "clevr-learning" "raw-data" "latest"
             ,(downcase (mkstr experiment)) "lexicon-size"))
          ;:title (format nil "Series ~a" i)
          :captions '("communicative success" "grammar size")
          :average-windows 1000
          ;:plot-file-name (format nil "series-~a" i)
          ;:plot-directory `("experiments" "clevr-learning" "raw-data"
          ;                  ,(downcase (mkstr experiment)) "per-series")
          ;:series-numbers (list i)
          :use-y-axis '(1 2) :y1-min 0 :y1-max 1 :y2-min 0
          :y1-label "Communicative Success"
          :y2-label "Grammar Size"
          :open nil))

(defun create-all-graphs (experiment-name)
  (create-graph-for-single-strategy :experiment-name experiment-name
                                    :measure-names '("communicative-success"
                                                     "lexicon-size")
                                    :y-axis '(1 2) :y1-max 1 :open nil))

(create-all-graphs "test-smart-speaker-learner")

(create-graph-comparing-strategies
 :experiment-names '("0.4-250k-composer-strategy-store-scenes-random-tutor"
                     "0.4-250k-composer-strategy-store-scenes-smart-tutor")
 :measure-name "number-of-item-based-cxns"
 :y1-label "Number of Item-Based Constructions"
 :captions '("baseline tutor" "probabilistic tutor")
 :average-windows 1
 :y-min 0 :y-max nil
 :xlabel "Number of Games"
 ;:end 25000
 :open nil)

(create-graph-comparing-strategies
 :experiment-names '("v1-composer-strategy-store-scenes-smart-tutor"
                     "v2-composer-strategy-store-scenes-smart-tutor"
                     "v3-composer-strategy-store-scenes-smart-tutor"
                     "v4-composer-strategy-store-scenes-smart-tutor")
 :measure-name "lexicon-size" :y1-label "Grammar Size"
 :captions '("v1" "v2" "v3" "v4") :y-max nil
 :xlabel "Number of Games" :open nil)

(create-graph-comparing-strategies
 :experiment-names (mapcar #'downcase
                           (mapcar #'mkstr
                                   '(cxn-decf-0.2-confidence-0.2
                                     cxn-decf-0.2-confidence-0.3
                                     cxn-decf-0.3-confidence-0.1
                                     cxn-decf-0.3-confidence-0.3
                                     cxn-decf-0.3-confidence-0.4)))
 :measure-name "communicative-success"
 :y1-label "Communicative Success"
 :title "Effect of :cxn-decf-score and :learner-speaks-confidence-level"
 :y-min 0.95 :y-max 1 :open t)

(create-graph-comparing-strategies
 :experiment-names (mapcar #'downcase
                           (mapcar #'mkstr
                                   '(cxn-decf-0.2-confidence-0.2
                                     cxn-decf-0.2-confidence-0.3
                                     cxn-decf-0.3-confidence-0.1
                                     cxn-decf-0.3-confidence-0.3
                                     cxn-decf-0.3-confidence-0.4)))
 :measure-name "lexicon-size"
 :y1-label "Grammar Size"
 :title "Effect of :cxn-decf-score and :learner-speaks-confidence-level"
 :y-min 0 :y-max nil :open t)
