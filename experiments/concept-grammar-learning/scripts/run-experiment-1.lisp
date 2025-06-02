(ql:quickload :clg)
(in-package :clg)

#+sbcl (sb-int:set-floating-point-modes :traps '(:INVALID :DIVIDE-BY-ZERO))

(defparameter *configuration* (utils::make-configuration
                               :entries `((:determine-interacting-agents-mode . :tutor-learner)
                                          (:question-sample-mode . :all)
                                          (:questions-type . :count)
                                          (:nr-of-filters . :all)
                                          ;(:questions-type . :query)
                                          ;(:nr-of-filters . :all)
                                          ;(:questions-per-challenge . 1000)
                                          (:scenes-per-question . 50)
                                          (:confidence-threshold . 1.1)
                                          (:tutor-sample-mode . :random) ;; or :random
                                          (:cxn-incf-score . 0.1)
                                          (:cxn-decf-score . 0.1)
                                          (:cxn-inhibit-score . 0.1)
                                          (:chunk-incf-score . 0.1)
                                          (:chunk-decf-score . 0.1)
                                          (:primitives . :symbolic)
                                          (:learner-cxn-supplier . :hashed-and-scored)
                                          (:alignment-strategy . :lateral-inhibition)
                                          (:hide-type-hierarchy . nil)
                                          (:remove-cxn-on-lower-bound . t)
                                          
                                          (:th-link-repair-mode-comprehension . :no-path-required)
                                          (:th-link-repair-mode-formulation . :path-required)
                                          ;; new configuration

                                          ;; logging and monitors
                                          (:log-every-x-interactions . 100)
                                          (:experiment-group . "clevr")
                                          (:dataset-split . "val")
                                          (:experiment-name . "clevr-simulated")
                                          
                                          ;; new configuration
                                          ;(:sort-questions-on-length . t) ;; doesnt work yet

                                          (:seed . 42)
                                          (:data-source . "simulated") ;; "simulated" or "extracted"
                                          (:pretrained-concepts . t)
                                          (:update-concepts-with-success . nil)
                                          (:nr-of-categories . 4)

                                          (:sigmoid-slope-c . 0.5) ;; todo

                                          ;; category-strategy
                                          (:category-strategy . :use-categorial-network) ; :use-predefined-categories :use-categorial-network
                                          (:category-strategy-threshold . 0)

                                          ;; for update-concept repair
                                          (:max-concept-update-iterations . 10)
                                          (:filter-similarity-threshold . 0.1)
                                          (:lexical-cxn-inhibition-value . 0.02)

                                          ;; diagnostics and repairs (order is important!)
                                          (:diagnostics 
                                                        diagnose-failed-interpretation
                                                        diagnose-partial-utterance
                                                        diagnose-unknown-utterance
                                                        diagnose-partial-meaning
                                                        )
                                          (:repairs 
                                                    ;add-th-links-formulation
                                                    ;update-concept
                                                    add-th-links
                                                    lexical->item-based
                                                    ;item-based->lexical
                                                    ;holophrase->item-based--substitution
                                                    ;holophrase->item-based--addition
                                                    ;holophrase->item-based--deletion
                                                    add-holophrase
                                                    )
                                          
                                          ;; composer strategy
                                          (:composer-strategy . :standard) ;; :standard or :store-past-scenes
                                          ;; composer node tests
                                          ;;    - remove clevr incoherent filter groups checks the type of the bindings of the filter group,
                                          ;;    - you cannot have multiple filters that filter on the same type of bind statement
                                          (:composer-node-tests
                                           ;;  - this one checks the type of the bindings of the filter group,
                                           ;;  - you cannot have multiple filters that filter on the same type of bind statement
                                            
                                           :remove-clevr-filter-permutations)
                                          
                                          )))

;; (ontology (second (agents *experiment*)))
;; (question-data  *experiment*)
(reset-id-counters)

(defparameter *experiment* (make-instance 'clevr-learning-experiment :configuration *configuration*))

(progn
  (format t "~% Starting a new experiment.~%")
  ;; reset the seed
  (set-seed (get-configuration *experiment* :seed))
  ;; reset the web interface
  (wi::reset)
  ;; deactivate all monitors (as a sanity check)
  (monitors::notify reset-monitors)

  ;; reset population
  (setf (population *experiment*) (list (make-clevr-learning-tutor *experiment*)
                                        (make-clevr-learning-learner *experiment*))))

;  (add-element (make-html (categorial-network (grammar (learner *experiment*)))))

;; First phase of the experiment: all count questions
(progn
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor display-metrics)
  

  (set-configuration *experiment* :experiment-run-name (generate-log-dir-name (get-configuration *experiment* :seed)))
  (set-seed (get-configuration *experiment* :seed))

  (set-up-monitors (list "log-every-x-interactions-in-output-browser"
                         "export-experiment-configurations"
                         "record-communicative-success"
                         "record-number-of-holophrase-cxns"
                         "record-number-of-item-based-cxns"
                         "record-number-of-lexical-cxns"
                         "export-communicative-success"
                         "export-number-of-holophrase-cxns"
                         "export-number-of-item-based-cxns"
                         "export-number-of-lexical-cxns"
                         "display-metrics")
                   *experiment*)
  (activate-monitor export-type-hierarchy-to-json)

  (run-series *experiment* 1000)

  )
#|(fcg::draw-categorial-network-node-and-neighbours
 (clg::grammar (clg::learner clg::*experiment*))
 (list 'FCG::red-2
       'FCG::purple-2
       'FCG::gray-2
       'FCG::yellow-2
       'FCG::green-2
       'FCG::blue-2
       'FCG::metal-2
       'FCG::rubber-2
       'FCG::large-2
       'FCG::small-2
       'FCG::cubes-2
       'FCG::spheres-2
       'FCG::cylinders-2)
 :included (list 'FCG::red-2
       'FCG::purple-2
       'FCG::gray-2
       'FCG::yellow-2
       'FCG::green-2
       'FCG::blue-2
       'FCG::metal-2
       'FCG::rubber-2
       'FCG::large-2
       'FCG::small-2
       'FCG::cubes-2
       'FCG::spheres-2
       'FCG::cylinders-2)
 :render-program "fdp"
 )|#



#|(fcg::draw-categorial-network-node-and-neighbours
 (clg::grammar (clg::learner clg::*experiment*))
 (list 'FCG::red-2
       'FCG::purple-2
       'FCG::gray-2
       'FCG::yellow-2
       'FCG::green-2
       'FCG::blue-2
       'FCG::metal-2
       'FCG::rubber-2
       'FCG::large-2
       'FCG::small-2
       'FCG::cubes-2
       'FCG::spheres-2
       'FCG::cylinders-2)
 :included (list 'fcg::how-many-?a-?z-?y-?x-are-there-\(?a\)-1
                 'fcg::how-many-?a-?z-?y-?x-are-there-\(?x\)-1
                 'fcg::how-many-?a-?z-?y-?x-are-there-\(?z\)-1
                 'fcg::how-many-?a-?z-?y-?x-are-there-\(?y\)-1
                 'fcg::how-many-?z-?y-?x-are-there-\(?y\)-1
                 'fcg::how-many-?z-?y-?x-are-there-\(?z\)-1
                 'fcg::how-many-?z-?y-?x-are-there-\(?x\)-1
                 'fcg::how-many-?y-?x-are-there-\(?y\)-1
                 'fcg::how-many-?y-?x-are-there-\(?x\)-1)

 :render-program "fdp"
 )|#
;; Second phase of the experiment: query questions

;; Setup categories
(progn
  (set-configuration *experiment* :category-strategy-threshold 0.8)
  
  (setf *categories* (make-categories (second (agents *experiment*))))
  
  (set-configuration *experiment* :questions-type :query)
  (set-configuration *experiment* :nr-of-filters :all)
  
  (load-questions-for-current-challenge-level *experiment* :all)
  
  (set-data (ontology (second (agents *experiment*))) 'categories  *categories*))


;; Run experiment in second phase


(run-series *experiment* 1000)


(notify run-series-finished *experiment*)
(notify series-finished 1)
(notify batch-finished (class-string *experiment*))
  


#|(fcg::draw-categorial-network-node-and-neighbours
 (clg::grammar (clg::learner clg::*experiment*))
 (list 'FCG::red-2
       'FCG::purple-2
       'FCG::gray-2
       'FCG::yellow-2
       'FCG::green-2
       'FCG::blue-2
       'FCG::metal-2
       'FCG::rubber-2
       'FCG::large-2
       'FCG::small-2
       'FCG::cubes-2
       'FCG::spheres-2
       'FCG::cylinders-2)
 :included (list 'fcg::how-many-?a-?z-?y-?x-are-there-\(?a\)-1
                 'fcg::how-many-?a-?z-?y-?x-are-there-\(?x\)-1
                 'fcg::how-many-?a-?z-?y-?x-are-there-\(?z\)-1
                 'fcg::how-many-?a-?z-?y-?x-are-there-\(?y\)-1
                 'fcg::how-many-?z-?y-?x-are-there-\(?y\)-1
                 'fcg::how-many-?z-?y-?x-are-there-\(?z\)-1
                 'fcg::how-many-?z-?y-?x-are-there-\(?x\)-1
                 'fcg::how-many-?y-?x-are-there-\(?y\)-1
                 'fcg::how-many-?y-?x-are-there-\(?x\)-1
                 'fcg::what-color-is-the-?x-1
                 'fcg::what-color-is-the-?x-?y-\(?y\)-1
                 'fcg::what-color-is-the-?x-?y-\(?x\)-1
                 'fcg::what-color-is-the-?z-?x-?y-\(?x\)-1
                 'fcg::what-color-is-the-?z-?x-?y-\(?y\)-1
                 'fcg::what-color-is-the-?z-?x-?y-\(?z\)-1
                 )
 :render-program "fdp"
 )|#


 



