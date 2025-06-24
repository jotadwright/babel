(ql:quickload :clg)
(in-package :clg)

#+sbcl (sb-int:set-floating-point-modes :traps '(:INVALID :DIVIDE-BY-ZERO))

;; --------------------------------------------
;; + PHASE 1: LEARNING GROUNDED CONSTRUCTIONS +
;; --------------------------------------------

;; step 1: navigate to concept-emergence2/scripts/export-lexicons-to-cr-package.lisp
;; step 2: compile file


;; -----------------------------------------------
;; + PHASE 2: LEARNING GRAMMATICAL CONSTRUCTIONS +
;; -----------------------------------------------

(defun disable-meta-layer-configuration (agent)
  (let ((cxn-inventory (grammar agent)))
    (set-configuration cxn-inventory :category-linking-mode :neighbours)
    (set-configuration cxn-inventory :update-th-links nil)
    (set-configuration cxn-inventory :use-meta-layer nil)
    (set-configuration cxn-inventory :consolidate-repairs nil)))

(defun enable-meta-layer-configuration (agent)
  (let ((cxn-inventory (grammar agent)))
    (set-configuration cxn-inventory :category-linking-mode :neighbours)
    (set-configuration cxn-inventory :update-th-links t)
    (set-configuration cxn-inventory :use-meta-layer t)
    (set-configuration cxn-inventory :consolidate-repairs t)))


(loop for seed in (list 1 2 3 4 5 6 7 8 9 10)
      do (format t "Next seed: ~a" seed)
      do (progn


           ;; !!! everything on stage 1 data without synonyms!

           (defparameter *configuration* (utils::make-configuration
                                          :entries `((:determine-interacting-agents-mode . :tutor-learner)
                                                     (:question-sample-mode . :all)
                                                     (:questions-type . :count)
                                                     (:nr-of-filters . :all)
                                          ;(:questions-type . :query)
                                          ;(:nr-of-filters . :all)
                                          ;(:questions-per-challenge . 1000)
                                                     (:scenes-per-question . 15000)
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
                                                     (:experiment-group . "cxn-nlp")
                                                     (:dataset-split . "val")
                                                     (:current-split . :train) ;; current phase: if "train" -> training, if "test" -> no updates, only testing
                                                     (:scenes-train-test-split-index . 1000)
                                                     (:experiment-name . "clevr-simulated")
                                          
                                                     ;; new configuration
                                          ;(:sort-questions-on-length . t) ;; doesnt work yet

                                                     (:seed . ,seed)
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




           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;;;;;; Make experiment  + set population    ;;;;;;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           (defparameter *experiment* (make-instance 'clevr-learning-experiment :configuration *configuration*))

           ;; ENABLE META LAYER
           (enable-meta-layer-configuration (second (agents *experiment*)))

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

           ;; --------------------
           ;; + Phase 2A: filter +
           ;; --------------------

           (progn
             ;; reset monitors
             (deactivate-all-monitors)

             ;; activate monitors
             (activate-monitor print-a-dot-for-each-interaction)
             ;(activate-monitor display-metrics)
  

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
                                    ;"display-metrics"
                                    )
                              *experiment*)
             (activate-monitor export-type-hierarchy-to-json)

             (set-configuration *experiment* :questions-type :count)
             (set-configuration *experiment* :nr-of-filters :all)
             (load-questions-for-current-challenge-level *experiment* :all)
             (run-series *experiment* 5000))

           ;; ---------------------------------------
           ;; + Phase 2B: existence check questions +
           ;; ---------------------------------------


           ;; load in the correct data (first set configurations of the experiment and then load questions):
           (progn
             (set-configuration *experiment* :questions-type :exist)
             (set-configuration *experiment* :nr-of-filters :all)
             (load-questions-for-current-challenge-level *experiment* :all)
             (run-series *experiment* 5000))

           ;; -----------------------------
           ;; + Phase 2C: query questions +
           ;; -----------------------------

           ;; Third phase of the experiment: 

           ;; Setup categories + load in correct data
           (progn
             (set-configuration *experiment* :category-strategy-threshold 0.7) 
  
             (setf *categories* (make-categories (second (agents *experiment*))))
  
             (set-configuration *experiment* :questions-type :query)
             (set-configuration *experiment* :nr-of-filters :all)
  
             (load-questions-for-current-challenge-level *experiment* :all)
  
             (set-data (ontology (second (agents *experiment*))) 'categories  *categories*)
             (run-series *experiment* 5000))



           ;; -------------------------------
           ;; + Phase 3: test on everything +
           ;; -------------------------------

           ;; DISABLE META LAYER
           (disable-meta-layer-configuration (second (agents *experiment*)))

           (progn
             (set-configuration *experiment* :questions-type :count-exist-query-all) ;:count-exist-query-all
             (set-configuration *experiment* :nr-of-filters :all)
  
             (load-questions-for-current-challenge-level *experiment* :all)

             (set-configuration *experiment* :current-split :test)

             (run-series *experiment* 5000))
  
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; Notify that series are finished ;;;;;;;;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           (progn
             (notify run-series-finished *experiment*)
             (notify series-finished 1)
             (notify batch-finished (class-string *experiment*)))))