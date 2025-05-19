;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                    ;;
;; Script for running a DEMO with pretrained concepts ;;
;;                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clg)
(in-package :clg)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Finding the data

(defparameter *configuration* (utils::make-configuration
                               :entries `((:determine-interacting-agents-mode . :tutor-learner)
                                          (:question-sample-mode . :all)
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
                                          (:composer-strategy . :standard)
                                          (:th-link-repair-mode-comprehension . :no-path-required)
                                          (:th-link-repair-mode-formulation . :path-required)
                                          ;; new configuration

                                          ;; logging and monitors
                                          (:log-every-x-interactions . 100)
                                          ;(:sort-questions-on-length . t) ;; doesnt work yet

                                          (:initial-seed . 42)
                                          (:data-source . "simulated") ;; "simulated" or "extracted"
                                          (:pretrained-concepts . t)
                                          (:update-concepts-with-success . nil)

                                          (:sigmoid-slope-c . 0.5) ;; todo

                                          ;; for update-concept repair
                                          (:max-concept-update-iterations . 10)
                                          (:filter-similarity-threshold . 0.5)
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
                                                    ))))

;; (ontology (second (agents *experiment*)))

(defparameter *experiment* (make-instance 'clevr-learning-experiment :configuration *configuration*))

(progn
  (format t "~% Starting a new experiment.~%")
  ;; reset the seed
  (set-seed (get-configuration *experiment* :initial-seed))
  ;; reset the web interface
  (wi::reset)
  ;; deactivate all monitors (as a sanity check)
  (monitors::notify reset-monitors)

  ;; reset population
  (setf (population *experiment*) (list (make-clevr-learning-tutor *experiment*)
                                        (make-clevr-learning-learner *experiment*))))

;; Option 1: run experiment with real-time plotting (using gnuplot)
(progn
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor print-a-dot-for-each-interaction)
  ;(activate-monitor log-every-x-interactions-in-output-browser)
  (activate-monitor display-metrics)

  (run-series *experiment* 20000))

;; Option 2: run experiment with real-time tracing in the web-interface
(progn
  (wi::reset)
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  ;(activate-monitor trace-tasks-and-processes)

  (run-series *experiment* 1)
  )


;; debugging
(progn
  (add-element `((h4) "Inventory: " ,(make-html (grammar (second (agents *experiment*))))))
  (add-element (make-html (categorial-network (grammar (second (agents *experiment*)))) :weights? t :render-program "circo")))


(loop for id being the hash-keys of (get-data (ontology (second (agents *experiment*))) 'concepts)
        using (hash-value concept) and idx from 0
      do (add-element `((h2) ,(format nil "~a: ~a" idx (mkstr id))))
      do (concept-representations::add-concept-to-interface (meaning concept) :weight-threshold 0.5))
