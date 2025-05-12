;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;; Script for running a DEMO ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :clg)
(in-package :clg)

;; -----------------------------
;; + Experiment Configurations +
;; -----------------------------

;; Finding the data


(defparameter *configuration* (make-configuration
                               :entries '((:determine-interacting-agents-mode . :tutor-learner)
                                          (:question-sample-mode . :all)
                                          ;(:questions-per-challenge . 1000)
                                          (:scenes-per-question . 50)
                                          (:confidence-threshold . 1.1)
                                          (:tutor-sample-mode . :random) ;; or :random
                                          (:cxn-incf-score . 0.3)
                                          (:cxn-decf-score . 0.01)
                                          (:cxn-inhibit-score . 0.01)
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
                                          (:update-concepts-p . t)
                                          )))

(defparameter *experiment* (make-instance 'clevr-learning-experiment :configuration *configuration*))

(progn
  (format t "~% Starting a new experiment.~%")
  ;; reset the web interface
  (wi::reset)
  ;; deactivate all monitors (as a sanity check)
  (monitors::notify reset-monitors)

  ;; reset population
  (setf (population *experiment*) (list (make-clevr-learning-tutor *experiment*)
                                        (make-clevr-learning-learner *experiment*))))

#|
  ;; Option 1: run experiment with real-time plotting (using gnuplot)
(progn
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor display-metrics)

  (run-series *experiment* 5000))

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

  (run-interaction *experiment*)
  )

|#




;; create constructions for concepts, add them to construction grammar
;; bind statement with id to object, add it to ontology
;; partial analysis with those constructions
;; compose-program with partial program

(progn
  (add-element `((h4) "Inventory: " ,(make-html (grammar (second (agents *experiment*))))))
  (add-element (make-html (categorial-network (grammar (second (agents *experiment*)))) :weights? t)))


(loop for id being the hash-keys of (first (get-data (ontology (second (agents *experiment*))) 'all-concepts))
        using (hash-value concept) and idx from 0
      do (add-element `((h2) ,(format nil "~a: ~a" idx (mkstr id))))
      do (concept-representations::add-concept-to-interface (meaning concept) :weight-threshold 0.5))
        

;(set-up-concepts (second (agents *experiment*)))

(progn
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)

  (setf (topic (second (agents *experiment*))) 2)

  (comprehend "how many sphere are there" :cxn-inventory (grammar (second (agents *experiment*)))))

 
(first (objects (current-scene (world *experiment*))))


(setf inventory (cl-store::restore (babel-pathname
                                     :directory `("experiments" 
                                                  "concept-emergence2" 
                                                  "storage"
                                                  "cle4-grammar")
                                     :name (format nil "inventory")
                                     :type "store")))


(setf sims 
      (loop with obj = (first (objects (current-scene (world *experiment*))))
            for form being the hash-keys of inventory
              using (hash-value concept)
            collect (cons form (concept-representations::concept-entity-similarity concept obj))))

(loop for el in (sort (copy-list sims) #'> :key #'cdr)
      do (format t "~% (~a : ~,3f)" (car el) (cdr el)))

(loop for form being the hash-keys of inventory
        using (hash-value concept)
      do (add-element `((h2) "Form: " ,form))
      do (concept-representations::add-concept-to-interface concept :weight-threshold 0.1))
