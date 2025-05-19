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
                                          (:sort-questions-on-length . t)
                                          
                                          
                                          
                                          (:data-source . "simulated") ;; "simulated" or "extracted"
                                          (:pretrained-concepts . t)
                                          (:update-concepts-with-success . nil)

                                          (:sigmoid-slope-c . 0.5)

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
                                                    item-based->lexical
                                                    holophrase->item-based--substitution
                                                    holophrase->item-based--addition
                                                    holophrase->item-based--deletion
                                                    add-holophrase
                                                    ))))

;; (ontology (second (agents *experiment*)))

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

;; Option 1: run experiment with real-time plotting (using gnuplot)
(progn
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor display-metrics)

  (run-series *experiment* 5000))

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
  (add-element (make-html (categorial-network (grammar (second (agents *experiment*)))) :weights? t)))


(loop for id being the hash-keys of (get-data (ontology (second (agents *experiment*))) 'concepts)
        using (hash-value concept) and idx from 0
      do (add-element `((h2) ,(format nil "~a: ~a" idx (mkstr id))))
      do (concept-representations::add-concept-to-interface (meaning concept) :weight-threshold 0.5))























;; overwrite ontology
(set-data (ontology (second (agents *experiment*))) 'all-concepts
          (copy-object (get-data (ontology (second (agents *experiment*))) 'all-concepts)))



(gethash (first (hash-keys (first (get-data (ontology (second (agents *experiment*))) 'all-concepts))))
          (first (get-data (ontology (second (agents *experiment*))) 'all-concepts)))
 

#|

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


;; test copy-object
(setf *scene* (objects (get-data (ontology (second (agents *experiment*))) 'clevr-context)))

(setf *original-concepts-ht* (first (get-data (ontology (second (agents *experiment*))) 'all-concepts)))

(setf *id* (first (hash-keys *concepts*)))

(gethash *id* *original-concepts-ht*)

(setf *copy-concepts-ht* (copy-object (first (get-data (ontology (second (agents *experiment*))) 'all-concepts))))

(concept-representations::add-concept-to-interface (meaning (gethash *id* *original-concepts-ht*)) :weight-threshold 0.5)

(concept-representations::update-concept (meaning (gethash *id* *original-concepts-ht*)) (first *scene*) (rest *scene*))

(concept-representations::add-concept-to-interface (meaning (gethash *id* *copy-concepts-ht*)) :weight-threshold 0.5)



  
  (wi::reset)
  ;; reset monitors
  (deactivate-all-monitors)

  ;; activate monitors
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi)
  ;(activate-monitor display-metrics)
  ;(activate-monitor trace-tasks-and-processes)

  (run-series *experiment* 10)
  
  )



;(set-data (ontology learner) 'max-concept-update-iterations (get-configuration experiment :max-concept-update-iterations))
;(set-data (ontology learner) 'filter-similarity-threshold (get-configuration experiment :filter-similarity-threshold))


(notify-learning

(defun test ()
  (let* ((form-and-concepts (loop for cxn in (constructions-list (grammar (second (agents *experiment*))))
                                  for concepts = (get-data (ontology (second (agents *experiment*))) 'concepts)
                                  for string = (attr-val cxn :string)
                                  for meaning = (attr-val cxn :meaning)
                                  for type = (attr-val cxn :cxn-type)
                                  for score = (attr-val cxn :score)
                                  for concept = (gethash meaning concepts)
                                  when (and concept (eq type 'lexical))
                                    do (concept-representations::add-concept-to-interface (meaning concept) :weight-threshold 0.5)
                                    and collect (list string concept score) into form-and-concepts
                                  do (add-element (make-html cxn))
                                  finally (return form-and-concepts)))
         (unique-forms (remove-duplicates (mapcar #'first form-and-concepts) :test #'string=)))
    (setf sorted-form-and-concepts (sort form-and-concepts #'> :key #'third))
    (loop for unique-form in unique-forms
          ;; get all concepts that have the same form as unique-form
          for concepts = (mapcar #'second (find-all unique-form sorted-form-and-concepts :key #'first :test #'string=))
          for scores = (mapcar #'third (find-all unique-form sorted-form-and-concepts :key #'first :test #'string=))

          when (string= unique-form "blocks")
            do (format t "~a~%" unique-form)
            
            and do (format t "~{~,2f~^, ~}~%" (mapcar #'id concepts))
            and do (format t "~{~,2f~^, ~}~%" scores)

            and do (loop for concept in concepts
                         for similarity-scores
                           = (mapcar #'(lambda (c2) (concept-representations::concept-similarity (meaning concept) (meaning c2))) concepts)
                         for easy-similarity-scores
                           = (mapcar #'(lambda (score) (if (> score 0.7) "high" "low")) similarity-scores)
                         do (format t "~a: ~{~,2f~^, ~}~%" (id concept) similarity-scores)))))


(test)

 ;(grammar (second (agents *experiment*)))


(loop for id being the hash-keys of (get-data (ontology (second (agents *experiment*))) 'concepts)
        using (hash-value concept) and idx from 0
      do (add-element `((h2) ,(format nil "~a: ~a" idx (mkstr id))))
      do (concept-representations::add-concept-to-interface (meaning concept) :weight-threshold 0.5))

(reset-id-counters)

