(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    ;;
;;  Comprehension and interpretation  ;;
;;                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric comprehend-and-interpret (hearer scene)
  (:documentation "Based on the topic and scene, the hearer comprehends the utterance"))


(defmethod comprehend-and-interpret ((hearer crs-conventionality-agent) (scene crs-conventionality-scene))
  "Based on the topic and scene, the hearer comprehends the utterance"
  (interpret (utterance hearer) :cxn-inventory (grammar hearer) :agent hearer :scene scene))


(defmethod interpret ((utterance list) &key cxn-inventory
                      agent scene (silent nil) (n 1))  
  ;; Store topic, agent and scene in the blackboard of the cxn-inventory
  (set-data (blackboard (grammar agent)) :agent agent)
  (set-data (blackboard (grammar agent)) :scene scene)

  (unless silent (notify routine-interpretation-started agent scene))

  (let* ((solution-and-cip (multiple-value-list (fcg-apply-with-n-solutions (processing-cxn-inventory cxn-inventory)
                                                                            (create-initial-structure utterance
                                                                                                      (get-configuration cxn-inventory :create-initial-structure-mode-comprehension)
                                                                                                      :scene scene)
                                                                            '<- 1
                                                                            :notify (not silent))))
         (solution-node (first solution-and-cip))
         (cip (second solution-and-cip)))
    
    (if (succeeded-nodes cip)
       
      (progn
        (setf (computed-topic agent) (get-data (first solution-node) :computed-topic))
        (setf (applied-constructions agent) (applied-constructions (first solution-node)))
        (unless silent (notify routine-interpretation-finished cip agent scene)))

      (progn
        (loop for diagnostic in (reverse (get-configuration cxn-inventory :interpretation-diagnostics))
              do (fcg::add-diagnostic (top-node cip) diagnostic))
        (when (get-configuration (experiment agent) :introduce-noise-after-interaction)
          (when (< (get-configuration (experiment agent) :introduce-noise-after-interaction) ;; add form-similarity repair only when there is noise
                   (interaction-number (current-interaction (experiment agent))))          
            (loop for repair in (reverse (get-configuration cxn-inventory :repairs))
                  do (fcg::add-repair (top-node cip) repair))))
        (set-data (blackboard (grammar agent)) :cipn (top-node cip))
        ;; Notify learning

        (let* ((fix (first (second (multiple-value-list (fcg::notify-learning (top-node cip) :trigger 'fcg::routine-processing-finished))))))
          (when fix
            (let* ((cxn (first (fcg::restart-data fix)))
                   (best-solution nil)
                   (consolidated-cxns nil)
                   (current-node (fcg::top-node cip))
                   (fixed-car (first (get-data fix 'fcg::fixed-cars))) 
                   (child (fcg::cip-add-child current-node fixed-car)))
              
              (setf current-node child)
              (push (type-of (fcg::issued-by fix)) (statuses child))
              (setf (fcg::fully-expanded? child) t)
              (fcg::cip-run-goal-tests child cip) ;; to include succeeded status in node statuses
              (push 'added-by-repair (statuses child))
              
; (add-cxn cxn cxn-inventory)
;(push cxn consolidated-cxns)
              
              
              (setf (computed-topic agent) (get-data child :computed-topic))
              (setf (applied-constructions agent) (list (processing-cxn cxn) ))
              )))



      ))))


(defmethod create-initial-structure ((utterance list)
                                     (mode (eql :utterance-and-scene))
                                     &key scene)
  "In formulation, the initial transient structure holds the topic and scene."
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
                               (scene ?scene)
                               (form ,utterance)))
		 :right-pole '((root))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))


(defmethod cip-goal-test ((node cip-node) (mode (eql :interpretation-in-scene)))
  "Checks whether the extracted meaning can be evaluated in the scene."
  (let* ((irl-program (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node)))))
         (primitive-inventory (find-data (blackboard (construction-inventory node)) :primitive-inventory))
         (ontology (find-data (blackboard (construction-inventory node)) :ontology))
         (target-var (irl::get-target-var irl-program))
         (irl-solution (first (irl::evaluate-irl-program irl-program ontology :primitive-inventory primitive-inventory :n 1)))
         (computed-topic (when irl-solution (irl::value (find target-var irl-solution :key #'irl::var))))
         (success (when irl-solution t)))

    ;set computed-target to access later on
    (if success
      (set-data node :computed-topic computed-topic)
      (set-data node :computed-topic nil))

    success))
