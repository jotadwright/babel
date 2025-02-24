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
        (set-data (blackboard (grammar agent)) :cipn (top-node cip))
        ;; Notify learning
        (fcg::notify-learning (top-node cip) :trigger 'fcg::routine-processing-finished)))))


(defmethod create-initial-structure ((utterance list)
                                     (mode (eql :utterance-and-scene))
                                     &key scene)
  "In formulation, the initial transient structure holds the topic and scene."
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
                               (scene ?scene)
                               (form ,utterance)
                               (meaning ((bind crs-conventionality-entity-set ?scene ,scene)))))
		 :right-pole '((root))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))


(defmethod cip-goal-test ((node cip-node) (mode (eql :interpretation-in-scene)))
  "Checks whether the extracted meaning can be evaluated in the scene."
  "We need to connect the scene bind statement from the root to the meaning, therefore we remove the id of the ?scene variable. "
  (let* ((irl-program (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node)))))
         (success nil)
         (computed-topic nil))
    ;; only do this if there is meaning added by this node, by checking whether the length of the meaning of the resulting-cfs is equal to the meaning of the source-cfs.
    (when (not (equal (length irl-program)
                      (length (extract-meanings (left-pole-structure (car-source-cfs (cipn-car node)))))))
      (let* ((renamed-irl-program (loop for predicate in irl-program
                                        for last-var = (fourth predicate)
                                        when last-var
                                          do (when (equal (get-base-name last-var)  "SCENE")
                                               (setf (fourth predicate) 'crs-conventionality::?scene))
                                        collect predicate))
             (primitive-inventory (find-data (blackboard (construction-inventory node)) :primitive-inventory))
             (ontology (find-data (blackboard primitive-inventory) :ontology))
             (target-var (irl::get-target-var renamed-irl-program))
             (irl-solution (first (irl::evaluate-irl-program renamed-irl-program ontology :primitive-inventory primitive-inventory :n 1))))
        (setf computed-topic (when irl-solution (irl::value (find target-var irl-solution :key #'irl::var))))
        (setf success (if irl-solution t nil))))

    ;set computed-target to access later on
    (if success
      (set-data node :computed-topic computed-topic)
      (set-data node :computed-topic nil))

    success))


(defmethod cip-goal-test ((node cip-node) (mode (eql :discriminative-in-scene)))
  "Checks whether the extracted meaning leads to a solution, by binding the topic that it found. "
  "We need to connect the scene bind statement from the root to the meaning, therefore we remove the id of the ?scene variable. "
  (let* ((irl-program (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node)))))
         (success nil)
         (computed-topic nil))
    ;; only do this if there is meaning added by this node, by checking whether the length of the meaning of the resulting-cfs is equal to the meaning of the source-cfs.
    (when (not (equal (length irl-program)
                      (length (extract-meanings (left-pole-structure (car-source-cfs (cipn-car node)))))))
      (let* ((renamed-irl-program (loop for predicate in irl-program
                                        for last-var = (fourth predicate)
                                        when last-var
                                          do (when (equal (get-base-name last-var)  "SCENE")
                                               (setf (fourth predicate) 'crs-conventionality::?scene))
                                        collect predicate))
             (primitive-inventory (find-data (blackboard (construction-inventory node)) :primitive-inventory))
             (ontology (find-data (blackboard primitive-inventory) :ontology))
             (target-var (irl::get-target-var renamed-irl-program))
             (irl-solution (first (irl::evaluate-irl-program renamed-irl-program ontology :primitive-inventory primitive-inventory :n 1))))
     
        
        (setf computed-topic (when irl-solution (irl::value (find target-var irl-solution :key #'irl::var))))
        (setf success (if irl-solution t nil))))

    ;set computed-target to access later on
    (if success
      (set-data node :computed-topic computed-topic)
      (set-data node :computed-topic nil))

    success))