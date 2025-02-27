(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    ;;
;;  Conceptualisation and production  ;;
;;                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Remi told us not to do this BUT we want to explore this anyway... To be continued :)
;; We will prove Remi wrong! 
;; Update: 31/01/2025, we did it.

;; We want to do conceptualisation and formulation at the same time since these are two processes that are integrated.
;; If we conceptualise, we use information from the grammar (e.g. in concept learning we use entrenchment scores), also if we don't use constructional information in conceptualisation, it could be the case that you conceptualise something that you cannot formulate. 

(defgeneric conceptualise-and-produce (speaker scene topic &key &allow-other-keys)
  (:documentation "Based on the topic and scene, the speaker produces an utterance."))


(defmethod conceptualise-and-produce ((agent crs-conventionality-agent)
                                      (scene crs-conventionality-scene)
                                      (topic crs-conventionality-entity-set)
                                      &key (silent nil) (n 5) (use-meta-layer t)) ;; n default hardcoded?
  "Based on the topic and scene, the speaker produces an utterance.
   The agents attempts first routine formulation, in case of failure, it goes to metalayer."
  ;; Store topic, agent and scene in the blackboard of the cxn-inventory
  (set-data (blackboard (grammar agent)) :topic topic) ;; SHOULD THIS BE HERE?
  (set-data (blackboard (grammar agent)) :agent agent) ;; SHOULD THIS BE HERE?
  (set-data (blackboard (grammar agent)) :scene scene) ;; SHOULD THIS BE HERE?
  (setf (topic agent) topic) ;; TODO to remove?

  (unless silent (notify routine-conceptualisation-started topic agent scene))

  ;; routine formulation
  (let* ((cxn-inventory (grammar agent))
         (solution-and-cip (multiple-value-list (fcg-apply-with-n-solutions (processing-cxn-inventory cxn-inventory)
                                                                            (create-initial-structure topic
                                                                                                      (get-configuration cxn-inventory :create-initial-structure-mode-formulation)
                                                                                                      :scene scene)
                                                                            '-> n
                                                                            :notify (not silent))))
         (solution-node (first solution-and-cip)) ;;TO DO: select best solution
         (cip (second solution-and-cip)))

    (unless silent
      (if (eq 'speaker (experiment-framework::discourse-role agent))
        (notify routine-conceptualisation-finished cip solution-node agent)))
    
    ;; check if a solution is found
    (if (not (succeeded-nodes cip))
      (progn
        (when use-meta-layer
          ;; ! meta-layer !
          ;;     if no solution is found, start invention and set utterance in agent
          (unless silent (notify meta-conceptualisation-started topic agent scene))
          (multiple-value-bind (cxn fix)
              (fcg::invent cip agent topic scene)
            (unless silent (notify meta-conceptualisation-finished fix agent))
            (setf (conceptualised-utterance agent) (render (car-resulting-cfs (first (get-data (blackboard fix) 'fcg::fixed-cars)))
                                                           (get-configuration (grammar agent) :render-mode)))
            (setf (applied-constructions agent) (list (processing-cxn cxn))))
          (setf (invention (interaction scene)) t)))
      ;; otherwise, render and set solution nodes
      (progn
        (setf (conceptualised-utterance agent) (render (car-resulting-cfs (fcg:cipn-car (first solution-node)))
                                                       (get-configuration (grammar agent) :render-mode)))
        (when (eq (discourse-role agent) 'speaker)
          
          (setf (applied-constructions agent) (applied-constructions (first solution-node))) 
          (setf (solution-node agent) (first solution-node)))))))


(defmethod create-initial-structure ((topic crs-conventionality-entity-set)
                                     (mode (eql :topic-and-scene))
                                     &key scene)
  "In formulation, the initial transient structure holds the topic and scene."
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
                               (topic ,topic)
                               (scene ?scene)
                               (meaning (;(get-scene ?scene)
                                         (bind crs-conventionality-entity-set ?scene ,scene)))))
		 :right-pole '((root))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))


(in-package :fcg)
;; ! SPECIALISES METHODS IN :fcg

(defmethod cip-goal-test ((node cip-node) (mode (eql :topic-retrieved))) 
  "Checks whether the extracted meaning leads to the topic by evaluating irl program."
  (let* ((irl-program (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node)))))
         (topic (first (crs-conventionality::entities (find-data (blackboard (construction-inventory node)) :topic))))
         (primitive-inventory (find-data (blackboard (construction-inventory node)) :primitive-inventory))
         (ontology (find-data (blackboard (construction-inventory node)) :ontology))
         (target-var (irl::get-target-var irl-program))
         (irl-solution (first (irl::evaluate-irl-program irl-program ontology :primitive-inventory primitive-inventory :n 1))))
    (when target-var
      (irl::equal-entity topic (irl::value (find target-var irl-solution :key #'irl::var))))))


(defmethod cip-node-test ((node cip-node) (mode (eql :check-branch-for-solution)))
  "Checks whether the node's parent is a solution."
  (let ((solution-in-branch (find 'succeeded (mappend #'statuses (all-parents node)))))
    (if solution-in-branch
      (progn
        (push 'solution-in-branch (statuses node))
        nil)
      t)))
