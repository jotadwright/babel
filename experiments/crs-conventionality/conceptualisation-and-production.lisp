(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                    ;;
;; Code implementing conceptualisation and production ;;
;;                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric conceptualise-and-produce (speaker scene topic)
  (:documentation "Based on the topic and scene, the speaker produces an utterance."))

(defmethod conceptualise-and-produce ((speaker crs-conventionality-agent) (scene crs-conventionality-scene) (topic crs-conventionality-entity-set))
  "Based on the topic and scene, the speaker produces an utterance."
  (formulate topic :cxn-inventory (grammar speaker) :agent speaker :scene scene))

(defmethod formulate ((topic crs-conventionality-entity-set) &key cxn-inventory
                      agent scene (silent nil) (n 1))
  "Produce utterance based on agent, topic and scene."
  ;; Store topic, agent and scene in the blackboard of the cxn-inventory
  (set-data (blackboard cxn-inventory) :topic topic)
  (set-data (blackboard cxn-inventory) :agent agent)
  (set-data (blackboard cxn-inventory) :scene scene)

  (setf (topic agent) topic)

  (unless silent (notify experiment-framework::routine-conceptualisation-started topic agent scene))

  (let* ((solution-and-cip (multiple-value-list (fcg-apply-with-n-solutions (processing-cxn-inventory cxn-inventory)
                                                                            (create-initial-structure topic
                                                                                                      (get-configuration cxn-inventory :create-initial-structure-mode-formulation)
                                                                                                      :scene scene)
                                                                            '-> n
                                                                            :notify (not silent))))
         (solution-node (first solution-and-cip))
         (cip (second solution-and-cip)))
    
    (unless silent (notify experiment-framework::routine-conceptualisation-finished cip solution-node agent))

    ;; If no solution is found, start invention and set utterance in speaker
    (if (not (succeeded-nodes cip))
      (progn
        (unless silent (notify experiment-framework::meta-conceptualisation-started topic agent scene))
        (multiple-value-bind (cxn fix)
            (fcg::invent cip agent topic scene)
          (unless silent (notify experiment-framework::meta-conceptualisation-finished fix agent))
          (setf (utterance agent) (render (car-resulting-cfs (first (get-data (blackboard fix) 'fcg::fixed-cars)))
                                            (get-configuration (grammar agent) :render-mode)))))
      (progn 
        (setf (utterance agent) (render (car-resulting-cfs (fcg:cipn-car (first solution-node)))
                                        (get-configuration (grammar agent) :render-mode)))
        (setf (applied-constructions agent) (applied-constructions (first solution-node)))))))




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

(defmethod cip-goal-test ((node cip-node) (mode (eql :topic-retrieved)))
  "Checks whether the extracted meaning leads to the topic by evaluating irl program."
  (let* ((irl-program (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node)))))
         (topic (first (crs-conventionality::entities (find-data (blackboard (construction-inventory node)) :topic))))
         (primitive-inventory (find-data (blackboard (construction-inventory node)) :primitive-inventory))
         (ontology (find-data (blackboard (construction-inventory node)) :ontology))
         (target-var (irl::get-target-var irl-program))
         (irl-solution (first (irl::evaluate-irl-program irl-program ontology :primitive-inventory primitive-inventory :n 1)))
         (computed-target (irl::value (find target-var irl-solution :key #'irl::var)))
         (success (irl::equal-entity topic computed-target)))
    success))



    
       #| for fix-cxn-inventory = (fix-cxn-inventory solution-state)
        do (set-configuration fix-cxn-inventory (if (eql (direction cip) '<-) :parse-goal-tests :production-goal-tests) (list :gold-standard))
           (let* ((repair-solution-node (fcg-apply (processing-cxn-inventory fix-cxn-inventory) (initial-cfs cip) (direction cip)))
                  (fixed-cars (mapcar #'cipn-car (reverse (upward-branch repair-solution-node :include-initial nil)))))
             (when (find 'succeeded (statuses repair-solution-node))
               (push (make-instance 'invention-fix
                                    :anti-unification-state solution-state
                                    :repair repair
                                    :problem problem
                                    :base-cxns (remove nil (mapcar #'base-cxn (upward-branch solution-state)))
                                    :fix-constructions (constructions-list fix-cxn-inventory)
                                    :fix-categories (categories fix-cxn-inventory)
                                    :fix-categorial-links (links fix-cxn-inventory)
                                    :fixed-cars fixed-cars
                                    :speech-act speech-act
                                    :cip cip)
                     fixes)))
        finally
          ;; Set fix slot of the fix-constructions before returning the fixes
          (mapcar #'(lambda (fix)
                      (loop for fix-cxn in (fix-constructions fix)
                              do (setf (attr-val fix-cxn :fix) fix))) fixes)
          
          (return (select-fixes fixes (get-configuration (construction-inventory cip) :fix-selection-mode)))|#

    





#|
    
    (setf solution-node (best-solution cip (get-configuration cxn-inventory :best-solution-mode))) ; can be nil

    (if (and solution-node
             (gold-standard-solution-p (car-resulting-cfs (cipn-car solution-node)) speech-act (direction cip) (configuration cxn-inventory)))
      (set-data cip :best-solution-matches-gold-standard t)
      (set-data cip :best-solution-matches-gold-standard nil))
    
    (unless silent (notify routine-comprehension-finished solution-node cip))

    (let* ((node-and-fixes-from-learning (when learn
                                           (multiple-value-list (learn solution-node
                                                                       cip
                                                                       consolidate
                                                                       (get-configuration cxn-inventory :learning-mode) :silent silent))))
           (solution-node-after-learning (first node-and-fixes-from-learning))
           (applied-fixes (second node-and-fixes-from-learning)))

    
      (when (and solution-node align)
        (align solution-node cip (get-configuration cxn-inventory :alignment-mode)))

      ;; Return
      (cond ((and solution-node (not applied-fixes))
             (values (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car solution-node))))
                     solution-node
                     cip))
            ((and (not solution-node) (not applied-fixes))
             (values nil nil cip))
            ((and solution-node-after-learning applied-fixes)
             (values (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car solution-node-after-learning))))
                     solution-node-after-learning
                     cip))
            (t
             (warn "Fixes were applied, but did not yield a solution.")
             (values nil nil cip)))))

|#
#|
  
  (let ((initial-cfs (create-initial-structure 
		      meaning 
		      (get-configuration construction-inventory :create-initial-structure-mode))))
    (unless silent (notify produce-all-started n meaning construction-inventory))

    (set-data (blackboard construction-inventory) :input meaning)
    
    (multiple-value-bind (solutions cip)
        (if n
          (fcg-apply-with-n-solutions construction-inventory initial-cfs '-> n
                                      :notify (not silent))
          (fcg-apply-exhaustively construction-inventory initial-cfs '->
                                  :notify (not silent)))
      (let ((utterances
             (mapcar #'(lambda(solution)
                         (or (find-data (goal-test-data solution) 'utterance)
                             (render 
                              (car-resulting-cfs (cipn-car solution)) 
                              (get-configuration construction-inventory :render-mode)
                              :node solution)))
                     solutions)))
        (unless silent (notify produce-all-finished utterances))
        (values utterances solutions cip)))))

(formulate
 |#