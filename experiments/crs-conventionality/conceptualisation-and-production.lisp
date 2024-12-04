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

(defmethod formulate ((topic crs-conventionality-entity-set) &key (cxn-inventory fcg-construction-set)
                      (agent crs-conventionality-agent) (scene crs-conventionality-scene) (silent nil) (n nil))
  "Produce utterance based on agent, topic and scene."
  ;; Store topic, agent and scene in the blackboard of the cxn-inventory
  (set-data (blackboard cxn-inventory) :topic topic)
  (set-data (blackboard cxn-inventory) :agent agent)
  (set-data (blackboard cxn-inventory) :scene scene)

  ;(unless silent (notify routine-formulation-started topic agent scene))

  (let* ((cip (second (multiple-value-list (fcg-apply-with-n-solutions (processing-cxn-inventory cxn-inventory)
                                                                       (create-initial-structure topic
                                                                                                 (get-configuration cxn-inventory :create-initial-structure-mode)
                                                                                                 :scene scene)
                                                                       '-> n
                                                                       :notify (not silent)))))
         (solution-node nil))))


(defmethod create-initial-structure ((topic crs-conventionality-entity-set)
                                     (mode (eql :topic-and-scene))
                                     &key scene)
  "In formulation, the initial transient structure holds the topic and scene."
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
                               (topic ,topic)
                               (scene ,scene)))
		 :right-pole '((root))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))
    
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