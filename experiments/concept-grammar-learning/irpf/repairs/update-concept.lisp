(in-package :clg)

;; ---------------------------------------
;; + Repair: UPDATE CONCEPT TILL SUCCESS +
;; ---------------------------------------

;; This repair is applied when the 

;(second (agents *experiment*))
;(define-event update-concept-repair-started)
;(define-event update-concept-repair)

(defclass update-concept (clevr-learning-repair)
  ((trigger :initform 'fcg::new-node)))


(defun update-concept-and-evaluate-irl-program (irl-program primitive-inventory ontology ground-truth-topic)
  ;; only works for count primitive
  (let* ((target-var (irl::get-target-var irl-program))
         (predicate-with-target-var (find target-var irl-program :key #'second)))
    (setf (second predicate-with-target-var) ground-truth-topic))
  (evaluate-irl-program irl-program ontology :primitive-inventory primitive-inventory))

(defmethod repair ((repair update-concept)
                   (problem failed-interpretation-problem)
                   (node cip-node) &key
                   &allow-other-keys)
  "Repair the failed utterance problem by updating a concept repeatedly."
  (loop with ground-truth-topic = (find-data (blackboard (construction-inventory node)) :ground-truth-topic)
        with primitive-inventory = (find-data (blackboard (construction-inventory node)) :primitive-inventory)
        with ontology = (find-data (blackboard (construction-inventory node)) :ontology)
        with potential-nodes = (sort (find-data node :potential-update-concept-nodes) #'> :key #'priority)
        for potential-node in potential-nodes
        for irl-program = (find-data (goal-test-data potential-node) :irl-program)
        for concepts = (loop for predicate in irl-program
                             when (and (equal (first predicate) 'bind)
                                       (not (equal (second predicate) 'attribute-category)))
                               collect (fourth predicate))
        ;; todo
        for copy-concepts = (copy-object (get-data ontology 'concepts))
        for solution-p = (update-concept-and-evaluate-irl-program irl-program primitive-inventory ontology ground-truth-topic)
#|      for solution-p = (loop with max-iterations = 100
                               with current-iteration = 0
                               while (< current-iteration max-iterations)
                               for computed-topic = (update-concept-and-evaluate-irl-program irl-program
                                                                                             primitive-inventory
                                                                                             ontology
                                                                                             concepts)
                               for success-p = (equal-entity computed-topic ground-truth-topic)
                               do (incf current-iteration)
                               if success-p
                                 do (loop-finish)
                               finally (return t))|#
        if solution-p
          do (print "we happy")
        else
          do (print "sad")
          
          ))


        ;; todo
      #|(make-instance 'fcg::concept-fix
                     :repair repair
                     :problem problem
                     :restart-data updated-concepts)|#

      


      
      #|(let ((constructions-and-th-links (create-item-based-cxn-substitution problem
                                                                            node
                                                                            reconstructed-intention)))
        (when constructions-and-th-links
          (make-instance 'fcg::cxn-fix
                         :repair repair
                         :problem problem
                         :restart-data constructions-and-th-links))))))|#

      

#|
(defmethod handle-fix ((fix fcg::update-concept-fix) (repair clevr-learning-repair)
                       (problem problem) (node cip-node))
  (push fix (fixes problem))
  (let ((updated-concepts (restart-data fix)))
    (loop for updated-concept in updated-concepts
            .. ; search concept
          do (setf old-concept updated-concept))))



(in-package :fcg)

(defclass update-concept-fix (fix)
  ()
  (:documentation "A fix class for fixes that apply a construction and return the cxn-application-result"))|#