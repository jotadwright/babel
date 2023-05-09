(in-package :duckie-language-learning)

;; ------------------------------------------
;; + CIP Goal test - correct interpretation +
;; ------------------------------------------
(defmethod cip-goal-test ((node cip-node) (mode (eql :correct-interpretation)))
  "Checks whether the extracted meaning can be correctly interpreted.
   When arriving at this goal test, the other goal tests have already succeeded.
   Thus, we know the meaning is fully connected.
   We get all necessary information from the blackboard of the cxn-inventory."
  (if (some-applied-repair-in-tree node)
    t
    (let* ((irl-program (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node)))))
           (ontology (find-data (blackboard (construction-inventory node)) :ontology))
           (primitive-inventory (find-data (blackboard (construction-inventory node)) :primitive-inventory))
           (owner (find-data (blackboard (construction-inventory node)) :owner))
           (solutions (evaluate-irl-program irl-program
                                            ontology
                                            :silent t
                                            :primitive-inventory primitive-inventory)))
      (set-data (blackboard (grammar owner)) :fcg-solution? t)
      (cond ((length= solutions 1)
             (let* ((computed-topic (get-target-value irl-program (first solutions)))
                    (answer-correct? (confirm-answer computed-topic))
                    (correct-answer (if answer-correct? (id computed-topic) (ask-correct-answer owner))))
               (set-data (blackboard (grammar owner)) :answer-correct? answer-correct?)
               (if answer-correct?
                 t
                 (progn
                   (push 'fcg::goal-test-failed (statuses node))
                   (set-data (initial-node node) :some-interpretation-failed t)
                   nil))))
            (t
             (let ((correct-answer (ask-correct-answer owner)))
               (push 'fcg::goal-test-failed (statuses node))
               (set-data (initial-node node) :some-interpretation-failed t)
               nil))))))

(defun get-target-value (irl-program list-of-bindings)
  (let* ((target-variable (get-target-var irl-program))
         (target-binding (find target-variable list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))
