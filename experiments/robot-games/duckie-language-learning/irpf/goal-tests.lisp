(in-package :duckie-language-learning)

;; ------------------------------------------
;; + CIP Goal test - correct interpretation +
;; ------------------------------------------
(defmethod cip-goal-test ((node cip-node) (mode (eql :correct-interpretation)))
  "Checks whether the extracted meaning can be correctly interpreted.
   When arriving at this goal test, the other goal tests have already succeeded.
   Thus, we know the meaning is fully connected.
   We get all necessary information from the blackboard of the cxn-inventory."
  (if (some-regular-repair-in-tree node)
    ;; CASE 1: a repair has occured (that is not a add-categorial-link repair)
    t
    ;; CASE 2: no repair - then evaluate and compare against answer
    (let* ((irl-program (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node)))))
           (ontology (find-data (blackboard (construction-inventory node)) :ontology))
           (primitive-inventory (find-data (blackboard (construction-inventory node)) :primitive-inventory))
           (owner (find-data (blackboard (construction-inventory node)) :owner))
           (solutions (evaluate-irl-program irl-program
                                            ontology
                                            :silent t
                                            :primitive-inventory primitive-inventory)))
      (if (find-data (blackboard (grammar owner)) :answer-correct?)
        (set-data (blackboard (grammar owner)) :fcg-solution? t)
        (cond ((length= solutions 1)
               (if (and (not (find-data (blackboard (grammar owner)) :guessed))
                        (not (find-data (blackboard (grammar owner)) :ground-truth-topic)))
                 ;; CASE 1: agent has not guessed yet
                 (let* ((computed-topic (get-target-value irl-program (first solutions)))
                        (answer-correct? (confirm-answer computed-topic))
                        (correct-answer (if answer-correct? (id computed-topic) (ask-correct-answer owner))))
                   (set-data (blackboard (grammar owner)) :guessed t)
                   (set-data (blackboard (grammar owner)) :answer-correct? answer-correct?)
                   (if answer-correct?
                     t
                     (progn
                       (push 'fcg::goal-test-failed (statuses node))
                       (set-data (initial-node node) :some-interpretation-failed t)
                       nil)))
                 ;; CASE 2: agent has already guessed, don't try again
                 (let* ((computed-topic (get-target-value irl-program (first solutions)))
                        (correct-answer (find-data (blackboard (construction-inventory node)) :ground-truth-topic))
                        (answer-correct? (equal-entity computed-topic correct-answer)))
                   (set-data (blackboard (grammar owner)) :answer-correct? answer-correct?)
                   (if answer-correct?
                     t
                     (progn
                       (push 'fcg::goal-test-failed (statuses node))
                       (set-data (initial-node node) :some-interpretation-failed t)
                       nil)))))
              (t
               (let ((correct-answer (ask-correct-answer owner)))
                 (push 'fcg::goal-test-failed (statuses node))
                 (set-data (initial-node node) :some-interpretation-failed t)
                 nil)))))))

(defun get-target-value (irl-program list-of-bindings)
  (let* ((target-variable (get-target-var irl-program))
         (target-binding (find target-variable list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))
