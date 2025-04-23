;;;; goal-tests.lisp

(in-package :cgl)

(defmethod cip-goal-test ((node cip-node) (mode (eql :correct-interpretation)))
  "Checks whether the extracted meaning can be correctly interpreted.
   When arriving at this goal test, the other goal tests have already succeeded.
   Thus, we know the meaning is fully connected.
   We get all necessary information from the blackboard of the cxn-inventory."
  (let* ((irl-program
          (extract-meanings
           (left-pole-structure
            (car-resulting-cfs
             (cipn-car node)))))
         (ontology
          (find-data (blackboard (construction-inventory node)) :ontology))
         (primitive-inventory
          (find-data (blackboard (construction-inventory node)) :primitive-inventory))
         (ground-truth-topic
          (find-data (blackboard (construction-inventory node)) :ground-truth-topic))
         (all-irl-solutions
          (evaluate-irl-program irl-program ontology
                                :primitive-inventory
                                primitive-inventory))
         (computed-topic nil)
         (success nil))
    ;; store the computed topic in the goal-test data (to avoid recomputing it later)
    ;; and return t if the computed topic is correct!
    (when (length= all-irl-solutions 1)
      (setf computed-topic (get-target-value irl-program (first all-irl-solutions)))
      (setf success (equal-entity computed-topic ground-truth-topic)))
    (set-data (goal-test-data node) :computed-topic computed-topic)
    (set-data (goal-test-data node) :interpretation-success success)
    ;; when not successful, enqueue the node again so it can be diagnosed
    ;; when not successful, write this in the blackboard of the initial node
    ;; for more efficient diagnostics
    (unless success
      (push 'fcg::goal-test-failed (statuses node))
      (set-data (initial-node node) :some-interpretation-failed t))
    success))