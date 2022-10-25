(in-package :irl)

;; ############################################################################
;; goal-tests
;; ----------------------------------------------------------------------------

(defgeneric pip-run-goal-tests (node primitive-inventory-processor)
  (:documentation "Runs all goad tests on the given node. A goal test should return t or nil.
                   This function returns t if all goal tests succeed."))

(defmethod pip-run-goal-tests ((node pip-node) (pip primitive-inventory-processor))
  (setf (goal-test-data node) (make-blackboard))
  (when (and (primitive-evaluated node)
             (loop for mode in (get-configuration pip :goal-tests)
                   always (pip-goal-test node mode)))
    (push 'succeeded (statuses node))
    (push node (succeeded-nodes pip))
    t))

;; ---------------------------------------------------------
;; no primitives remaining

(defmethod pip-goal-test ((node pip-node) (mode (eql :no-primitives-remaining)))
  (null (primitives-remaining node)))

;; ---------------------------------------------------------
;; all variables bound

(defmethod pip-goal-test ((node pip-node) (mode (eql :all-variables-bound)))
  (when (bindings node)
    (loop for binding in (bindings node)
          never (null (value binding)))))