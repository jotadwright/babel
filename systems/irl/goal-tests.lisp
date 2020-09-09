(in-package :irl)

;; ############################################################################
;; goal-tests
;; ----------------------------------------------------------------------------

(defgeneric run-goal-tests (node primitive-inventory)
  (:documentation "Runs all goad tests on the given node. A goal test should return t or nil.
                   This function returns t if all goal tests succeed."))

(defmethod run-goal-tests ((node irl-program-processor-node)
                           (primitive-inventory primitive-inventory))
  (loop for mode in (get-configuration primitive-inventory :goal-tests)
        always (goal-test node mode)))


(defgeneric goal-test (node mode)
  (:documentation "Runs the goal test specified by mode on the node"))

;; ---------------------------------------------------------
;; no primitives remaining

(defmethod goal-test ((node irl-program-processor-node)
                      (mode (eql :no-primitives-remaining)))
  (null (primitives-remaining node)))

;; ---------------------------------------------------------
;; all variables bound

(defmethod goal-test ((node irl-program-processor-node)
                      (mode (eql :all-variables-bound)))
  (loop for binding in (bindings node)
        never (null (value binding))))