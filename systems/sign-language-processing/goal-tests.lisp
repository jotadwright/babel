(in-package :slp)

(defun get-articulations (unit &optional (ignore-tags nil))
  (loop for el in (feature-value (unit-feature unit 'form ignore-tags))
       when (or (and (consp el)
		 (eql (first el) 'left-hand-articulation))
                (and (consp el)
		 (eql (first el) 'right-hand-articulation)))
       collect (third el)))

(defmethod cip-goal-test ((node cip-node) (mode (eql :no-articulations-in-root)))
  "The node is a valid solution when there is are no articulation features
left in the root unit's form predicates (comprehension only)."
  (let ((articulations-in-root (get-articulations (assoc 'root
                                       (left-pole-structure
                                        (car-resulting-cfs (cipn-car node)))))))
    (set-data (goal-test-data node) 'articulations-in-root articulations-in-root)
    (not articulations-in-root)))