;;;; node-test.lisp

(in-package :fcg)

(defmethod cip-node-test ((node cip-node) (mode (eql :connected-structure-for-morph)))
  "If a hashed-morph cxn applied in formulation,
   check if the transient structure is connected."
  (if (eql (direction (cip node)) '->)
    (let* ((applied-cxn-label (attr-val (first (applied-constructions node)) :label)))
      (if (and (listp applied-cxn-label) (find 'hashed-morph applied-cxn-label))
        (let* ((left-pole (left-pole-structure (car-resulting-cfs (cipn-car node))))
               (connected? (fcg::connected-syntactic-structure
                            left-pole
                            :grammar-hierarchy-features
                            (fcg::hierarchy-features (construction-inventory node)))))
          (or connected?
              (progn
                (set-data (goal-test-data node) 'dependencies-realized left-pole)
                nil)))
        t))
    t))