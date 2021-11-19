(in-package :muhai-cookingbot)

(defmethod apply-heuristic ((node cip-node) (mode (eql :ontological-distance)))
  "Returns a normalisation of the number of units matched by the cxn."
  (let* ((cxn-inventory (construction-inventory node))
         (bindings (car-second-merge-bindings (cipn-car node)))
         (ontological-class-utterance (cdr (assoc "ONTOLOGICAL-CLASS-UTTERANCE" ;;brittle!!
                                                  bindings :key #'get-base-name :test #'string=)))
         (ontological-class-world (cdr (assoc "ONTOLOGICAL-CLASS-WORLD"
                                              bindings :key #'get-base-name :test #'string=))))
    (if (and ontological-class-utterance ontological-class-world)
      (let* ((utterance-vector (ontological-vector ontological-class-utterance cxn-inventory))
             (world-vector (ontological-vector ontological-class-world cxn-inventory))
             (similarity-score (cosine-similarity utterance-vector world-vector)))
        (- (- 1 similarity-score)))
        0)))

