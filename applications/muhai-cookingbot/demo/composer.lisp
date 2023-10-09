(in-package :muhai-cookingbot)

(defun connect-accessible-entities (meaning-network)
  (let ((ontological-vectors (make-ontology-vectors))
        (composer
         (make-chunk-composer
          :topic nil :meaning nil
          :initial-chunk (create-chunk-from-irl-program meaning-network)
          :chunks (mapcar #'create-chunk-from-primitive (primitives-list *irl-primitives*))
          :configurations '((:chunk-expansion-modes :link-open-variables)
                            (:node-cost-mode . :best-linked-chunk)
                            (:chunk-evaluation-score-mode . :uniform)
                            (:chunk-node-tests :restrict-search-depth :check-duplicate :valid-irl-program)
                            (:chunk-evaluation-goal-tests :duplicate-solutions))
          :ontology nil
          :primitive-inventory *irl-primitives*)))
    (set-data composer :ontological-vectors ontological-vectors)
    (let ((solution (first (get-next-solutions composer))))
      (irl-program (chunk solution)))))