(in-package :duckie-language-learning)

;; ---------------------
;; + Intention reading +
;; ---------------------

(defun make-default-composer (agent
                              target-category
                              &key partial-program)
  (let* ((target-category-type
          (type-of target-category))
         ;; initial chunk
         (initial-chunk
          (make-instance 'chunk :id 'initial
                         :target-var `(?answer . ,target-category-type)
                         :open-vars `((?answer . ,target-category-type)))))
    (make-chunk-composer
     :topic target-category
     :meaning partial-program
     :initial-chunk initial-chunk
     :chunks (mapcar #'create-chunk-from-primitive
                     (irl::primitives (primitive-inventory agent)))
     :ontology (ontology agent)
     :primitive-inventory (primitive-inventory agent)
     :configurations `((:max-irl-program-length . 5)
                       (:chunk-node-tests :restrict-irl-program-length ;; limit the length of the irl program
                                           :check-duplicate ;; no duplicates
                                           ;:no-circular-primitives ;; no predicates with multiple times the same variable
                                           ;:fully-connected-meaning ;; meaning has to be fully connected
                        )
                       ;; default expand mode
                       (:chunk-expansion-modes :combine-program)
                       ;; default node cost
                       (:node-cost-mode . :short-programs-with-few-primitives-and-open-vars)))))

(defmethod compose-program ((agent duckie-language-learning-agent)
                            target-category
                            &key partial-program)
  (if (subtypep (type-of target-category) 'duckie-category)
    (notify irl::chunk-composer-started (mkstr (category target-category)))
    (notify irl::chunk-composer-started (mkstr (zone target-category)))) ;; special for MOVE-TO primitive
  (let* ((composer
          (make-default-composer agent target-category
                                 :partial-program `((scan-world ?world)))) ;; TODO: remove assumption?
         (solution (first (get-next-solutions composer))))
    (when solution 
      (append (irl::bind-statements solution)
              (irl::irl-program (chunk solution))))))
