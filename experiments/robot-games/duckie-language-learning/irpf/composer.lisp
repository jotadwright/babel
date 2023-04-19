(in-package :duckie-language-learning)

;; ---------------------
;; + Intention reading +
;; ---------------------

(defun make-default-composer (agent target-category
                              &key partial-program)
  (let* ((target-category-type
          (type-of target-category))
         ;; initial chunk
         (initial-chunk
          (make-instance 'chunk :id 'initial
                         ;:irl-program `((scan-world ?world))
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
                       (:check-node-modes ;; limit the length of the irl program
                        :limit-irl-program-length
                        ;; no duplicates
                        :check-duplicate
                        ;; no predicates with multiple times
                        ;; the same variable
                        ;:no-circular-primitives
                        ;; meaning has to be fully connected
                        :fully-connected-meaning
                        )))))

(defmethod compose-program ((agent duckie-language-learning-agent)
                            target-category &key partial-program)
  (let* ((composer
          (make-default-composer agent target-category
                                 :partial-program `((scan-world ?world)))) ;; TODO: remove assumption?
         (solution (first (get-next-solutions composer))))
    (when solution 
      (append (irl::bind-statements solution)
              (irl::irl-program (chunk solution))))))
