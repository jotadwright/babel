(in-package :duckie-language-learning)

(defun make-default-composer (agent target-category
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
                       (:check-node-modes ;; limit the length of the irl program
                        :limit-irl-program-length
                        ;; no duplicates
                        ;:check-duplicate
                        ;; no predicates with multiple times
                        ;; the same variable
                        ;:no-circular-primitives
                        ;; meaning has to be fully connected
                        ;:fully-connected-meaning
                        )))))

(defmethod compose-program ((agent duckie-language-learning-agent)
                            target-category &key partial-program)
  (let* ((composer
          (lll::make-default-composer agent target-category
                                 :partial-program partial-program))
         (solution (first (get-next-solutions composer))))
    (when solution 
      (append (irl::bind-statements solution)
              (irl::irl-program (chunk solution))))))

;(setf *agent* (make-instance 'duckie-language-learning-agent))
;(compose-program *agent*  (find "2" (get-data (ontology *agent*) 'numbers) :key #'value :test #'string=))
;(compose-program *agent*  (find "4" (get-data (ontology *agent*) 'numbers) :key #'value :test #'string=))
;(compose-program *agent*  (find "yellow" (get-data (ontology *agent*) 'colors) :key #'value :test #'string=)
;                 :partial-program '((bind degree-category ?degree 270)
;                                    (rotate-and-scan ?brol ?degree)))