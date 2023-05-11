(in-package :duckie-language-learning)

;; ---------------------
;; + Intention reading +
;; ---------------------

(defun make-default-composer (agent
                              target-category
                              &key partial-program)
  (let* ((target-category-type (type-of target-category))
         ;; heuristic to guide the search process to the answer
         (initial-chunk (make-instance 'chunk
                                       :id 'initial
                                       :target-var `(?answer . ,target-category-type)
                                       :open-vars `((?answer . ,target-category-type)))))
    (make-chunk-composer :topic target-category
                         :meaning partial-program
                         :initial-chunk initial-chunk
                         :chunks (mapcar #'create-chunk-from-primitive (irl::primitives (primitive-inventory agent))) ;; CORRECT?
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
                            utterance
                            &key partial-program)
  ;; Notify expected answer
  (if (subtypep (type-of target-category) 'duckie-category)
    (notify irl::chunk-composer-started (mkstr (category target-category)))
    (notify irl::chunk-composer-started (mkstr (zone target-category)))) ;; special for DUCKIE-AGENT-CAR

  
  (let* ((composer (make-default-composer agent target-category :partial-program partial-program))
         (utterance-hash-key (sxhash utterance))
         (past-scenes-with-utterance (gethash utterance-hash-key (memory agent)))
         (composer-solution (if past-scenes-with-utterance
                              (compose-until composer
                                             (lambda (solution idx)
                                               (check-past-scenes solution idx past-scenes-with-utterance agent)))
                              (first (get-next-solutions composer)))))
    (when composer-solution 
      (append (irl::bind-statements composer-solution)
              (irl::irl-program (chunk composer-solution))))))

;; utils

(defun compose-until (composer fn &key (max-attempts nil))
  "Generate composer solutions until the
   function 'fn' returns t on the solution
   or the composer runs out of solutions"
  (loop with the-solution = nil
        for attempt from 1
        for next-solutions = (get-next-solutions composer)
        do (loop for solution in next-solutions
                 for i from 1
                 when (funcall fn solution i)
                 do (progn (setf the-solution solution) (return)))
        until (or (null next-solutions)
                  (not (null the-solution))
                  (and max-attempts (> attempt max-attempts)))
        finally (return the-solution)))

(defun check-past-scene (world answer irl-program agent)
  (set-data (ontology agent) 'world world)
  (let* ((all-solutions (evaluate-irl-program irl-program
                                              (ontology agent)
                                              :silent t
                                              :primitive-inventory (primitive-inventory agent)))
         (solution (when (length= all-solutions 1) (first all-solutions)))
         (found-answer (when solution (get-target-value irl-program solution))))
    (equal-entity found-answer answer)))

(defun check-past-scenes (solution solution-index list-of-samples agent)
  "A sample is a triple of (context-id utterance answer). The irl-program
  of the evaluation result has to return the correct answer for all samples
  of the same utterance."
  (let* ((current-world (find-data (ontology agent) 'world))
         (irl-program (append (irl::irl-program (chunk solution))
                              (irl::bind-statements solution)))
         (success t))
    ;(notify check-samples-started list-of-samples solution-index)
    ;; check the list of samples
    (setf success (loop for (stored-world . stored-answer) in list-of-samples
                        always (check-past-scene stored-world stored-answer irl-program agent)))
    ;; afterwards, restore the data for the current scene
    (set-data (ontology agent) 'world current-world)
    success))
