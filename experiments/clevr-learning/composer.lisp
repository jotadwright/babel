;;;; composer.lisp

(in-package :clevr-learning)

;; ------------
;; + Composer +
;; ------------

(defun make-default-composer (agent target-category)
  ;; make the chunk composer
  (make-chunk-composer
   :topic target-category
   ; if partial program is available, this can be passed along
   ; using :meaning initarg
   :initial-chunk (make-instance 'chunk :id 'initial
                                 :target-var `(?answer . ,(type-of target-category))
                                 :open-vars `((?answer . ,(type-of target-category))))
   :chunks (composer-chunks agent)
   :ontology (ontology agent)
   :primitive-inventory (available-primitives agent)
   :configurations '((:max-search-depth . 25)
                     ;; when providing bind statements,
                     ;; remove the :clevr-open-vars from
                     ;; the :check-node-modes
                     (:check-node-modes :check-duplicate 
                                        :clevr-primitive-occurrence-count                              
                                        :clevr-context-links
                                        :clevr-filter-group-length
                                        :clevr-open-vars)
                     (:expand-chunk-modes :combine-program)
                     (:node-rating-mode . :clevr-node-rating)
                     (:check-chunk-evaluation-result-modes
                      :clevr-coherent-filter-groups))
   :primitive-inventory-configurations '((:node-tests :no-duplicate-solutions))))

;; + compose-until +
(defun compose-until (composer fn)
  "Generate composer solutions until the
   function 'fn' returns t on the solution"
  (loop with solution = nil
        while (not solution)
        for solutions = (get-next-solutions composer)
        do (setf solution
                 (loop for s in solutions
                       for i from 1
                       when (funcall fn s i)
                       return s))
        finally
        (return solution)))

(defun check-past-programs (solution solution-index list-of-past-programs agent)
  (declare (ignorable solution-index agent))
  (let ((solution-irl-program
         (append (irl-program (chunk solution))
                 (bind-statements solution))))
    (loop for past-program in list-of-past-programs
          never (equivalent-irl-programs? past-program solution-irl-program))))


(defmethod compose-program-update (agent target-category
                                         (strategy (eql :minimal+store-past-programs)))
  (let* ((composer (make-default-composer agent target-category))
         (past-programs (rest
                         (assoc (utterance agent)
                                (find-data agent 'past-programs)
                                :test #'string=))))
    (compose-until
     composer (lambda (solution idx)
                (check-past-programs solution idx past-programs agent)))))

(define-event check-samples-started
  (list-of-samples list)
  (solution-index number))              

(defun check-all-samples (solution solution-index list-of-samples agent)
  "A sample is a triple of (context-id utterance answer). The irl-program
  of the evaluation result has to return the correct answer for all samples
  of the same utterance."
  (let ((clevr-context (find-data (ontology agent) 'clevr-context))
        (irl-program (append (irl-program (chunk solution))
                             (bind-statements solution)))
        (world (world (experiment agent)))
        (success t))
    (notify check-samples-started list-of-samples solution-index)
    (setf success
          (loop for sample in list-of-samples
                for context
                = (let* ((path (nth (first sample) (scenes world)))
                         (scene (load-clevr-scene path)))
                    (set-data (ontology agent) 'clevr-context scene)
                    scene)
                for solutions
                = (evaluate-irl-program irl-program (ontology agent)
                                        :silent t :primitive-inventory (available-primitives agent))
                for solution = (when (length= solutions 1) (first solutions))
                for found-answer = (when solution (get-target-value irl-program solution))
                for correct = (equal-entity found-answer (third sample))
                always correct))
    (set-data (ontology agent) 'clevr-context clevr-context)
    success))

(defmethod compose-program-update (agent target-category
                                         (strategy (eql :minimal+store-past-scenes)))
  (let* ((composer (make-default-composer agent target-category))
         (all-samples (find-all (utterance agent) (find-data agent 'samples)
                                :key #'second :test #'string=)))
    (compose-until
     composer (lambda (s idx)
                (check-all-samples s idx all-samples agent)))))
              
(defmethod compose-new-program (agent target-category)
  (let ((composer (make-default-composer agent target-category)))
    (random-elt (get-next-solutions composer))))