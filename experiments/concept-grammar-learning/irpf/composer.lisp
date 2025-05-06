;;;; composer.lisp

(in-package :clg)

;; ------------
;; + Composer +
;; ------------

(defparameter *max-irl-program-length-per-challenge-level*
  '((1 . 8) (2 . 25) (3 . 25)))

(define-event composer-solution-found
  (composer irl::chunk-composer)
  (solution chunk-evaluation-result))


(defun make-default-composer (agent target-category
                                    &key
                                    partial-program
                                    max-program-length)
  (let* ((current-challenge-level (get-configuration agent :current-challenge-level))
         (max-irl-program-length (if max-program-length max-program-length
                                   (rest (assoc current-challenge-level
                                                *max-irl-program-length-per-challenge-level*))))
         (target-category-type (if (numberp target-category)
                                 'integer
                                 (type-of target-category)))
         ;; initial chunk
         (initial-chunk (make-instance 'chunk
                                       :id 'initial
                                       :score 0.5
                                       :target-var `(?answer . ,target-category-type)
                                       :open-vars `((?answer . ,target-category-type))))
         ;; when partial bindings available, add
         ;; :check-bindings to the check chunk
         ;; evaluation result modes
         (check-chunk-evaluation-result-modes (append (when (get-configuration agent :composer-force-shape-category)
                                                        '(:at-least-one-shape-category)
                                                        (list :check-bindings))))
         (composer-primitive-inventory
          (let ((copy (copy-object (available-primitives agent))))
            ;; some extreme optimisations that are CLEVR specific
            ;; also, they will not work for level 3 questions!
            (set-configuration copy :node-tests
                               (append (get-configuration copy :node-tests)
                                       '(:remove-clevr-incoherent-filter-groups
                                         :remove-clevr-filter-permutations)))
            copy))
         ;; make the chunk composer
         (composer
          (make-chunk-composer :topic target-category
                               :meaning partial-program
                               :initial-chunk initial-chunk
                               :chunks (composer-chunks agent)
                               :ontology (ontology agent)
                               :primitive-inventory composer-primitive-inventory
                               :configurations `((:max-irl-program-length . ,max-irl-program-length)
                                                 (:chunk-node-tests ;; limit the length of the irl program
                                                  :restrict-irl-program-length
                                                  :check-duplicate ;; no duplicates
                                                  :clevr-primitive-occurrence-count
                                                  )
                                                 ;; default expand mode
                                                 (:chunk-expansion-modes :combine-program)
                                                 ;; default node cost
                                                 (:node-cost-mode . :short-programs-with-few-primitives-and-open-vars)
                                                 ;; remove unwanted chunk eval results
                                                 (:chunk-evaluation-goal-tests ,@check-chunk-evaluation-result-modes)))))
    composer))

(defmethod compose-program ((agent clevr-learning-learner)
                            target-category utterance
                            (strategy (eql :store-past-scenes))
                            &key partial-program)
  (let* ((composer (make-default-composer agent
                                          target-category
                                          :partial-program partial-program))
         (utterance-hash-key (sxhash utterance))
         (past-scenes-with-same-utterance (gethash utterance-hash-key (memory agent)))
         (composer-solution (if past-scenes-with-same-utterance
                              (compose-until composer
                                             (lambda (solution idx) (check-past-scenes solution 
                                                                                       idx
                                                                                       past-scenes-with-same-utterance
                                                                                       agent)))
                              (first (get-next-solutions composer)))))
    (when composer-solution
      (notify composer-solution-found composer composer-solution))
    composer-solution))

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

;; + store past scenes +
(define-event check-samples-started
  (list-of-samples list)
  (solution-index number))

(defun check-past-scenes (solution solution-index list-of-samples agent)
  "A sample is a triple of (context-id utterance answer). The irl-program
  of the evaluation result has to return the correct answer for all samples
  of the same utterance."
  (let* ((current-clevr-context (find-data (ontology agent) 'clevr-context))
         (irl-program (append (irl-program (chunk solution))
                              (bind-statements solution)))
         (success t))
    (notify check-samples-started list-of-samples solution-index)
    ;; check the list of samples
    (setf success
          (loop for (scene-index . stored-answer) in list-of-samples
                always (check-past-scene scene-index stored-answer irl-program agent)))
    ;; afterwards, restore the data for the current scene
    (set-data (ontology agent) 'clevr-context current-clevr-context)
    success))

(defun check-past-scene (scene-index answer irl-program agent)
  (let* ((world (world (experiment agent)))
         (path (nth scene-index (scenes world)))
         (scene (load-clevr-scene path)))
    (set-data (ontology agent) 'clevr-context scene)
    (let* ((all-solutions
            (evaluate-irl-program irl-program (ontology agent) :silent t
                                  :primitive-inventory (available-primitives agent)))
           (solution
            (when (length= all-solutions 1)
              (first all-solutions)))
           (found-answer
            (when solution
              (get-target-value irl-program solution))))
      (equal-entity found-answer answer))))