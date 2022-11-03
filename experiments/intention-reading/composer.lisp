;;;; composer.lisp

(in-package :intention-reading)

;; ------------
;; + Composer +
;; ------------

(defparameter *max-irl-program-length-per-challenge-level*
  '((1 . 8) (2 . 25) (3 . 25)))


(defun make-default-composer (agent target-category
                              &key partial-program
                              max-program-length)
  (let* ((current-challenge-level
          (get-configuration agent :current-challenge-level))
         (max-irl-program-length
          (if max-program-length max-program-length
            (rest (assoc current-challenge-level
                         *max-irl-program-length-per-challenge-level*))))
         (target-category-type
          (if (numberp target-category)
            'integer (type-of target-category)))
         ;; initial chunk
         (initial-chunk
          (make-instance 'chunk :id 'initial :score 0.5
                         :target-var `(?answer . ,target-category-type)
                         :open-vars `((?answer . ,target-category-type))))
         ;; when partial bindings available, add
         ;; :check-bindings to the check chunk
         ;; evaluation result modes
         (check-chunk-evaluation-result-modes
          (when (get-configuration agent :composer-force-shape-category)
            '(:at-least-one-shape-category)))
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
          (make-chunk-composer
           :topic target-category
           :meaning partial-program
           :initial-chunk initial-chunk
           :chunks (composer-chunks agent)
           :ontology (ontology agent)
           :primitive-inventory composer-primitive-inventory
           :configurations `((:max-irl-program-length . ,max-irl-program-length)
                             (:chunk-node-tests ;; limit the length of the irl program
                                                :restrict-irl-program-length
                                                ;; no duplicates
                                                :check-duplicate
                                                ;; unused node tests:
                                                ;:no-circular-primitives
                                                ;:fully-connected-meaning
                                                ;:one-target-var
                                                ;:clevr-primitive-occurrence-count
                                                ;:clevr-context-links
                                                ;:clevr-filter-group-length
                                                ;:clevr-open-vars
                                                )
                             ;; default expand mode
                             (:chunk-expansion-modes :combine-program)
                             ;; default node cost
                             (:node-cost-mode . :short-programs-with-few-primitives-and-open-vars)
                             ;(:node-cost-mode . :clevr-node-cost)
                             ;; remove unwanted chunk eval results
                             (:chunk-evaluation-goal-tests
                              ,@check-chunk-evaluation-result-modes)))))
    composer))

;; + compose-until +
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

(defun compose-minimize (composer fn)
  "Run 'fn' on the composer solutions and choose
   the one that minimizes the result. If there is
   no result, compose the next solution."
  (loop with the-solution = nil
        for next-solutions = (get-next-solutions composer)
        do (loop with minimum-solution = nil
                 with minimum-value = nil
                 for solution in next-solutions
                 for i from 1
                 for result = (funcall fn solution i)
                 when (and result (or (null minimum-solution) (< result minimum-value)))
                 do (setf minimum-solution solution)
                 (setf minimum-value result)
                 finally (setf the-solution minimum-solution))
        until (or (null next-solutions)
                  (not (null the-solution)))
        finally (return the-solution)))
                

;; + store past scenes +
(define-event check-samples-started
  (list-of-samples list)
  (solution-index number))

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

;; + store past programs +
(define-event check-programs-started
  (list-of-samples list)
  (solution-index number))

(defun check-past-programs (solution solution-index list-of-samples)
  "A sample is a previous irl program. The irl-program
  of the evaluation result has to be different from
  all previous irl programs."
  (when solution
    (let ((irl-program
           (append (irl-program (chunk solution))
                   (bind-statements solution))))
      (notify check-programs-started list-of-samples solution-index)
      ;; return t if there is never an equivalent program
      (loop for previous-program in list-of-samples
            never (equivalent-irl-programs? irl-program previous-program)))))
  

(define-event composer-solution-found
  (composer irl::chunk-composer)
  (solution chunk-evaluation-result))

(defmethod compose-program ((agent clevr-learning-learner)
                            target-category utterance
                            (strategy (eql :store-past-scenes))
                            &key partial-program)
  (let* ((composer
          (make-default-composer agent target-category
                                 :partial-program partial-program))
         (utterance-hash-key
          (sxhash utterance))
         (past-scenes-with-same-utterance
          (gethash utterance-hash-key (memory agent)))
         (composer-solution
          (if past-scenes-with-same-utterance
            (compose-until
             composer (lambda (solution idx)
                        (check-past-scenes
                         solution idx
                         past-scenes-with-same-utterance
                         agent)))
            (first (get-next-solutions composer)))))
    (when composer-solution
      (notify composer-solution-found composer composer-solution))
    composer-solution))

(defmethod compose-program ((agent clevr-learning-learner)
                             target-category utterance
                             (strategy (eql :store-past-programs))
                             &key partial-program)
  (let* ((composer
          (make-default-composer agent target-category
                                 :partial-program partial-program))
         (utterance-hash-key
          (sxhash utterance))
         (past-programs-with-same-utterance
          (gethash utterance-hash-key (memory agent)))
         (composer-solution
          (if past-programs-with-same-utterance
            (compose-until
             composer (lambda (solution idx)
                        (check-past-programs
                         solution idx past-programs-with-same-utterance)))
            (first (get-next-solutions composer)))))
    (when composer-solution
      (notify composer-solution-found composer composer-solution))
    composer-solution))
