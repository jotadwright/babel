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
                                        (get-configuration (experiment agent) :composer-node-tests)))
                                        
            copy))
         ;; make the chunk composer
         (composer (make-chunk-composer :topic target-category
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
                            target-category
                            utterance
                            (strategy (eql :standard))
                            &key partial-program)
  (let* (;; make the composer
         (composer (make-default-composer agent target-category :partial-program partial-program))
         ;; let the composer go wild
         (composer-solution (first (get-next-solutions composer))))
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
