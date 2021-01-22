;;;; composer.lisp

(in-package :clevr-learning)

;; ------------
;; + Composer +
;; ------------

(defparameter *max-composer-depth-per-challenge-level*
  '((1 . 8) (2 . 25) (3 . 25)))

(defun make-default-composer (agent target-category &key (partial-program nil))
  (let* ((current-challenge-level
          (get-configuration agent :current-challenge-level))
         (max-composer-depth
          (rest (assoc current-challenge-level *max-composer-depth-per-challenge-level*)))
         (target-category-type
          #-sbcl (type-of target-category)
          #+sbcl (if (numberp target-category)
                   'integer (type-of target-category))
          ))
    ;; make the chunk composer
    (make-chunk-composer
     :topic target-category
     ; if partial program is available, this can be passed along
     :meaning partial-program
     :initial-chunk (make-instance 'chunk :id 'initial
                                   :target-var `(?answer . ,target-category-type)
                                   :open-vars `((?answer . ,target-category-type)))
     :chunks (composer-chunks agent)
     :ontology (ontology agent)
     :primitive-inventory (available-primitives agent)
     :configurations `((:max-search-depth . ,max-composer-depth)
                       ;; when providing bind statements,
                       ;; remove the :clevr-open-vars from
                       ;; the :check-node-modes
                       (:check-node-modes ,@(append '(:check-duplicate 
                                                      :clevr-primitive-occurrence-count                              
                                                      :clevr-context-links
                                                      :clevr-filter-group-length)
                                                    (unless partial-program
                                                      '(:clevr-open-vars))))
                       (:expand-chunk-modes :combine-program)
                       (:node-rating-mode . :clevr-node-rating)
                       (:check-chunk-evaluation-result-modes
                        :clevr-coherent-filter-groups))
     :primitive-inventory-configurations '((:node-tests :no-duplicate-solutions)))))

;; + compose-until +
(defun compose-until (composer fn)
  "Generate composer solutions until the
   function 'fn' returns t on the solution
   or the composer runs out of solutions"
  (loop with the-solution = nil
        for next-solutions = (get-next-solutions composer)
        do (loop for solution in next-solutions
                 for i from 1
                 when (funcall fn solution i)
                 do (progn (setf the-solution solution) (return)))
        until (or (null next-solutions)
                  (not (null the-solution)))
        finally (return the-solution)))

;; + store past programs +
(defun check-past-programs (solution solution-index list-of-past-programs agent)
  (declare (ignorable solution-index agent))
  (let ((solution-irl-program
         (append (irl-program (chunk solution))
                 (bind-statements solution))))
    (loop for past-program in list-of-past-programs
          never (equivalent-irl-programs? past-program solution-irl-program))))


(defmethod compose-program ((agent clevr-learning-learner) target-category
                            (strategy (eql :store-past-programs))
                            &key partial-program)
  (let ((composer (make-default-composer agent target-category
                                         :partial-program partial-program))
        (past-programs (rest (assoc (utterance agent)
                                    (find-data agent 'past-programs)
                                    :test #'string=))))
    (if past-programs
      (compose-until
       composer (lambda (solution idx)
                  (check-past-programs solution idx past-programs agent)))
      (random-elt (get-next-solutions composer)))))

;; + store past scenes +
(define-event check-samples-started
  (list-of-samples list)
  (solution-index number))              

(defun check-past-scenes (solution solution-index list-of-samples agent)
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

(defmethod compose-program ((agent clevr-learning-learner) target-category
                            (strategy (eql :store-past-scenes))
                            &key partial-program)
  (let ((composer (make-default-composer agent target-category
                                         :partial-program partial-program))
        (past-scenes (find-all (utterance agent) (find-data agent 'past-scenes)
                               :key #'second :test #'string=)))
    (if past-scenes
      (compose-until
       composer (lambda (solution idx)
                  (check-past-scenes solution idx past-scenes agent)))
      (random-elt (get-next-solutions composer)))))
