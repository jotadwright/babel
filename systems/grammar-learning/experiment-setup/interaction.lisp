;;;; interaction.lisp

(in-package :grammar-learning)

;; --------------------
;; + Initialize agent +
;; --------------------

(defmethod initialize-agent ((agent clevr-learning-tutor)
                             utterance gold-standard-meaning)
  (setf (utterance agent) utterance
        (meaning agent) gold-standard-meaning
        (communicated-successfully agent) t))

(defmethod initialize-agent ((agent clevr-learning-learner)
                             utterance gold-standard-meaning)
  (setf (utterance agent) utterance
        (meaning agent) gold-standard-meaning
        (communicated-successfully agent) t
        (task-result agent) nil))

;; ---------------
;; + Interaction +
;; ---------------

(defun matches-gold-standard-meaning (node)
  "Checks whether the extracted meaning is equivalent with the gold standard meaning."
  (and (fully-expanded? node)
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (meaning (extract-meanings (left-pole-structure resulting-cfs)))
           (meaning-representation-formalism (get-configuration (construction-inventory node) :meaning-representation-formalism))
           (gold-standard-meanings (get-data resulting-cfs :meanings)))
      (when (find meaning gold-standard-meanings :test #'(lambda (m1 m2)
                                                         (equivalent-meaning-networks m1 m2 meaning-representation-formalism)))
        
        t))))

(define-event interaction-before-finished
  (utterance string) (gold-standard-meaning t))

(defun get-interaction-data (interaction)
  "retrieve the nth utterance and gold standard meaning from the dataset"
  (let* ((interaction-data (nth (- (interaction-number interaction) 1) (question-data (experiment interaction))))
         (utterance (first interaction-data))
         (gold-standard-meaning (cdr interaction-data)))
    (values utterance gold-standard-meaning)))

(defun determine-communicative-success (cipn)
  (assert (find 'SUCCEEDED (statuses cipn) :test #'string=))
  (let ((node-statuses (mappend #'statuses (cons cipn (all-parents cipn)))))
    (cond ((and (find 'add-categorial-links node-statuses :test #'string=)
                (not (loop for current-node in (traverse-depth-first (top-node (cip cipn)) :collect-fn #'identity)
                           if (and (field? (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)
                                   (not (get-data (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)))
                           do (return t))))
           t)
                
     ((find 'ADDED-BY-REPAIR node-statuses :test #'string=)
           nil)
          ((loop for current-node in (traverse-depth-first (top-node (cip cipn)) :collect-fn #'identity)
                 if (and (field? (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)
                        (not (get-data (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)))
                 do (return t))
           nil)
          (t
           t))))

(defun get-last-repair-symbol (cipn)
  (let ((node-statuses (mappend #'statuses (cons cipn (all-parents cipn)))))
    (if (not (find 'ADDED-BY-REPAIR node-statuses :test #'string=))
      (if (determine-communicative-success cipn) "." "x") ; return a dot or x in evaluation mode
      (cond ((find 'nothing->holistic node-statuses :test #'string=) "h")
            ((find 'holistic->item-based node-statuses :test #'string=) "i")
            ((find 'item-based->holistic node-statuses :test #'string=) "l")
            ((find 'holistic+item-based->item-based--substitution node-statuses :test #'string=) "z")
            ((find 'holophrase->item-based+holistic+holistic--substitution node-statuses :test #'string=) "s")
            ((find 'holophrase->item-based+holistic--addition node-statuses :test #'string=) "a")
            ((find 'holophrase->item-based+holistic+holophrase--deletion node-statuses :test #'string=) "d")
            ((find 'add-categorial-links node-statuses :test #'string=) "c")
            (t (error "Did not find any repair node statuses and no solution was found!"))))))
         
(defmethod interact :before ((experiment grammar-learning-experiment)
                             interaction &key)
  (multiple-value-bind (utterance gold-standard-meaning)
      (get-interaction-data interaction)
    (loop for agent in (interacting-agents experiment)
          do (initialize-agent agent utterance gold-standard-meaning))
    (notify interaction-before-finished utterance gold-standard-meaning)))           

(defmethod interact ((experiment grammar-learning-experiment)
                     interaction &key)
  "the learner attempts to comprehend the utterance with its grammar, and applies any repairs if necessary"
    (let* ((cipn (second (multiple-value-list (run-learner-comprehension-task (learner experiment)))))
           (success? (determine-communicative-success cipn)))
      (setf (success-buffer experiment) (append (success-buffer experiment)
                                                (list (if success? 1 0))))
      (setf (repair-buffer experiment) (append (repair-buffer experiment)
                                                (list (get-last-repair-symbol cipn))))            
      (loop for agent in (population experiment)
            do (setf (communicated-successfully agent) success?))))
    
(define-event agent-confidence-level (level float))

(defmethod interact :after ((experiment grammar-learning-experiment)
                            interaction &key)
  "the tutor gives the answer, the learner learns from the gold standard"
  (let ((successp
         (loop for agent in (population experiment)
               always (communicated-successfully agent))))
    
    ;; record the success of the current utterance
    ;; by adding the success to the confidence buffer of the learner
    (setf (confidence-buffer experiment)
          (cons (if successp 1 0)
                (butlast (confidence-buffer experiment))))
    (unless successp
      (setf (failed-question-data experiment) (append (list (list
                                                       (utterance (first (agents experiment)))
                                                       (meaning (first (agents experiment)))))
                                                      (failed-question-data experiment))))
    (when (get-configuration experiment :enable-autotelic-levels)
      (notify agent-confidence-level (average (confidence-buffer experiment))))

    ))

    