;;;; interaction.lisp

(in-package :clevr-grammar-learning)

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

(define-event interaction-before-finished
  (utterance string) (gold-standard-meaning t))

(defun get-interaction-data (interaction)
  "retrieve the nth utterance and gold standard meaning from the dataset"
  (let* ((interaction-data (nth (- (interaction-number interaction) 1) (question-data (experiment interaction))))
         (utterance (first interaction-data))
         (gold-standard-meaning (cdr interaction-data)))
    (values utterance gold-standard-meaning)))

(defun determine-communicative-success (cipn)
  (and (find 'SUCCEEDED (statuses cipn))
       (loop for node in (cons cipn (all-parents cipn))
             for node-statuses = (statuses node)
             never (find 'ADDED-BY-REPAIR node-statuses))))

(defmethod interact :before ((experiment clevr-grammar-learning-experiment)
                             interaction &key)
  (multiple-value-bind (utterance gold-standard-meaning)
      (get-interaction-data interaction)
    (loop for agent in (interacting-agents experiment)
          do (initialize-agent agent utterance gold-standard-meaning))
    (notify interaction-before-finished utterance gold-standard-meaning)))

(defmethod interact ((experiment clevr-grammar-learning-experiment)
                     interaction &key)
  "the learner attempts to comprehend the utterance with its grammar, and applies any repairs if necessary"
  (multiple-value-bind (learner-meaning cipn) (run-learner-comprehension-task (learner experiment))
    (let* ((successp (determine-communicative-success cipn)))
         (loop for agent in (population experiment)
          do (setf (communicated-successfully agent) successp)))))
    
(define-event agent-confidence-level (level float))

(defmethod interact :after ((experiment clevr-grammar-learning-experiment)
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
    (when (get-configuration experiment :enable-autotelic-levels)
      (notify agent-confidence-level (average (confidence-buffer experiment))))

    ;; check the confidence level and (maybe) transition to the next challenge
    (maybe-increase-level experiment)))

(defun maybe-increase-level (experiment)
  (when (and (get-configuration experiment :enable-autotelic-levels)
             (> (average (confidence-buffer experiment))
                (get-configuration experiment :confidence-threshold))
             (< (get-configuration experiment :current-challenge-level)
                (get-configuration experiment :max-challenge-level)))
    ;; increase the current challenge level
    (set-configuration experiment :current-challenge-level
                       (1+ (get-configuration experiment :current-challenge-level))
                       :replace t)
    ;; reset the confidence buffer to all zeros
    (setf (confidence-buffer experiment)
          (make-list (get-configuration experiment :evaluation-window-size)
                     :initial-element 0))
    ;; load the questions for the current challenge level
    (load-questions-for-current-challenge-level
     experiment (get-configuration experiment :observation-sample-mode))))
    