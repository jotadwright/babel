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
        (meaning agent) nil
        (communicated-successfully agent) t
        (task-result agent) nil
        (tasks-and-processes::tasks agent) nil))

;; ---------------
;; + Interaction +
;; ---------------

(define-event interaction-before-finished
  (utterance string) (gold-standard-meaning t))

(defmethod interact :before ((experiment clevr-grammar-learning-experiment)
                             interaction &key)
  ;; Choose a question and initialize the agents
  (let* ((interaction-data (nth (interaction-number interaction) (question-data experiment)))
         (utterance (first interaction-data))
         (gold-standard-meaning (cdr interaction-data)))
    (loop for agent in (interacting-agents experiment)
          do (initialize-agent agent utterance gold-standard-meaning))
    (notify interaction-before-finished utterance gold-standard-meaning)))

(defmethod interact ((experiment clevr-grammar-learning-experiment)
                     interaction &key)
  (let ((successp (run-learner-hearer-task (learner experiment))))
    (loop for agent in (population experiment)
          do (setf (communicated-successfully agent) successp))))
    
(define-event agent-confidence-level (level float))

(defmethod interact :after ((experiment clevr-grammar-learning-experiment)
                            interaction &key)
  (let ((successp
         (loop for agent in (population experiment)
               always (communicated-successfully agent))))
    ;; record the success of the current question
    ;; used by 'smart' speaker mode for the tutor
    (when (eq (speaker interaction) (tutor interaction))
      (record-interaction-success-in-table (tutor interaction) successp))
    ;; add the success to the confidence buffer of the learner
    (setf (confidence-buffer experiment)
          (cons (if successp 1 0)
                (butlast (confidence-buffer experiment))))
    (notify agent-confidence-level (average (confidence-buffer experiment)))
    ;; add the current scene/program to memory of the learner, 
    ;; depending on the composer strategy and the success,
    ;; but only when being the hearer (when learner is speaker,
    ;; we dont have access to the utterance)
    (when (eq (hearer interaction) (learner interaction))
      (case (get-configuration experiment :composer-strategy)
        (:store-past-programs
         (unless successp
           (add-past-program
            (learner experiment)
            (find-data (task-result (learner experiment))
                       'irl-program))))
        (:store-past-scenes
         (add-past-scene (learner experiment))))))
  ;; check the confidence level and (maybe) transition to the next challenge
  (maybe-increase-level experiment))


(defun record-interaction-success-in-table (agent success)
  "The agent records the success for the current question in its memory"
  (let ((index (current-question-index agent)))
    (unless (= index -1)
      (let ((entry (rest (assoc index (question-success-table agent)))))
        (if (null entry)
          (setf (rest (assoc index (question-success-table agent)))
                (if success (cons 1 0) (cons 0 1)))
          (setf (rest (assoc index (question-success-table agent)))
                (if success
                  (cons (1+ (car entry)) (cdr entry))
                  (cons (car entry) (1+ (cdr entry))))))))))

(defun maybe-increase-level (experiment)
  (when (and (> (average (confidence-buffer experiment))
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
     experiment (get-configuration experiment :question-sample-mode))
    ;; set the available primitives for the learner agent
    (set-primitives-for-current-challenge-level (learner experiment))
    ;; update the composer chunks for the learner agent
    (update-composer-chunks-w-primitive-inventory (learner experiment))
    ;; clear the question-index-table
    (clear-question-success-table (tutor experiment))
    ;; clear the learner's memory of scenes of the previous level
    (clear-memory (learner experiment))))
    