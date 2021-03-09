;;;; interaction.lisp

(in-package :clevr-learning)

;; --------------------
;; + Initialize agent +
;; --------------------

(defmethod initialize-agent ((agent clevr-learning-tutor)
                             question scene answer)
  (setf (utterance agent) question
        (topic agent) answer
        (communicated-successfully agent) t)
  (set-data (ontology agent) 'clevr-context scene))

(defmethod initialize-agent ((agent clevr-learning-learner)
                             question scene answer)
  (setf (utterance agent) question
        (topic agent) answer
        (communicated-successfully agent) t)
  (set-data (ontology agent) 'clevr-context scene))

;; ---------------
;; + Interaction +
;; ---------------

(define-event interaction-before-finished
  (scene clevr-scene) (question string) (answer t))

(defun sample-question (experiment)
  ;; !!!!!!!!!!! This is an ugly temporary solution
  ;; Needs to be fixed in the future...
  (let* ((random-sample (random-elt (question-data experiment)))
         (question (rest (assoc :question random-sample)))
         (scenes-and-answers (rest (assoc :answers random-sample))))
    (if (search "How big" question)
      (sample-question experiment)
      (let* ((random-scene-and-answer (random-elt scenes-and-answers))
             (answer-entity (find-clevr-entity (rest (assoc :answer random-scene-and-answer))
                                               *clevr-ontology*))
             (clevr-scene (find-scene-by-name (rest (assoc :scene random-scene-and-answer))
                                             (world experiment))))
        (values question clevr-scene answer-entity)))))


(defmethod interact :before ((experiment clevr-learning-experiment)
                             interaction &key)
  ;; Choose a random scene and a random question and initialize the agents
  (multiple-value-bind (question clevr-scene answer-entity) (sample-question experiment)
    (loop for agent in (interacting-agents experiment)
          do (initialize-agent agent question clevr-scene answer-entity))
    (notify interaction-before-finished clevr-scene question answer-entity)))

(defmethod interact ((experiment clevr-learning-experiment)
                     interaction &key)
  ;; The tutor is ran implicitely when choosing a random scene and question
  ;; Now we only need to run the learner's part.
  (let ((successp (run-learner-hearer-task (learner experiment))))
    (loop for agent in (population experiment)
          do (setf (communicated-successfully agent) successp))))

(define-event agent-confidence-level (level float))

(defmethod interact :after ((experiment clevr-learning-experiment)
                            interaction &key)
  ;; add the success to the confidence buffer
  (let ((successp
         (loop for agent in (population experiment)
               always (communicated-successfully agent)))
        (composer-strategy
         (get-configuration experiment :composer-strategy))
        (agent (learner experiment)))
    (if (= (length (confidence-buffer experiment))
           (get-configuration experiment :evaluation-window-size))
      (setf (confidence-buffer experiment)
            (cons (if successp 1 0)
                  (butlast (confidence-buffer experiment))))
      (push (if successp 1 0) (confidence-buffer experiment)))
    (notify agent-confidence-level (average (confidence-buffer experiment)))
    ;; add the current scene/program to memory, depending on the
    ;; composer strategy and the success
    (case composer-strategy
      (:store-past-programs
       (unless successp
         (add-past-program agent (find-data (task-result agent) 'irl-program))))
      (:store-past-scenes
       (add-past-scene agent))))
  ;; check the confidence level and (maybe) transition to the next challenge
  ;; clear the confidence buffer
  (when (and (> (average (confidence-buffer experiment))
                (get-configuration experiment :confidence-threshold))
             (< (get-configuration experiment :current-challenge-level)
                (get-configuration experiment :max-challenge-level)))
    (set-configuration experiment :current-challenge-level
                       (1+ (get-configuration experiment :current-challenge-level))
                       :replace t)
    (setf (confidence-buffer experiment) nil)
    (load-questions-for-current-challenge-level
     experiment (get-configuration experiment :question-sample-mode))
    (set-primitives-for-current-challenge-level (learner experiment))
    (update-composer-chunks-w-primitive-inventory (learner experiment))))
