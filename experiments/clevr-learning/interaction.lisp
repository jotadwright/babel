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
        (communicated-successfully agent) t
        (task-result agent) nil
        (tasks-and-processes::tasks agent) nil)
  (set-data (ontology agent) 'clevr-context scene))

;; ---------------
;; + Interaction +
;; ---------------

(define-event interaction-before-finished
  (scene clevr-scene) (question string) (answer t))

(defun load-clevr-scene-and-answer (tutor sample)
  ;; !!!!!!!!!!! This is an ugly temporary solution
  ;; Needs to be fixed in the future...
  (let* ((question (rest (assoc :question sample)))
         (scenes-and-answers (rest (assoc :answers sample))))
    (if (search "How big" question)
      (sample-question tutor (get-configuration tutor :tutor-mode))
      (let* ((random-scene-and-answer (random-elt scenes-and-answers))
             (answer-entity (find-clevr-entity (rest (assoc :answer random-scene-and-answer))
                                               *clevr-ontology*))
             (clevr-scene (find-scene-by-name (rest (assoc :scene random-scene-and-answer))
                                              (world (experiment tutor)))))
        (values question clevr-scene answer-entity)))))

(defgeneric sample-question (tutor mode)
  (:documentation "The tutor samples a question from the dataset according to mode"))

(defmethod sample-question ((tutor clevr-learning-tutor) (mode (eql :random)))
  (load-clevr-scene-and-answer
   tutor (random-elt
          (question-data
           (experiment tutor)))))

(defmethod sample-question ((tutor clevr-learning-tutor) (mode (eql :smart)))
  (let ((question-data (question-data (experiment tutor))))
    (multiple-value-bind (unseen-question-indices seen-question-indices)
        (loop for (index . elem) in (question-index-table tutor)
              if (null elem) collect index into unseen
              else collect index into seen
              finally (return (values unseen seen)))
      (let ((set-to-consider
             (cond ((and unseen-question-indices seen-question-indices)
                    (random-elt (list 'unseen 'seen)))
                   (unseen-question-indices 'unseen)
                   (seen-question-indices 'seen))))
        (case set-to-consider
          (unseen (let* ((random-index (random-elt unseen-question-indices))
                         (sample (nth random-index question-data)))
                    (setf (current-question-index tutor) random-index)
                    (load-clevr-scene-and-answer tutor sample)))
          (seen (let* ((failure-rates
                        (loop for index in seen-question-indices
                              for elem = (rest (nth index (question-index-table tutor)))
                              collect (/ (cdr elem) (+ (car elem) (cdr elem)))))
                       (cumulative-weights
                        (loop with failure-sum = (reduce #'+ failure-rates)
                              for rate in failure-rates
                              for weight = (/ rate failure-sum)
                              sum weight into cumw
                              collect cumw))
                       (sample-index
                        (loop with r = (random 1.0)
                              for cw in cumulative-weights
                              for index in seen-question-indices
                              when (> cw r) return index))
                       (sample (nth sample-index question-data)))
                  (setf (current-question-index tutor) sample-index)
                  (load-clevr-scene-and-answer tutor sample))))))))


(defmethod interact :before ((experiment clevr-learning-experiment)
                             interaction &key)
  ;; Choose a random scene and a random question and initialize the agents
  (multiple-value-bind (question clevr-scene answer-entity)
      (sample-question (tutor experiment) (get-configuration experiment :tutor-mode))
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
  (let ((successp
         (loop for agent in (population experiment)
               always (communicated-successfully agent)))
        (composer-strategy
         (get-configuration experiment :composer-strategy)))
    ;; add the success to the table of the tutor
    (let* ((current-index (current-question-index (tutor experiment)))
           (entry (rest (assoc current-index (question-index-table (tutor experiment)))))
           (failure-count (get-configuration experiment :tutor-counts-failure-as)))
      (if (null entry)
        (setf (rest (assoc current-index (question-index-table (tutor experiment))))
              (if successp (cons 1 0) (cons 0 failure-count)))
        (setf (rest (assoc current-index (question-index-table (tutor experiment))))
              (if successp
                (cons (1+ (car entry)) (cdr entry))
                (cons (car entry) (+ (cdr entry) failure-count))))))
    ;; add the success to the confidence buffer of the learner
    (if (= (length (confidence-buffer experiment))
           (get-configuration experiment :evaluation-window-size))
      (setf (confidence-buffer experiment)
            (cons (if successp 1 0)
                  (butlast (confidence-buffer experiment))))
      (push (if successp 1 0) (confidence-buffer experiment)))
    (notify agent-confidence-level (average (confidence-buffer experiment)))
    ;; add the current scene/program to memory of the learner, 
    ;; depending on the composer strategy and the success
    (case composer-strategy
      (:store-past-programs
       (unless successp
         (add-past-program (learner experiment) (find-data (task-result (learner experiment)) 'irl-program))))
      (:store-past-scenes
       (add-past-scene (learner experiment)))))
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
