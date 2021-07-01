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

(defun load-clevr-scene-and-answer (agent question-scenes-answers-cons)
  (let* ((question (car question-scenes-answers-cons))
         (scenes-and-answers (cdr question-scenes-answers-cons))
         (random-scene-and-answer (random-elt scenes-and-answers))
         (answer-entity (find-clevr-entity
                         (cdr random-scene-and-answer)
                         *clevr-ontology*))
         (clevr-scene (find-scene-by-name
                       (car random-scene-and-answer)
                       (world (experiment agent)))))
    (values question clevr-scene answer-entity)))

(defgeneric sample-question (speaker speaker-sample-mode)
  (:documentation "The speaker samples a question from the dataset according to mode"))

(define-event log-unseen-questions (n number))

(defmethod sample-question ((agent clevr-learning-agent) (mode (eql :debug)))
  ;; sample a question according to the index of the interaction
  (let* ((current-interaction-nr
          (interaction-number
           (current-interaction
            (experiment
             agent))))
         (nr-of-samples
          (length (question-data (experiment agent))))
         (n (mod (1- current-interaction-nr) nr-of-samples))
         (nth-sample
          (nth n (question-data (experiment agent)))))
    (load-clevr-scene-and-answer
     agent nth-sample)))

(defmethod sample-question ((agent clevr-learning-agent) (mode (eql :random)))
  ;; sample a random question
  (when (eql (role agent) 'tutor)
    (multiple-value-bind (unseen-question-indices seen-question-indices)
        (loop for (index . elem) in (question-success-table agent)
              if (null elem) collect index into unseen
              else collect index into seen
              finally (return (values unseen seen)))
      (declare (ignorable seen-question-indices))
      (notify log-unseen-questions (length unseen-question-indices))))
  (load-clevr-scene-and-answer
   agent (random-elt
          (question-data
           (experiment agent)))))

(defmethod sample-question ((agent clevr-learning-tutor) (mode (eql :smart)))
  ;; sample a question according to previous successes
  (let ((question-data (question-data (experiment agent))))
    (multiple-value-bind (unseen-question-indices seen-question-indices)
        (loop for (index . elem) in (question-success-table agent)
              if (null elem) collect index into unseen
              else collect index into seen
              finally (return (values unseen seen)))
      (notify log-unseen-questions (length unseen-question-indices))
      (let ((set-to-consider
             (cond ((and unseen-question-indices seen-question-indices)
                    (random-elt (list 'unseen 'seen)))
                   (unseen-question-indices 'unseen)
                   (seen-question-indices 'seen))))
        (case set-to-consider
          (unseen (let* ((random-index (random-elt unseen-question-indices))
                         (sample (nth random-index question-data)))
                    (setf (current-question-index agent) random-index)
                    (load-clevr-scene-and-answer agent sample)))
          (seen (let* ((failure-rates
                        (loop for index in seen-question-indices
                              for elem = (rest (nth index (question-success-table agent)))
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
                  (setf (current-question-index agent) sample-index)
                  (load-clevr-scene-and-answer agent sample))))))))


(defmethod interact :before ((experiment clevr-learning-experiment)
                             interaction &key)
  ;; Choose a random scene and a random question and initialize the agents
  (multiple-value-bind (question clevr-scene answer-entity)
      ;; speaker can be tutor or learner
      (sample-question (speaker interaction)
                       (get-configuration experiment :tutor-sample-mode))
    (loop for agent in (interacting-agents experiment)
          do (initialize-agent agent question clevr-scene answer-entity))
    ;; when the game is in :hybrid mode, also make sure the current
    ;; scene is loaded on the server side!
    (when (eql (get-configuration experiment :primitives) :hybrid)
      (let ((server-address
             (find-data (ontology (learner experiment)) 'hybrid-primitives::server-address))
            (cookie-jar
             (find-data (ontology (learner experiment)) 'hybrid-primitives::cookie-jar))
            (image-filename
             (file-namestring
              (image (find-data (ontology (learner experiment)) 'clevr-context)))))
        (load-image server-address cookie-jar image-filename)))
    (notify interaction-before-finished clevr-scene question answer-entity)))

(defmethod interact ((experiment clevr-learning-experiment)
                     interaction &key)
  (unwind-protect
      (case (role (speaker interaction))
        (tutor
         ;; if the tutor is the speaker, only need to run the learner's
         ;; hearer task
         (let ((successp (run-learner-hearer-task (learner experiment))))
           (loop for agent in (population experiment)
                 do (setf (communicated-successfully agent) successp))))
        (learner
         ;; if the learner is the speaker, need to run both the learner's
         ;; speaker task and the tutor's hearer task
         (let* ((learner-speaks-task-result
                 (run-learner-speaker-task (learner experiment)))
                (utterance (find-data learner-speaks-task-result 'utterance))
                successp)
           (when utterance
             (setf (utterance (learner experiment)) utterance)
             (setf (utterance (tutor experiment)) utterance)
             (let ((gold-answer (run-tutor-hearer-task (tutor experiment))))
               ;; even if gold-answer is nil, need to run alignment
               (setf successp
                     (run-learner-alignment-task (learner experiment)
                                                 learner-speaks-task-result
                                                 gold-answer))))
           (loop for agent in (population experiment)
                 do (setf (communicated-successfully agent) successp)))))
    ;; if anything goes wrong during the game,
    ;; or simply at the end of a game,
    ;; the session is cleared from the server.
    (when (eql (get-configuration experiment :primitives) :hybrid)
      (let ((server-address
             (find-data (ontology (learner experiment)) 'hybrid-primitives::server-address))
            (cookie-jar
             (find-data (ontology (learner experiment)) 'hybrid-primitives::cookie-jar)))
        (clear-session server-address cookie-jar)))))
    
(define-event agent-confidence-level (level float))

(defmethod interact :after ((experiment clevr-learning-experiment)
                            interaction &key)
  (let ((successp
         (loop for agent in (population experiment)
               always (communicated-successfully agent))))

    ;; useful for debugging
    ;(unless successp
    ;  (format nil "break here"))

    
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
  (when (and (>= (average (confidence-buffer experiment))
                (get-configuration experiment :confidence-threshold))
             (< (get-configuration experiment :current-challenge-level)
                (get-configuration experiment :max-challenge-level)))
    (format t "~%Increasing the challenge level!!")
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
    (set-primitives-for-current-challenge-level
     (learner experiment) (get-configuration experiment :primitives))
    ;; update the composer chunks for the learner agent
    (update-composer-chunks-w-primitive-inventory (learner experiment))
    ;; clear the question-index-table
    (clear-question-success-table (tutor experiment))
    ;; clear the learner's memory of scenes of the previous level
    (clear-memory (learner experiment))))
    