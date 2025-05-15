(in-package :clg)

;; ---------------
;; + Interaction +
;; ---------------

;; events
(define-event interaction-before-finished (scene clevr-scene) (question string) (answer t))
(define-event agent-confidence-level (level float))
(define-event log-unseen-questions (n number))

;; ----------------------
;; + BEFORE Interaction +
;; ----------------------
(defmethod interact :before ((experiment clevr-learning-experiment)
                             interaction &key)
  ;; Choose a random scene and a random question and initialize the agents
  (multiple-value-bind (question program clevr-scene answer-entity)
      ;; speaker can be tutor or learner
      (sample-question (speaker interaction) (get-configuration experiment :tutor-sample-mode))
    (loop for agent in (interacting-agents experiment)
          do (initialize-agent agent program question clevr-scene answer-entity))
    (notify interaction-before-finished clevr-scene question answer-entity)))

;; ----------------------
;; + DURING Interaction +
;; ----------------------
(defmethod interact ((experiment clevr-learning-experiment)
                     interaction &key)
  (case (role (speaker interaction))
    (tutor
     ;; if the tutor is the speaker,
     ;; only need to run the learner' hearer task
     (let ((successp (run-learner-hearer-task (learner experiment))))
       (loop for agent in (population experiment)
             do (setf (communicated-successfully agent) successp))))
    (learner
     ;; if the learner is the speaker, need to run
     ;; both the learner' speaker task and
     ;;      the tutor's hearer task
     (let* ((learner-speaks-task-result (run-learner-speaker-task (learner experiment)))
            (utterance (find-data learner-speaks-task-result 'utterance))
            successp)
       (when utterance
         ;; set the utterance in both agents
         (setf (utterance (learner experiment)) utterance)
         (setf (utterance (tutor experiment)) utterance)

         ;; even if gold-answer is nil, need to run alignment
         (let ((gold-answer (run-tutor-hearer-task (tutor experiment))))  
           (setf successp (run-learner-alignment-task (learner experiment)
                                                      learner-speaks-task-result
                                                      gold-answer))))
       ;; set whether interaction was successful
       (loop for agent in (population experiment)
             do (setf (communicated-successfully agent) successp))))))

;; ----------------------
;; + AFTER Interaction +
;; ----------------------
(defmethod interact :after ((experiment clevr-learning-experiment)
                            interaction &key)
  (let ((successp (loop for agent in (population experiment) always (communicated-successfully agent))))

    (unless successp
      (when (> (interaction-number interaction) 50000)
        (write-game-summary-to-log experiment interaction)))
    
    ;; record the success of the current question
    ;; used by 'smart' speaker mode for the tutor
    (when (eq (speaker interaction) (tutor interaction))
      (record-interaction-success-in-table (tutor interaction) successp))
    ;; add the success to the confidence buffer of the learner
    (setf (confidence-buffer experiment)
          (cons (if successp 1 0) (butlast (confidence-buffer experiment))))
    (notify agent-confidence-level (average (confidence-buffer experiment)))
    ;; add the current scene/program to memory of the learner, 
    ;; depending on the composer strategy and the success,
    ;; but only when being the hearer (when learner is speaker,
    ;; we dont have access to the utterance)
    #|(when (eq (hearer interaction) (learner interaction))
      (add-past-scene (learner experiment)))|#)
  ;; check the confidence level and (maybe) transition to the next challenge
  (maybe-increase-level experiment))

;; --------------------
;; + Initialize agent +
;; --------------------

(defmethod initialize-agent ((agent clevr-learning-tutor)
                             program
                             question
                             scene
                             answer)
  (setf (utterance agent) question
        (topic agent) answer
        (communicated-successfully agent) t
        (gold-standard-program agent) program)
  (set-data (ontology agent) 'clevr-context scene))

(defmethod initialize-agent ((agent clevr-learning-learner)
                             program
                             question
                             scene
                             answer)
  (setf (utterance agent) question
        (topic agent) answer
        (communicated-successfully agent) t
        (task-result agent) nil
        (tasks-and-processes::tasks agent) nil
        (gold-standard-program agent) program)
  (set-data (ontology agent) 'clevr-context scene))


;; -------------
;; + Utilities +
;; -------------
(defun add-past-scene (agent)
  ;;;; store past scenes (composer strategy)
  ;;;; ==> store all past scenes with the applied utterance and correct answer
  ;;;;     and use these when composing a new program (the new program must
  ;;;;     lead to the correct answer in all of these scenes).
  
  ;; Add the current scene and answer to the list
  ;; of past scenes for the current utterance
  (let* ((clevr-scene (find-data (ontology agent) 'clevr-context))
         (sample (cons (index clevr-scene) (topic agent)))
         (hash-key (sxhash (utterance agent)))
         (window-size (get-configuration agent :composer-past-scenes-window)))
    (if (gethash hash-key (memory agent))
      (if (>= (length (gethash hash-key (memory agent))) window-size)
        (setf (gethash hash-key (memory agent))
              (cons sample (butlast (gethash hash-key (memory agent)))))
        (push sample (gethash hash-key (memory agent))))
      (setf (gethash hash-key (memory agent)) (list sample)))))

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
