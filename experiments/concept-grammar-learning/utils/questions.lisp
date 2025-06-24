(in-package :clg)

;; ----------------------
;; + Sampling questions +
;; ----------------------

(define-event log-unseen-questions (n number))

(defgeneric sample-question (agent mode)
  (:documentation "The agent samples a question from the dataset according to mode"))

;; ---------------------------
;; + DETERMINISTIC questions +
;; ---------------------------

(defmethod sample-question ((agent clevr-learning-agent) (mode (eql :deterministic)))
  (let* (;; sample a question
         (question-scenes-answers-cons (first (question-data (experiment agent))))
         ;; question
         (question (car question-scenes-answers-cons))
         ;; scenes-and-answers
         (scenes-and-answers (cdr question-scenes-answers-cons))
         ;; sample a scene + its answer
         (scene-and-answer (first scenes-and-answers))
         ;; find the answer and scene
         (answer-entity (find-clevr-entity (cdr scene-and-answer) *clevr-ontology*))
         (clevr-scene (find-scene-by-name (car scene-and-answer) (world (experiment agent)))))
    (values question clevr-scene answer-entity)))

;; ---------------------
;; + RANDOM  questions +
;; ---------------------

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
  (load-clevr-scene-and-answer agent (random-elt (question-data (experiment agent)))))

;; -------------------
;; + DEBUG questions +
;; -------------------
(defmethod sample-question ((agent clevr-learning-agent) (mode (eql :debug)))
  ;; sample a question according to the index of the interaction
  (let* ((current-interaction-nr (interaction-number (current-interaction (experiment agent))))
         (nr-of-samples (length (question-data (experiment agent))))
         (n (mod (1- current-interaction-nr) nr-of-samples))
         (nth-sample (nth n (question-data (experiment agent)))))
    (load-clevr-scene-and-answer agent nth-sample)))



;; ----------------------------------
;; + DEBUG questions -- start len 4 +
;; ----------------------------------
(defmethod sample-question ((agent clevr-learning-agent) (mode (eql :select-from-question-list-random)))
  ;; sample a question according to the index of the interaction

  (let* ((questions (get-configuration (experiment agent) :questions-list))
        ; (current-interaction-nr (interaction-number (current-interaction (experiment agent))))
        ; (nr-of-samples (length (question-data (experiment agent))))
        ; (n (mod (1- current-interaction-nr) nr-of-samples))
         (random-question (random-elt questions))
         (random-sample (find random-question (question-data (experiment agent)) :test #'string= :key #'first)))
    (load-clevr-scene-and-answer agent random-sample)))



;; -------------------
;; + SMART questions +
;; -------------------
(defmethod sample-question ((agent clevr-learning-tutor) (mode (eql :smart)))
  ;; sample a question according to previous successes
  (let ((question-data (question-data (experiment agent))))
    (multiple-value-bind (unseen-question-indices seen-question-indices)
        (loop for (index . elem) in (question-success-table agent)
              if (null elem) collect index into unseen
              else collect index into seen
              finally (return (values unseen seen)))
      (notify log-unseen-questions (length unseen-question-indices))
      (let ((set-to-consider (cond ((and unseen-question-indices seen-question-indices)
                                    (random-elt (list 'unseen 'seen)))
                                   (unseen-question-indices 'unseen)
                                   (seen-question-indices 'seen))))
        (case set-to-consider
          (unseen (let* ((random-index (random-elt unseen-question-indices))
                         (sample (nth random-index question-data)))
                    (setf (current-question-index agent) random-index)
                    (load-clevr-scene-and-answer agent sample)))
          (seen (let* ((failure-rates (loop for index in seen-question-indices
                                            for elem = (rest (nth index (question-success-table agent)))
                                            collect (/ (cdr elem) (+ (car elem) (cdr elem)))))
                       (cumulative-weights (loop with failure-sum = (reduce #'+ failure-rates)
                                                 for rate in failure-rates
                                                 for weight = (/ rate failure-sum)
                                                 sum weight into cumw
                                                 collect cumw))
                       (sample-index (loop with r = (random 1.0)
                                           for cw in cumulative-weights
                                           for index in seen-question-indices
                                           when (> cw r) return index))
                       ;; do the sample
                       (sample (nth sample-index question-data)))
                  (setf (current-question-index agent) sample-index)
                  (load-clevr-scene-and-answer agent sample))))))))


;; Utility functions
(defun clevr-val-number (s)
  (let* ((parts (split-sequence:split-sequence #\_ s))
         (num-part (car (last parts))))
    (parse-integer num-part)))

(defun load-clevr-scene-and-answer-by-split (agent scenes-and-answers)
  (let ((current-split (get-configuration (experiment agent) :current-split))
        (split-on-index (get-configuration (experiment agent) :scenes-train-test-split-index)))
    (loop for scene-and-answer = (random-elt scenes-and-answers)
          for scene-fname = (first scene-and-answer)
          for val-id = (clevr-val-number scene-fname)
          when (or (and (eq current-split :train) (> val-id split-on-index))
                   (and (eq current-split :test)  (< val-id split-on-index)))
            return scene-and-answer)))

(defun load-clevr-scene-and-answer (agent question-scenes-answers-cons)
  (let* ((question (first question-scenes-answers-cons))
         (program (second question-scenes-answers-cons))
         (scenes-and-answers (third question-scenes-answers-cons))
         ;(random-scene-and-answer (random-elt scenes-and-answers))
         (random-scene-and-answer (load-clevr-scene-and-answer-by-split agent scenes-and-answers))
         (answer-entity (find-clevr-entity (cdr random-scene-and-answer) (ontology agent)))
         (clevr-scene (find-scene-by-name (car random-scene-and-answer) (world (experiment agent)))))
    (if clevr-scene
      (values question program clevr-scene answer-entity)
      (load-clevr-scene-and-answer agent question-scenes-answers-cons))))
