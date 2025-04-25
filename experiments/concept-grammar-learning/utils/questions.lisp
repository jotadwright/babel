(in-package :clg)

;; ----------------------
;; + Sampling questions +
;; ----------------------

(define-event log-unseen-questions (n number))

(defgeneric sample-question (agent mode)
  (:documentation "The agent samples a question from the dataset according to mode"))

(defmethod sample ((agent clevr-learning-agent) (mode (eql :deterministic)))
  ;; always load a specific question
  (load-clevr-scene-and-answer agent (first (question-data (experiment agent)))))

(defmethod sample-question ((agent clevr-learning-agent) (mode (eql :randoms)))
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

(defmethod sample-question ((agent clevr-learning-agent) (mode (eql :debug)))
  ;; sample a question according to the index of the interaction
  (let* ((current-interaction-nr (interaction-number (current-interaction (experiment agent))))
         (nr-of-samples (length (question-data (experiment agent))))
         (n (mod (1- current-interaction-nr) nr-of-samples))
         (nth-sample (nth n (question-data (experiment agent)))))
    (load-clevr-scene-and-answer agent nth-sample)))

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
