;;;; interaction.lisp

(in-package :pattern-finding-old)

;; ----------------------
;; + Before Interaction +
;; ----------------------

(defmethod initialize-agent ((agent pattern-finding-agent)
                             utterance gold-standard-meaning)
  (setf (utterance agent) utterance
        (meaning agent) gold-standard-meaning
        (communicated-successfully agent) t))

(defun get-interaction-data (interaction)
  "retrieve the nth utterance and gold standard meaning from the dataset"
  (let* ((interaction-data (nth (- (interaction-number interaction) 1)
                                (question-data (experiment interaction))))
         (utterance (first interaction-data))
         (gold-standard-meaning (cdr interaction-data)))
    (values utterance gold-standard-meaning)))

(define-event interaction-before-finished
  (utterance string) (gold-standard-meaning t))

(defmethod interact :before ((experiment pattern-finding-experiment)
                             interaction &key)
  (multiple-value-bind (utterance gold-standard-meaning)
      (get-interaction-data interaction)
    (loop for agent in (interacting-agents experiment)
          do (initialize-agent agent utterance gold-standard-meaning))
    (notify interaction-before-finished utterance gold-standard-meaning)))

;; ---------------
;; + Interaction +
;; ---------------

(defun determine-communicative-success (cipn)
  (assert (find 'fcg::succeeded (statuses cipn)))
  (let ((all-node-statuses (mappend #'statuses (cons cipn (all-parents cipn))))
        (communicative-success t))
    (when (find 'gold-standard-consulted all-node-statuses)
      (setf communicative-success nil))
    communicative-success))

(defun get-last-repair-symbol (cipn success?)
  (let ((all-node-statuses (mappend #'statuses (cons cipn (all-parents cipn)))))
    (if (not (find 'added-by-repair all-node-statuses))
      (if success? "." "x") ; return a dot or x in processing mode
      (cond ((find 'add-categorial-links all-node-statuses) "1")
            ((find 'item-based->item-based all-node-statuses) "2")
            ((find 'item-based-partial-analysis all-node-statuses) "3")
            ((find 'holistic->item-based all-node-statuses) "4")
            ((find 'holistic-partial-analysis all-node-statuses) "5")
            ((find 'nothing->holistic all-node-statuses) "6")
            (t (error "Did not find any repair node statuses and no solution was found!"))))))

(defmethod interact ((experiment pattern-finding-experiment)
                     interaction &key)
  "the learner attempts to comprehend the utterance with its grammar, and applies any repairs if necessary"
  (let* ((cipn (run-learner-comprehension-task (learner experiment)))
         (success? (determine-communicative-success cipn)))
    ;; add success to the front of the success buffer
    ;; add last repair to the front of the repair buffer
    (push (if success? 1 0) (success-buffer experiment))
    (push (get-last-repair-symbol cipn success?) (repair-buffer experiment))
    ;; when no success, store the example
    (unless success?
      (push (list (utterance (first (agents experiment)))
                  (meaning (first (agents experiment))))
            (failed-question-data experiment)))
    (loop for agent in (population experiment)
          do (setf (communicated-successfully agent) success?))))

    