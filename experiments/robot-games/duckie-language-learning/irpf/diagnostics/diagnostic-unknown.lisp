(in-package :duckie-language-learning)

;; ---------------------------------
;; + Diagnostic: unknown utterance +
;; ---------------------------------

;; problem
(defclass unknown-utterance-problem (problem)
  () (:documentation "Problem created when the utterance is completely unknown
                      OR when no solution is found using partial utterances."))

;; diagnostic class
(defclass diagnose-unknown-utterance (diagnostic)
  ((trigger :initform 'fcg::new-node)))

;; diagnose func
(defmethod diagnose ((diagnostic diagnose-unknown-utterance)
                     (node cip-node) &key &allow-other-keys)
  (when (eql (direction (cip node)) '<-)
    (when  (unknown-utterance-p node)
      ;; Within the diagnostic, the agent performs intention reading.
      ;; The reconstructed intention is then used to check following repairs:
      ;;     holophrase -> item-based (all variants) and
      ;;     add-holophrase.
      (let* ((agent (find-data (blackboard (construction-inventory node)) :owner))
             (problem (make-instance 'unknown-utterance-problem)))
        (push (type-of problem) (fcg::statuses node))
        (push 'fcg::diagnostic-triggered (fcg::statuses node))
        (notify diagnostic-trigger node diagnostic)
        (let ((correct-answer (ask-correct-answer agent)))
          (set-data problem :intention (compose-program agent correct-answer))
          (set-data problem :answer correct-answer))
        (set-data problem :owner agent)
        problem))))

;; helper functions
(defun unknown-utterance-p (node)
  (and ;; node is fully expanded
       (fully-expanded? node)
       ;; node is the initial node
       (initial-node-p node)
       ;; queue is empty or fully expanded
       (or (null (queue (cip node)))
           (notany #'null
                   (mapcar #'fully-expanded?
                           (cons node (queue (cip node))))))
       ;; no children
       (null (children node))
       ;; no applied constructions
       (null (applied-constructions node))))
