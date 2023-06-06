(in-package :pattern-finding)

;;;;;;;;;;;;;;;;;;;
;; CIP Goal Test ;;
;;;;;;;;;;;;;;;;;;;

(defmethod cip-goal-test ((node cip-node) (mode (eql :non-gold-standard-meaning)))
  "Checks whether the extracted meaning is equivalent with the gold standard meaning."
  (and (fully-expanded? node)
       (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
              (meaning (extract-meanings (left-pole-structure resulting-cfs)))
              (meaning-representation-formalism (get-configuration (construction-inventory node) :meaning-representation))
              (gold-standard-meaning (get-data resulting-cfs :meaning)))
         (if (equivalent-meaning-networks gold-standard-meaning meaning meaning-representation-formalism)
           (progn (set-data (goal-test-data node) :result-goal-test-non-gold-standard-meaning t) t)
           (progn (set-data (goal-test-data node) :result-goal-test-non-gold-standard-meaning nil) nil)))))


(defmethod cip-goal-test ((node cip-node) (mode (eql :non-gold-standard-utterance)))
  "Checks whether the extracted meaning is equivalent with the gold standard meaning."
  (and (fully-expanded? node)
       (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
              (utterance (render node (get-configuration (construction-inventory node) :render-mode)))
              (gold-standard-utterances (get-data resulting-cfs :utterances)))
         (when (find (format nil "~{~a~^ ~}" utterance) gold-standard-utterances :test #'equalp)
           (set-data (blackboard (construction-inventory node)) :add-categorial-links-repair-failed nil)
           t))))
