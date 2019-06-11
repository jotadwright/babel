(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;
;; CIP Goal Test ;;
;;;;;;;;;;;;;;;;;;;

(defmethod cip-goal-test ((node cip-node) (mode (eql :non-gold-standard-meaning)))
  "Checks whether the extracted meaning is equivalent with the gold standard meaning."
  (and (fully-expanded? node)
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (meaning (extract-meanings (left-pole-structure resulting-cfs)))
           (gold-standard-meanings (get-data resulting-cfs :meanings)))
      (find meaning gold-standard-meanings :test #'irl:equivalent-irl-programs?))))

(defmethod cip-goal-test ((node cip-node) (mode (eql :non-gold-standard-utterance)))
  "Checks whether the extracted meaning is equivalent with the gold standard meaning."
  (and (fully-expanded? node)
    (let* ((resulting-cfs (car-resulting-cfs (cipn-car node)))
           (utterance (render node (get-configuration (construction-inventory node) :render-mode)))
           (gold-standard-utterances (get-data resulting-cfs :utterances)))
      (find (format nil "~{~a~^ ~}" utterance) gold-standard-utterances :test #'equalp))))
