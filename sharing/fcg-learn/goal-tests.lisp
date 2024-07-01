(in-package :fcg)

;;;;;;;;;;;;;;;;
;;            ;;
;; Goal tests ;;
;;            ;;
;;;;;;;;;;;;;;;;

(defmethod cip-goal-test ((node cip-node) (mode (eql :gold-standard)))
  "Returns t if gold meaning of gold form is in transient structure."
  (gold-standard-solution-p (car-resulting-cfs (cipn-car node))
                            (get-data (blackboard (construction-inventory node)) :speech-act)
                            (direction (cip node))
                            (configuration (construction-inventory node))))


(defun gold-standard-solution-p (cfs speech-act direction configuration)
  "Checks whether the the transient structure holds the gold standard meaning or form, depending on the direction of processing."
  (case direction
    ('<-
     (equivalent-predicate-networks (extract-meanings (left-pole-structure cfs))
                                    (pn::variablify-predicate-network (meaning speech-act)
                                                                      (get-configuration configuration :meaning-representation-format))))
    ('->
     (equalp (render cfs (get-configuration configuration :render-mode))
              (form speech-act)))))