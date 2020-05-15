(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;; Priority mode  integrating seq2seq heuristics.            ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod cip-priority ((node cip-node) (mode (eql :seq2seq-heuristic-additive)))
  "Adds probability of cxn according to seq2seq-model to priority of parent."
  (if (all-parents node)
    (let* (;; look for distribution information in data of parent-node
           (distribution (get-data (first (all-parents node)) :seq2seq-prediction))
           (applied-cxn (first (applied-constructions node)))
           (cxn-probability (cdr (assoc (name applied-cxn) distribution :key #'internal-symb :test #'equal))))
      (+ cxn-probability (priority (first (all-parents node)))))
    0))


(defmethod cip-priority ((node cip-node) (mode (eql :seq2seq-heuristic-log-sum)))
  "Adds the log of the probability of cxn according to seq2seq-model to priority of parent.
   This corresponds to multiplying the probabilities."
  (if (all-parents node)
    (let* (;; look for distribution information in data of parent-node
           (distribution (get-data (first (all-parents node)) :seq2seq-prediction))
           (applied-cxn (first (applied-constructions node)))
           (cxn-probability (cdr (assoc (name applied-cxn) distribution
                                        :key #'internal-symb :test #'equal)))
           (depth (length (all-parents node))))
      (+ (* (+ (random 0.1) 0.1) depth) ; multiplies depth with random between 0.1 and 0.2
         (log cxn-probability)
         (priority (first (all-parents node)))))
    0))
            