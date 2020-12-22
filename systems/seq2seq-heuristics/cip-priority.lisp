(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                           ;;
;; Priority mode  integrating seq2seq heuristics.            ;;
;;                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod cip-priority ((node cip-node) (mode (eql :seq2seq-heuristic)))
  "Return the probability of the cxn."
  (if (all-parents node)
    (let* ((distribution (get-data (first (all-parents node)) :seq2seq-prediction))
           (applied-cxn (first (applied-constructions node)))
           (cxn-probability (rest
                             (assoc (name applied-cxn) distribution
                                    :key #'(lambda (n) (intern (mkstr n) :clevr-grammar))
                                    :test #'equal))))
      cxn-probability)
    0))

(defmethod cip-priority ((node cip-node) (mode (eql :seq2seq-heuristic-additive)))
  "Adds probability of cxn according to seq2seq-model to priority of parent."
  (if (all-parents node)
    (let* (;; look for distribution information in data of parent-node
           (distribution (get-data (first (all-parents node)) :seq2seq-prediction))
           (applied-cxn (first (applied-constructions node)))
           (cxn-probability (cdr
                             (assoc (name applied-cxn) distribution
                                    :key #'(lambda (n) (intern (mkstr n) :clevr-grammar))
                                    :test #'equal))))
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

(defmethod cip-priority ((node cip-node) (mode (eql :seq2seq-additive-with-sets)))
  "The priority of the node depends on the current label of the cxn-supplier.
   If it is :cxn, use the probabilities predicted by the seq2seq model.
   Otherwise, assign equal probability to all cxns.
   Always sum with the priority of the parent."
  (if (all-parents node)
    (if (eq (intern (mkstr (current-label (cxn-supplier (parent node)))) :fcg) 'cxn)
      (let* ((distribution (get-data (parent node) :seq2seq-prediction))
             (applied-cxn (first (applied-constructions node)))
             (cxn-prob (cdr (assoc (name applied-cxn) distribution
                                   :key #'(lambda (n) (intern (mkstr n) :clevr-grammar))
                                   :test #'equal))))
        (+ cxn-prob
           (priority (parent node))))
      (let ((num-siblings
             (if (siblings node)
               (1+ (length (siblings node)))
               1)))
        (+ (float (/ 1 num-siblings))
           (priority (parent node)))))
    0))

                                            
