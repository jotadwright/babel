;;;; alignment.lisp

(in-package :grammar-learning)

(define-event alignment-started)
(define-event cxns-rewarded (cxns list))
(define-event cxns-punished (cxns list))

(defun alter-ego-cxn (original-cxn cxn-inventory)
  (when (attr-val original-cxn :bare-cxn-name)
    (loop for cxn in (constructions cxn-inventory)
          when (and (attr-val cxn :bare-cxn-name)
                    (eq (attr-val cxn :bare-cxn-name) (attr-val original-cxn :bare-cxn-name))
                    (not (eql (name cxn) (name original-cxn))))
          do (return cxn))))
  
(defmethod run-alignment ((agent clevr-learning-learner)
                          cipn (strategy (eql :lateral-inhibition)))

  (notify alignment-started)
  ;; align categorial links
  (loop for (cat-1 . cat-2) in (extract-used-categorial-links cipn)
        do (incf-link-weight cat-1 cat-2 (categorial-network (construction-inventory cipn))))

  ;; align cxns
  (let ((applied-cxns (original-applied-constructions cipn)))
    
    ;; reward the applied cxns 
    (loop with cxn-delta = (get-configuration agent :cxn-incf-score)
          for cxn in applied-cxns
          for alter-ego-cxn = (alter-ego-cxn cxn (grammar agent))
          do (inc-cxn-score cxn :delta cxn-delta)
          (when alter-ego-cxn
            (inc-cxn-score alter-ego-cxn :delta cxn-delta))
          finally (notify cxns-rewarded applied-cxns))
    
     (loop for current-node in (traverse-depth-first (top-node (cip cipn)) :collect-fn #'identity)
                 when (and (field? (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)
                         (not (get-data (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)))
                 do (loop for bad-node in (set-difference (remove (top-node (cip cipn))
                                                                  (cons current-node (all-parents current-node)))
                                                          (remove (top-node (cip cipn))
                                                                  (cons cipn (all-parents cipn)))
                                                          :key #'(lambda (node) (name (car-applied-cxn (cipn-car node)))))
                          for bad-cxn = (car-applied-cxn (cipn-car bad-node))
                          do (dec-cxn-score agent bad-cxn :delta (get-configuration agent :cxn-decf-score))
                          (dec-cxn-score agent (alter-ego-cxn bad-cxn (grammar agent)) :delta (get-configuration agent :cxn-decf-score))
                          append (list bad-cxn (alter-ego-cxn bad-cxn (grammar agent))) into punished-cxns
                          finally (notify cxns-punished (mapcar #'original-cxn punished-cxns))))))

      
