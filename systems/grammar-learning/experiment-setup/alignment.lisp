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
                          solution-cipn competing-solution-cipns (strategy (eql :lateral-inhibition)))
  (notify alignment-started)
  ;; align categorial links
  (loop for (cat-1 . cat-2) in (extract-used-categorial-links solution-cipn)
        do (incf-link-weight cat-1 cat-2 (categorial-network (construction-inventory solution-cipn))))

  ;; reward used cxns 
  (let ((applied-cxns (original-applied-constructions solution-cipn)))
    
    ;; reward the applied cxns 
    (loop with cxn-delta = (get-configuration agent :cxn-incf-score)
          for cxn in applied-cxns
          for alter-ego-cxn = (alter-ego-cxn cxn (grammar agent))
          do (inc-cxn-score cxn :delta cxn-delta)
          (inc-cxn-score alter-ego-cxn :delta cxn-delta)
          finally (notify cxns-rewarded applied-cxns)))        
  ;; punish competitors
  (let* ((cxns-to-punish-solutions (loop for competing-solution-node in competing-solution-cipns
                                         for competitor-cxns = (set-difference (applied-constructions competing-solution-node) (applied-constructions solution-cipn) :key #'name)
                                         append (mapcar #'original-cxn competitor-cxns)))
         (cxns-to-punish-non-solutions
          (loop for current-node in (traverse-depth-first (top-node (cip solution-cipn)) :collect-fn #'identity)
                ;when (and (field? (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)
                ;          (not (get-data (goal-test-data current-node) :result-goal-test-non-gold-standard-meaning)))
                append (loop for bad-node in (set-difference (remove (top-node (cip solution-cipn))
                                                                     (cons current-node (all-parents current-node)))
                                                             (remove (top-node (cip solution-cipn))
                                                                     (cons solution-cipn (all-parents solution-cipn)))
                                                             :key #'(lambda (node) (name (car-applied-cxn (cipn-car node)))))
                             collect (original-cxn (car-applied-cxn (cipn-car bad-node))))))
         (cxns-to-punish (remove-duplicates (append cxns-to-punish-solutions cxns-to-punish-non-solutions))))
    (dolist (cxn cxns-to-punish)
      (dec-cxn-score agent cxn :delta (get-configuration (experiment agent) :cxn-decf-score)))
    (notify cxns-punished cxns-to-punish)))
      
