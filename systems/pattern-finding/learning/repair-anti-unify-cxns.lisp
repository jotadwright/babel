(in-package :pattern-finding)


(defclass anti-unify-cxns (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair anti-unify-cxns)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by anti-unifying the observation with the subset of cxns
   that results in the smallest generalisation."
  (let ((cxns-and-categorial-links (create-cxns-by-anti-unification problem node)))
    (when cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))


(defun create-cxns-by-anti-unification (problem node)
  (do-repair
   (get-data problem :utterance)
   (get-data problem :meaning)
   nil
   nil
   (construction-inventory node)
   node
   'anti-unify-cxns))


(defmethod do-repair (observation-form observation-meaning form-args meaning-args
                                       (cxn-inventory construction-inventory)
                                       node (repair-type (eql 'anti-unify-cxns)))
  (apply-fix ...))


;; TO DO
;; Information from partial analysis is not NOW used
;; Find the best partial analysis node and try to learn from that one??
;; How to define "the best" partial analysis?


(defun find-cxns-and-anti-unify (observation-form observation-meaning processing-direction cxn-inventory)
  ;; return new cxns and links
  
  (let* (;; 1) select cxns by hasing the observation according to processing direction
         (hash-compatible-cxns
          (case processing-direction
            (<- (constructions-for-anti-unification-hashed observation-form nil cxn-inventory))
            (-> (constructions-for-anti-unification-hashed nil observation-meaning cxn-inventory))))
         ;; 2) make powerset of hash compatible cxns, sort by length and by score
         (cxn-sets-for-anti-unification
          (sort #'(lambda (cxn-set-1 cxn-set-2)
                    ;; if two cxn sets have the same length
                    ;; take the one with the highest average score
                    (if (length= cxn-set-1 cxn-set-2)
                      (let ((avg-score-1 (average (mapcar #'get-cxn-score cxn-set-1)))
                            (avg-score-2 (average (mapcar #'get-cxn-score cxn-set-2))))
                        (> avg-score-1 avg-score-2))
                      (length> cxn-set-1 cxn-set-2)))
                (all-subsets hash-compatible-cxns)))
         ;; 3) anti-unify each set of cxns with the observation and take the one with lowest cost
         ;;    pattern-matching requires string, so need to render the observation and the cxn forms
         ;;    but rendering can return multiple solutions, so try out all combinations
         ;;    TO DO: pattern-matching does not assign costs to its results!
         ;;     so cannot select the best one at this point...
         (source-forms
          (render-all observation-form :render-sequences))
         (best-anti-unification-results
          (loop for cxn-set in cxn-sets-for-anti-unification
                for pattern-forms = (render-all
                                     (loop for cxn in cxn-set
                                           append (extract-form-predicates cxn))
                                     :render-sequences)
                for pattern-meaning = (loop for cxn in cxn-set
                                            append (extract-meaning-predicates cxn))
                for form-anti-unification-results = (loop for (pattern-form source-form) in (combinations source-forms pattern-forms)
                                                          collect (multiple-value-list (generalise-strings pattern-form source-form))) 
                for meaning-anti-unification-results = (fcg:anti-unify-predicate-network cxn-set-meaning observation-meaning)
        
        
