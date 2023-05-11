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
  ;; call to apply-fix in the end
  )


;; TO DO
;; Information from partial analysis is not NOW used
;; Find the best partial analysis node and try to learn from that one??
;; How to define "the best" partial analysis?

;; Taking the lowest anti-unification cost with partial analyses is not
;; a good solution, since the cost will be high instead of low! With
;; partial analyses, the generalisation are the parts that are identical,
;; while the rest is in the delta's. Often there will be more in the rest
;; than in the overlap.


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
         (possible-source-forms
          (render-all observation-form :render-sequences))
         (best-form-anti-unification-result
          (loop for cxn-set in cxn-sets-for-anti-unification
                for possible-pattern-forms
                = (render-all
                   (loop for cxn in cxn-set
                         append (extract-form-predicates cxn))
                   :render-sequences)
                for form-anti-unification-results
                = (loop for (pattern-form source-form) in (combinations possible-source-forms possible-pattern-forms)
                        collect (multiple-value-list (fcg::anti-unify-strings pattern-form source-form)))
                ;; sort by cost
                return (first (sort form-anti-unification-results #'< :key #'fourth))))
         (best-meaning-anti-unification-result
          (loop for cxn-set in cxn-sets-for-anti-unification
                for pattern-meaning
                = (loop for cxn in cxn-set
                        append (extract-meaning-predicates cxn))
                for meaning-anti-unification-results
                = (fcg:anti-unify-predicate-network pattern-meaning observation-meaning)
                return (first (sort meaning-anti-unification-results #'< :key #'cost))))
         ;; 4) learn cxn(s) from the anti-unification results
         ;;    - no anti-unification results -> learn holistic
         ;;    - yes anti-unification results -> learn cxns for generalisation and delta's
         )
    (values best-form-anti-unification-result best-meaning-anti-unification-result)))
        
        
