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
;; Information from partial analysis is now NOT used
;; Find the best partial analysis node and try to learn from that one??
;; How to define "the best" partial analysis?

;; Taking the lowest anti-unification cost with partial analyses is not
;; a good solution, since the cost will be high instead of low! With
;; partial analyses, the generalisation are the parts that are identical,
;; while the rest is in the delta's. Often there will be more in the rest
;; than in the overlap.


(defun anti-unify-form (observation set-of-cxns)
  "Anti-unify the observation with the given set of cxns
   on the form side."
  ;; anti-unify-string requires string, so need to render the observation and the cxn forms
  ;; but rendering can return multiple solutions, so try out all combinations
  (let* ((possible-source-forms (first (render-all observation :render-sequences)))
         (cxn-forms (loop for cxn in set-of-cxns
                          append (fcg::extract-form-predicates cxn)))
         (possible-target-forms (first (render-all cxn-forms :render-sequences)))
         (anti-unification-results
          (loop for (pattern-form source-form) in (combinations possible-source-forms possible-target-forms)
                append (fcg::anti-unify-strings pattern-form source-form :to-sequence-predicates-p t))))
    (first (sort anti-unification-results #'< :key #'fcg::cost))))

(defun anti-unify-meaning (source-meaning set-of-cxns)
  "Anti-unify the observation with the given set of cxns
   on the meaning side."
  (let* ((pattern-meaning (loop for cxn in set-of-cxns
                                append (fcg::extract-meaning-predicates cxn)))
         (anti-unification-results
          (fcg::anti-unify-predicate-network pattern-meaning source-meaning)))
    (first anti-unification-results)))

(defun sort-anti-unification-results (list-of-anti-unification-results)
  "Sort the anti-unifcation results based on cost (of both form- and
   meaning-anti-unification) and avg cxn score as a tie breaker."
  (labels ((total-au-cost (combined-anti-unification-result)
             (let ((avg-cxn-score (average (mapcar #'cxn-score (first combined-anti-unification-result))))
                   (form-au-cost (fcg::cost (second combined-anti-unification-result)))
                   (meaning-au-cost (fcg::cost (third combined-anti-unification-result))))
               (+ form-au-cost meaning-au-cost (- 1 avg-cxn-score)))))
    (sort list-of-anti-unification-results #'< :key #'total-au-cost)))

(defun find-cxns-and-anti-unify (observation-form observation-meaning cxn-inventory)
  "Given form and meaning of an observation and a cxn inventory,
   find the set of cxns that leads to the smallest generalisation
   and learn new cxn(s) from this generalisation."
  (if (null (constructions cxn-inventory))
    ;; if the cxn inventory is empty, immediately learn a holistic cxn
    ;; for the entire observation
    (make-holistic-cxn observation-form observation-meaning cxn-inventory)
    (let* (;; 1) select cxns by hasing the observation according to processing direction
           ;;    How to hash cxns using the sequence predicates?
           ;;    The :test function of the hash table should be substringp
           ;(hash-compatible-cxns
           ; (case processing-direction
           ;   (<- (constructions-for-anti-unification-hashed observation-form nil cxn-inventory))
           ;   (-> (constructions-for-anti-unification-hashed nil observation-meaning cxn-inventory))))
         
           ;; 2) make powerset of (hash compatible) cxns, sort by length and by score
           (routine-cxns-with-positive-score
            (remove-if-not #'non-zero-cxn-p (remove-if-not #'routine-cxn-p (constructions cxn-inventory))))
           (cxn-sets-for-anti-unification
            (sort (all-subsets routine-cxns-with-positive-score)
                  #'(lambda (cxn-set-1 cxn-set-2)
                      ;; if two cxn sets have the same length
                      ;; take the one with the highest average score
                      (if (length= cxn-set-1 cxn-set-2)
                        (let ((avg-score-1 (average (mapcar #'get-cxn-score cxn-set-1)))
                              (avg-score-2 (average (mapcar #'get-cxn-score cxn-set-2))))
                          (> avg-score-1 avg-score-2))
                        (length> cxn-set-1 cxn-set-2)))))
           
           ;; 3) anti-unify each set of cxns with the observation
           ;;    and take the one with lowest cost
           (least-general-generalisation
            (loop for set-of-cxns in cxn-sets-for-anti-unification
                  for best-form-anti-unification-result
                    = (anti-unify-form observation-form set-of-cxns)
                  for best-meaning-anti-unification-result
                    = (anti-unify-meaning observation-meaning set-of-cxns)
                  when (and best-form-anti-unification-result
                            best-meaning-anti-unification-result)
                  collect (list set-of-cxns
                                best-form-anti-unification-result
                                best-meaning-anti-unification-result)
                      into anti-unification-results
                  finally (return (first (sort-anti-unification-results anti-unification-results)))))
           
           ;; 4) learn cxn(s) from the anti-unification results
           ;;    - no anti-unification results -> learn holistic
           ;;    - yes anti-unification results -> learn cxns for generalisation and delta's
           (new-cxn-and-links
            (if least-general-generalisation
              (make-cxns-from-generalisations least-general-generalisation
                                              observation-form
                                              observation-meaning
                                              cxn-inventory)
              (make-holistic-cxn observation-form observation-meaning cxn-inventory))))
    new-cxn-and-links)))

(defun make-cxns-from-generalisations (anti-unification-results observation-form observation-meaning cxn-inventory)
  (destructuring-bind (anti-unified-cxns
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* ((cxn-inventory (original-cxn-set cxn-inventory))
           ;; all form-args and meaning-args
           (top-lvl-form-args (get-unconnected-vars observation-form))
           (top-lvl-meaning-args (get-unconnected-vars observation-meaning))
           (slot-form-args (compute-slot-args form-anti-unification))
           (slot-meaning-args (compute-slot-args meaning-anti-unification))
           ;; lex classes (top-lvl and slot)
           ;; dispatch to helper functions to make generalisation-cxn and delta cxns
           )
      )))


(defun compute-slot-args (anti-unification-result)
  (with-slots (generalisation
               pattern-bindings
               source-bindings
               pattern-delta
               source-delta) anti-unification-result
    (let* ((cxn-args (make-blackboard))
          ;; unique vars in deltas
          (pattern-delta-vars
           (remove-duplicates (find-all-anywhere-if #'variable-p pattern-delta)))
          (source-delta-vars
           (remove-duplicates (find-all-anywhere-if #'variable-p source-delta)))
          ;; vars that connect delta's to generalisation
          (pattern-delta-connection-vars
           (loop for var in pattern-delta-vars
                 when (assoc var pattern-bindings)
                 collect var))
          (source-delta-connection-vars
           (loop for var in source-delta-vars
                 when (assoc var source-bindings)
                 collect var))
          ;; vars that were decoupled
          (pattern-delta-decoupled-link-vars
            (loop for (binding . rest) on pattern-bindings
                  when (find (car binding) rest :key #'car :test #'equalp)
                  collect (car binding)))
          (source-delta-decoupled-link-vars
            (loop for (binding . rest) on source-bindings
                  when (find (car binding) rest :key #'car :test #'equalp)
                  collect (car binding)))
          ;; lengths of vars
          (length-all-pattern-delta-vars
            (reduce #'+ (mapcar #'length (list pattern-delta-connection-vars
                                               pattern-delta-decoupled-link-vars))))
          (length-all-source-delta-vars
           (reduce #'+ (mapcar #'length (list source-delta-connection-vars
                                              source-delta-decoupled-link-vars)))))
      ;; the pattern-cxn, source-cxn, and slot-unit in the generalisation cxn
      ;; need to have the same number of args
      (multiple-value-bind (longest-delta-key other-delta-key longest-bindings other-bindings)
          (if (> length-all-pattern-delta-vars length-all-source-delta-vars)
            (values :pattern-args :source-args pattern-bindings source-bindings)
            (values :source-args :pattern-args source-bindings pattern-bindings))
        ;; handle connection vars
        (loop with longest-var-list = (case longest-delta-key
                                        (:pattern-args pattern-delta-connection-vars)
                                        (:source-args source-delta-connection-vars))
              for var in longest-var-list
              for var-in-generalisation = (rest (assoc var longest-bindings))
              for var-in-other-delta = (first (rassoc var-in-generalisation other-bindings))
              do (push-data cxn-args longest-delta-key var)
                 (push-data cxn-args :generalisation-slot-args var-in-generalisation)
                 (push-data cxn-args other-delta-key var-in-other-delta))
        ;; handle decoupled link vars
        (loop with longest-var-list = (case longest-delta-key
                                        (:pattern-args pattern-delta-decoupled-link-vars)
                                        (:source-args source-delta-decoupled-link-vars))
              for var in longest-var-list
              for vars-in-generalisation = (mapcar #'cdr (find-all var longest-bindings :key #'car))
              for vars-in-other-delta = (loop for v in vars-in-generalisation
                                              collect (first (rassoc v other-bindings)))
              for vars-in-longest-delta = (make-list (length vars-in-generalisation) :initial-element var)
              do (loop for x in vars-in-longest-delta
                       for y in vars-in-generalisation
                       for z in vars-in-other-delta
                       unless (and (member x (get-data cxn-args longest-delta-key))
                                   (member y (get-data cxn-args :generalisation-slot-args))
                                   (member z (get-data cxn-args other-delta-key)))
                       do (push-data cxn-args longest-delta-key x)
                          (push-data cxn-args :generalisation-slot-args y)
                          (push-data cxn-args other-delta-key z)))
        ;; return result
        (data-fields cxn-args)))))
          
        


        
(defun make-holistic-cxn (form meaning cxn-inventory)
  (let* (;(cxn-inventory (original-cxn-set cxn-inventory))
         ;; make the cxn names
         (cxn-name
          (make-cxn-name form cxn-inventory :add-numeric-tail t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" cxn-name))))
         ;; unit name
         (unit-name-holistic-cxn
          (make-unit-name (mkstr cxn-name) cxn-inventory :trim-cxn-suffix t))
         ;; lex class
         (lex-class-holistic-cxn
          (make-lex-class cxn-name :trim-cxn-suffix t))
         ;; temp cxn inventory
         (cxn-inventory-copy
          (copy-object cxn-inventory))
         ;; args
         (meaning-args (get-unconnected-vars meaning))
         (form-args (get-unconnected-vars form))
         ;; apply first cxn
         (holistic-cxn-apply-first
          (second
           (multiple-value-list
            (eval
             (holistic-cxn-apply-first-skeleton cxn-name cxn-name-apply-first
                                                unit-name-holistic-cxn lex-class-holistic-cxn
                                                form meaning form-args meaning-args
                                                (get-configuration cxn-inventory :initial-cxn-score)
                                                cxn-inventory-copy)))))
         ;; apply last cxn
         (holistic-cxn-apply-last
          (second
           (multiple-value-list
            (eval
             (holistic-cxn-apply-last-skeleton cxn-name cxn-name-apply-last
                                               unit-name-holistic-cxn lex-class-holistic-cxn
                                               form meaning form-args meaning-args
                                               (get-configuration cxn-inventory :initial-cxn-score)
                                               cxn-inventory-copy)))))
         (cxns-to-apply (list holistic-cxn-apply-first))
         (cxns-to-consolidate (list holistic-cxn-apply-last))
         (cats-to-add (list lex-class-holistic-cxn))
         (links-to-add nil))

    (list cxns-to-apply cxns-to-consolidate cats-to-add links-to-add)))
        
(defun holistic-cxn-apply-first-skeleton (bare-cxn-name cxn-name unit-name lex-class
                                                        form meaning form-args meaning-args
                                                        initial-cxn-score cxn-inventory)
  `(def-fcg-cxn ,cxn-name
               ((,unit-name
                 (form-args ,form-args)
                 (meaning-args ,meaning-args)
                 (syn-cat (phrase-type holistic)
                          (lex-class ,lex-class)))
                <-
                (,unit-name
                 (HASH meaning ,meaning)
                 --
                 (HASH form ,form)))
               :attributes (:label fcg::routine
                            :cxn-type holistic
                            :bare-cxn-name ,bare-cxn-name)
               :score ,initial-cxn-score
               :cxn-inventory ,cxn-inventory))

(defun holistic-cxn-apply-last-skeleton (bare-cxn-name cxn-name unit-name lex-class
                                                       form meaning form-args meaning-args
                                                       initial-cxn-score cxn-inventory)
  `(def-fcg-cxn ,cxn-name
                (<-
                 (,unit-name
                  (HASH meaning ,meaning)
                  (meaning-args ,meaning-args)
                  (syn-cat (phrase-type holistic)
                           (lex-class ,lex-class))
                  --
                  (HASH form ,form)
                  (form-args ,form-args)
                  (syn-cat (phrase-type holistic)
                           (lex-class ,lex-class))))
                :attributes (:label fcg::meta-only
                             :cxn-type holistic
                             :bare-cxn-name ,bare-cxn-name)
                :score ,initial-cxn-score
                :cxn-inventory ,cxn-inventory))
  