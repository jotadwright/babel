(in-package :pattern-finding-old)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair anti-unify cipn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass anti-unify-cipn (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair anti-unify-cipn)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  (let ((cxns-and-categorial-links
         (do-repair
          (get-data problem :utterance)
          (get-data problem :meaning)
          (make-blackboard)
          (construction-inventory node)
          node
          'anti-unify-cipn)))
    (when cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))


(defmethod do-repair (observation-form observation-meaning (args blackboard) (cxn-inventory construction-inventory) node (repair-type (eql 'anti-unify-cipn)))
  (when (constructions cxn-inventory)
    (let ((new-cxns-and-links (find-cipn-and-anti-unify observation-form observation-meaning args (original-cxn-set cxn-inventory))))
      (when new-cxns-and-links
        (destructuring-bind (cxns-to-apply cxns-to-consolidate cats-to-add cat-links-to-add) new-cxns-and-links
          (apply-fix observation-form
                     cxns-to-apply
                     cat-links-to-add
                     cxns-to-consolidate
                     cats-to-add
                     (extract-contributing-lex-class (last-elt cxns-to-apply))
                     t
                     node
                     repair-type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find ts and anti-unify ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-cipn-and-anti-unify (observation-form observation-meaning (args blackboard) (cxn-inventory fcg-construction-set))
  "Given form and meaning of an observation and a cxn inventory,
   find the best transient structure that partially covers the observation
   and learn new cxn(s) from the remainder"
  (let* (;; 1) run comprehension (routine and meta set) and select the best compatible partial analysis
         (cipns-with-routine-cxns (compatible-cipns-with-routine-cxns observation-form observation-meaning cxn-inventory))
         (cipns-with-meta-cxns (compatible-cipns-with-meta-cxns observation-form observation-meaning cxn-inventory))
         (all-partial-analysis-cipns (append cipns-with-routine-cxns cipns-with-meta-cxns))
         (partial-analysis-cipn (first (sort-cipns-by-coverage-and-nr-of-applied-cxns all-partial-analysis-cipns))))
    ;; 2) anti-unify the transient structure with the observation!
    (when partial-analysis-cipn
      (let* (;; do we want the same max cost??
             (max-au-cost (get-configuration cxn-inventory :max-au-cost))
             ;; when are these AU results valid/invalid??
             (form-anti-unification-results
              (anti-unify-form observation-form partial-analysis-cipn args max-au-cost))
             (meaning-anti-unification-results
              (anti-unify-meaning observation-meaning partial-analysis-cipn args max-au-cost))
             (all-anti-unification-combinations
              (remove-if-not #'valid-au-combination-p
                             (combinations meaning-anti-unification-results
                                           form-anti-unification-results)))
             ;; how to sort AU results??
             (best-anti-unification-combination
              (first (sort-anti-unification-combinations all-anti-unification-combinations)))
             (best-anti-unification-result
              (when best-anti-unification-combination
                (cons partial-analysis-cipn best-anti-unification-combination))))
        ;; 3) when there are anti-unification results, learn cxns from them!
        (when best-anti-unification-result
          (destructuring-bind (anti-unified-cipn
                               form-anti-unification
                               meaning-anti-unification) best-anti-unification-result
            (declare (ignore anti-unified-cipn))
            (copy-arg-predicates form-anti-unification)
            (copy-arg-predicates meaning-anti-unification)
            (let (;; all form-args and meaning-args
                  (form-args (compute-args form-anti-unification))
                  (meaning-args (compute-args meaning-anti-unification)))
              (cond ((and (find 'top-arg (pattern-delta form-anti-unification) :key #'first)
                          (find 'top-arg (pattern-delta meaning-anti-unification) :key #'first)
                          ;(find 'slot-arg (pattern-delta form-anti-unification) :key #'first)
                          (find 'slot-arg (pattern-delta meaning-anti-unification) :key #'first))
                     (make-holistic-cxns-from-partial-analysis best-anti-unification-result observation-form observation-meaning
                                                               form-args meaning-args cxn-inventory))
                    ((and (find 'slot-arg (pattern-delta form-anti-unification) :key #'first)
                          (find 'slot-arg (pattern-delta meaning-anti-unification) :key #'first))
                     (make-item-based-cxn-from-partial-analysis best-anti-unification-result observation-form observation-meaning
                                                                form-args meaning-args cxn-inventory))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from partial analysis ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-item-based-cxn-from-partial-analysis (anti-unification-results observation-form observation-meaning form-args meaning-args cxn-inventory)
  (destructuring-bind (anti-unified-cipn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* ((source-delta-form-arg-groups
            (group-slot-args-into-units (source-delta form-anti-unification)))
           (source-delta-meaning-arg-groups
            (group-slot-args-into-units (source-delta meaning-anti-unification)))
           ;; learn cxns from source delta
           (source-delta-cxns-and-categories
            (make-generalisation-cxn-with-n-units (remove-arg-predicates (source-delta form-anti-unification))
                                                  (remove-arg-predicates (source-delta meaning-anti-unification))
                                                  (find-data form-args :source-top-lvl-args)
                                                  (find-data meaning-args :source-top-lvl-args)
                                                  (find-data form-args :source-slot-args)
                                                  (find-data meaning-args :source-slot-args)
                                                  source-delta-form-arg-groups
                                                  source-delta-meaning-arg-groups
                                                  cxn-inventory))
           ;; apply new cxns in sandbox to extract categorial links
           (applied-cxns
            (loop for cxn in (applied-constructions anti-unified-cipn)
                  collect (original-cxn (if (routine-cxn-p cxn) cxn
                                          (alter-ego-cxn cxn (construction-inventory anti-unified-cipn))))))
           (sandbox-cxns
            (cons (first source-delta-cxns-and-categories) applied-cxns))
           (sandbox-categories
            (append (mappend #'extract-conditional-lex-classes sandbox-cxns)
                    (mapcar #'extract-contributing-lex-class sandbox-cxns)))
           (sandbox-cipn
            (comprehend-in-sandbox observation-form cxn-inventory
                                   :gold-standard-meaning observation-meaning
                                   :cxns-to-add sandbox-cxns
                                   :categories-to-add sandbox-categories))
           ;; build results
           (cxns-to-apply sandbox-cxns)
           (cxns-to-consolidate (list (second source-delta-cxns-and-categories)))
           (categories-to-add
            (remove nil
                    (append (list (third source-delta-cxns-and-categories))
                            (fourth source-delta-cxns-and-categories))))
           (links-to-add (extract-used-categorial-links sandbox-cipn)))
      ;; done!
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))

(defun make-holistic-cxns-from-partial-analysis (anti-unification-results observation-form observation-meaning form-args meaning-args cxn-inventory)
  (destructuring-bind (anti-unified-cipn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* ((source-delta-form-arg-groups
            (group-top-args-into-units (source-delta form-anti-unification)))
           (source-delta-meaning-arg-groups
            (group-top-args-into-units (source-delta meaning-anti-unification)))
           ;; learn cxns from source delta
           (source-delta-cxns-and-categories
            (make-n-holistic-cxns (remove-arg-predicates (source-delta form-anti-unification))
                                  (remove-arg-predicates (source-delta meaning-anti-unification))
                                  (find-data form-args :source-top-lvl-args)
                                  (find-data meaning-args :source-top-lvl-args)
                                  source-delta-form-arg-groups
                                  source-delta-meaning-arg-groups
                                  cxn-inventory))
           ;; apply new cxns in sandbox to extract categorial links
           (applied-cxns
            (loop for cxn in (applied-constructions anti-unified-cipn)
                  collect (original-cxn (if (routine-cxn-p cxn) cxn
                                          (alter-ego-cxn cxn (construction-inventory anti-unified-cipn))))))
           (sandbox-cxns
            (append (first source-delta-cxns-and-categories) applied-cxns))
           (sandbox-categories
            (append (mappend #'extract-conditional-lex-classes sandbox-cxns)
                    (mapcar #'extract-contributing-lex-class sandbox-cxns)))
           (sandbox-cipn
            (comprehend-in-sandbox observation-form cxn-inventory
                                   :gold-standard-meaning observation-meaning
                                   :cxns-to-add sandbox-cxns
                                   :categories-to-add sandbox-categories))
           ;; build results
           (cxns-to-apply sandbox-cxns)
           (cxns-to-consolidate (second source-delta-cxns-and-categories))
           (categories-to-add (third source-delta-cxns-and-categories))
           (links-to-add (extract-used-categorial-links sandbox-cipn)))
      ;; done!
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))