(in-package :pattern-finding-old)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair anti-unify cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass anti-unify-cxns (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair anti-unify-cxns)
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
          'anti-unify-cxns)))
    (when cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))


(defmethod do-repair (observation-form observation-meaning (args blackboard) (cxn-inventory construction-inventory) node (repair-type (eql 'anti-unify-cxns)))
  (when (constructions cxn-inventory)
    (let ((new-cxns-and-links (find-cxns-and-anti-unify observation-form observation-meaning args (original-cxn-set cxn-inventory))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find cxns and anti-unify ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-cxns-and-anti-unify (observation-form observation-meaning (args blackboard) (cxn-inventory fcg-construction-set))
  "Given form and meaning of an observation and a cxn inventory,
   find the cxn that leads to the smallest generalisation
   and learn new cxn(s) from this generalisation."
  (let* (;; 1) select cxns by hasing the observation
         ;;    only form is provided since we are learning in comprehension
         (hash-compatible-cxns
          (constructions-for-anti-unification-hashed observation-form nil cxn-inventory))
         
         ;; 2) filter hash-compatible cxns for routine cxns with a positive score
         (filtered-hash-compatible-cxns
          (remove-if-not #'non-zero-cxn-p
                         (remove-if-not #'routine-cxn-p
                                        hash-compatible-cxns)))
           
         ;; 3) find the least general generalisation through anti-unification
         (least-general-generalisation
          (loop with sorted-cxns = (sort filtered-hash-compatible-cxns #'> :key #'get-cxn-score)
                with max-au-cost = (get-configuration cxn-inventory :max-au-cost)
                for cxn in sorted-cxns
                ;; returns all valid form anti unification results
                for form-anti-unification-results
                  = (anti-unify-form observation-form cxn args max-au-cost)
                ;; returns all valid meaning anti unification results
                for meaning-anti-unification-results
                  = (anti-unify-meaning observation-meaning cxn args max-au-cost)
                ;; make all combinations and filter for valid combinations
                for all-anti-unification-combinations
                  = (remove-if-not #'valid-au-combination-p
                                   (combinations meaning-anti-unification-results
                                                 form-anti-unification-results))
                when all-anti-unification-combinations
                ;; store all valid combinations with the cxn used for anti unification
                append (loop for combo in all-anti-unification-combinations
                             collect (cons cxn combo))
                into anti-unification-results
                ;; return the best anti unification combination (costs and cxn score)
                finally (return (first (sort-anti-unification-combinations anti-unification-results))))))
    
    ;; 4) learn cxn(s) from the anti-unification results
    (when least-general-generalisation
      (cond ((and (au-all-parts-present-p (second least-general-generalisation))
                  (au-all-parts-present-p (third least-general-generalisation)))
             (make-cxns-from-generalisations least-general-generalisation cxn-inventory))
            ((and (au-partial-analysis-p (second least-general-generalisation))
                  (au-partial-analysis-p (third least-general-generalisation)))
             (break) ;; TODO
             )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-generalisations (anti-unification-results cxn-inventory)
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; all form-args and meaning-args
           (form-args (compute-args form-anti-unification 'form))
           (meaning-args (compute-args meaning-anti-unification 'meaning))
           ;; dispatch to helper functions to make generalisation-cxn and delta cxns
           ;; generalisation cxn is always item-based!
           (generalisation-cxns-and-categories
            (make-generalisation-cxn (remove-arg-predicates (generalisation form-anti-unification))
                                     (remove-arg-predicates (generalisation meaning-anti-unification))
                                     (find-data form-args :generalisation-top-lvl-args)
                                     (find-data meaning-args :generalisation-top-lvl-args)
                                     (find-data form-args :generalisation-slot-args)
                                     (find-data meaning-args :generalisation-slot-args)
                                     cxn-inventory))
           ;; for learning cxns from delta's;
           ;; if there are slot-arg predicates in the delta
           ;;   learn an item-based cxn wih as many slot units as there are arg-groups
           ;;   these slot units should take the same fillers as the item-based cxn that was used for anti-unification!
           ;; if there are top-arg predicates in the delta
           ;;   learn as many holistic cxns as there are arg-groups
           (source-delta-cxns-and-categories
            (make-holistic-cxn (remove-arg-predicates (source-delta form-anti-unification))
                               (remove-arg-predicates (source-delta meaning-anti-unification))
                               (find-data form-args :source-top-lvl-args)
                               (find-data meaning-args :source-top-lvl-args)
                               cxn-inventory))

           (pattern-delta-form-arg-groups
            (loop with groups = (group-slot-args-into-units (pattern-delta form-anti-unification))
                  for group in groups
                  collect (cons (first group) (reverse (rest group)))))
           (pattern-delta-meaning-arg-groups
            (loop with groups = (group-slot-args-into-units (pattern-delta meaning-anti-unification))
                  for group in groups
                  collect (cons (first group) (reverse (rest group)))))
           (pattern-delta-cxns-and-categories
            (cond ((or (find 'slot-arg (pattern-delta form-anti-unification) :key #'first)
                       (find 'slot-arg (pattern-delta meaning-anti-unification) :key #'first))
                   (make-generalisation-cxn-with-n-units (remove-arg-predicates (pattern-delta form-anti-unification))
                                                         (remove-arg-predicates (pattern-delta meaning-anti-unification))
                                                         (find-data form-args :pattern-top-lvl-args)
                                                         (find-data meaning-args :pattern-top-lvl-args)
                                                         (find-data form-args :pattern-slot-args)
                                                         (find-data meaning-args :pattern-slot-args)
                                                         pattern-delta-form-arg-groups
                                                         pattern-delta-meaning-arg-groups
                                                         cxn-inventory))
                  ((or (find 'top-arg (pattern-delta form-anti-unification) :key #'first)
                       (find 'top-arg (pattern-delta meaning-anti-unification) :key #'first))
                   (make-holistic-cxn (remove-arg-predicates (pattern-delta form-anti-unification))
                                      (remove-arg-predicates (pattern-delta meaning-anti-unification))
                                      (find-data form-args :pattern-top-lvl-args)
                                      (find-data meaning-args :pattern-top-lvl-args)
                                      cxn-inventory))))
           ;; make holistic cxn
           ;; => holistic-cxn-apply-first, holistic-cxn-apply-last, lex-class
           ;; make generalisation cxn
           ;; => item-based-cxn-apply-last, item-based-cxn-apply-first,
           ;;    lex-class-item-based, lex-classes-slots
           
           ;; build result
           (cxns-to-apply
            (list (first source-delta-cxns-and-categories)
                  (first generalisation-cxns-and-categories)))
           (cxns-to-consolidate
            (list (second source-delta-cxns-and-categories)
                  (second generalisation-cxns-and-categories)
                  (first pattern-delta-cxns-and-categories)
                  (second pattern-delta-cxns-and-categories)))
           (categories-to-add
            (append (list (third source-delta-cxns-and-categories))
                    (append (list (third generalisation-cxns-and-categories))
                            (fourth generalisation-cxns-and-categories))
                    (if (holistic-cxn-p anti-unified-cxn)
                      (list (third pattern-delta-cxns-and-categories))
                      (append (list (third pattern-delta-cxns-and-categories))
                              (fourth pattern-delta-cxns-and-categories)))))
           (links-to-add
            (if (holistic-cxn-p anti-unified-cxn)
              (list (cons (first (fourth generalisation-cxns-and-categories))
                          (third pattern-delta-cxns-and-categories))
                    (cons (first (fourth generalisation-cxns-and-categories))
                          (third source-delta-cxns-and-categories)))
              (append (list (cons (first (fourth generalisation-cxns-and-categories))
                                  (third source-delta-cxns-and-categories))
                            (cons (first (fourth generalisation-cxns-and-categories))
                                  (third pattern-delta-cxns-and-categories)))
                      (links-to-neighbouring-categories
                       pattern-delta-meaning-arg-groups
                       (first pattern-delta-cxns-and-categories)
                       cxn-inventory)))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))

(defun links-to-neighbouring-categories (arg-groups new-cxn cxn-inventory)
  (loop for arg-group in arg-groups
        for lex-class-anti-unified-cxn = (first arg-group)
        for args = (rest arg-group)
        for fillers-anti-unified-cxn-slots
          = (neighbouring-categories lex-class-anti-unified-cxn (categorial-network cxn-inventory))
        for unit = (loop for unit in (conditional-part new-cxn)
                         when (or (equal args (first (fcg-unit-feature-value unit 'meaning-args)))
                                  (equal args (first (fcg-unit-feature-value unit 'form-args))))
                           return unit)
        for unit-lex-class = (extract-lex-class-unit unit)
        append (loop for filler in fillers-anti-unified-cxn-slots
                     collect (cons filler unit-lex-class))))

