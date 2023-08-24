(in-package :pf)

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
                     (extract-contributing-category (last-elt cxns-to-apply))
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
         (least-general-generalisations
          (loop with max-au-cost = (get-configuration cxn-inventory :max-au-cost)
                for cxn in filtered-hash-compatible-cxns
                ;; returns all valid form anti unification results
                for form-anti-unification-results
                  = (anti-unify-form observation-form cxn :max-au-cost max-au-cost)
                ;; returns all valid meaning anti unification results
                for meaning-anti-unification-results
                  = (anti-unify-meaning observation-meaning cxn :max-au-cost max-au-cost)
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
                finally (return (sort-anti-unification-combinations anti-unification-results)))))

    ;; 4) learn cxns(s) from the anti-unification results
    (when least-general-generalisations
      (dolist (generalisation least-general-generalisations)
        ;; the type of cxns that can be learned differ
        ;; depending on the cxn that was used for
        ;; anti-unification
        (let* ((anti-unified-cxn (first generalisation))
               (new-cxns-and-links
                (cond ((holophrase-cxn-p anti-unified-cxn)
                       (make-cxns-from-holophrase-generalisation generalisation args cxn-inventory))
                      ((holistic-cxn-p anti-unified-cxn)
                       (make-cxns-from-holistic-generalisation generalisation args cxn-inventory))
                      ((item-based-cxn-p anti-unified-cxn)
                       (make-cxns-from-item-based-generalisation generalisation args cxn-inventory)))))
          (when new-cxns-and-links
            (return new-cxns-and-links)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from holophrase generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-holophrase-generalisation (anti-unification-results args cxn-inventory)
  "When anti-unifying with a holophrase cxn, make
   1) an item-based cxn from the generalisation,
   2) a holistic cxn from the source delta, and
   3) a holistic cxn from the pattern delta (can be empty!).
   The cxns from both delta's fill the slot of the cxn from the generalisation."
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; all form-args and meaning-args
           (form-args (compute-form-args form-anti-unification anti-unified-cxn args))
           (meaning-args (compute-meaning-args meaning-anti-unification anti-unified-cxn args))
           ;; item-based cxn from generalisation
           (generalisation-cxns-and-categories
            (make-generalisation-cxn (generalisation form-anti-unification)
                                     (generalisation meaning-anti-unification)
                                     (find-data form-args :generalisation-top-lvl-args)
                                     (find-data meaning-args :generalisation-top-lvl-args)
                                     (find-data form-args :generalisation-slot-args)
                                     (find-data meaning-args :generalisation-slot-args)
                                     cxn-inventory))
           ;; holistic cxn from source delta
           (source-delta-cxns-and-categories
            (make-holistic-cxn (source-delta form-anti-unification)
                               (source-delta meaning-anti-unification)
                               (find-data form-args :source-top-lvl-args)
                               (find-data meaning-args :source-top-lvl-args)
                               cxn-inventory))
           ;; holistic cxn from pattern delta
           (pattern-delta-cxns-and-categories
            (when (and (pattern-delta form-anti-unification)
                       (pattern-delta meaning-anti-unification))
              (make-holistic-cxn (pattern-delta form-anti-unification)
                                 (pattern-delta meaning-anti-unification)
                                 (find-data form-args :pattern-top-lvl-args)
                                 (find-data meaning-args :pattern-top-lvl-args)
                                 cxn-inventory)))
           ;; build results
           (cxns-to-apply
            ;; holistic cxn source delta apply-first + item-based cxn generalisation apply-last
            (list (first source-delta-cxns-and-categories)
                  (first generalisation-cxns-and-categories)))
           (cxns-to-consolidate
            ;; holistic cxn source delta apply-last + item-based cxn generalisation apply-first
            ;; + holistic cxn pattern delta apply-first and apply-last
            (append
             (list (second source-delta-cxns-and-categories)
                   (second generalisation-cxns-and-categories))
             (when pattern-delta-cxns-and-categories
               (list (first pattern-delta-cxns-and-categories)
                     (second pattern-delta-cxns-and-categories)))))
           (categories-to-add
            ;; top lvl category source delta cxn + top lvl category generalisation cxn
            ;; + slot category generalisation cxn + top-lvl category pattern delta cxn
            (append
             (cons (third source-delta-cxns-and-categories)
                   (cons (third generalisation-cxns-and-categories)
                         (fourth generalisation-cxns-and-categories)))
             (when pattern-delta-cxns-and-categories
               (list (third pattern-delta-cxns-and-categories)))))
           (links-to-add
            ;; link between slot of generalisation cxn and source delta cxn
            ;; + link between slot of generalisation cxn and pattern delta cxn
            (append
             (list (cons (first (fourth generalisation-cxns-and-categories))
                         (third source-delta-cxns-and-categories)))
             (when pattern-delta-cxns-and-categories
               (list (cons (first (fourth generalisation-cxns-and-categories))
                           (third pattern-delta-cxns-and-categories)))))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from holistic generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-holistic-generalisation (anti-unification-results args cxn-inventory)
  "When anti-unifying with a holistic cxn, make
   1) a holistic cxn from the generalisation,
   2) an item-based cxn from the source delta, and
   3) an item-based cxn from the pattern delta (can be empty!)
   The cxn from the generalisation fills the slot of both the source delta cxn
   and the pattern delta cxn.
   Also add links such that the pattern delta cxn fills the same slots as
   the original holistic cxn used for anti-unification! This ensures that
   the previous observations are now also covered with these new cxns."
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; all form-args and meaning-args
           (form-args (compute-form-args form-anti-unification anti-unified-cxn args))
           (meaning-args (compute-meaning-args meaning-anti-unification anti-unified-cxn args))
           ;; holistic cxn from generalisation
           (generalisation-cxns-and-categories
            (make-holistic-cxn (generalisation form-anti-unification)
                               (generalisation meaning-anti-unification)
                               (find-data form-args :generalisation-slot-args)
                               (find-data meaning-args :generalisation-slot-args)
                               cxn-inventory))
           ;; item-based cxn from source delta
           (source-delta-cxns-and-categories
            (make-generalisation-cxn (source-delta form-anti-unification)
                                     (source-delta meaning-anti-unification)
                                     (find-data form-args :source-slot-args)
                                     (find-data meaning-args :source-slot-args)
                                     (find-data form-args :source-top-lvl-args)
                                     (find-data meaning-args :source-top-lvl-args)
                                     cxn-inventory))
           ;; item-based cxn from pattern delta
           (pattern-delta-cxns-and-categories
            (when (and (pattern-delta form-anti-unification)
                       (pattern-delta meaning-anti-unification))
              (make-generalisation-cxn (pattern-delta form-anti-unification)
                                       (pattern-delta meaning-anti-unification)
                                       (find-data form-args :pattern-slot-args)
                                       (find-data meaning-args :pattern-slot-args)
                                       (find-data form-args :pattern-top-lvl-args)
                                       (find-data meaning-args :pattern-top-lvl-args)
                                       cxn-inventory)))
           ;; build results
           (cxns-to-apply
            ;; holistic generalisation cxn apply-first + item-based source delta cxn apply-last
            (list (first generalisation-cxns-and-categories)
                  (first source-delta-cxns-and-categories)))
           (cxns-to-consolidate
            ;; holistic generalisation cxn apply-last + item-based source delta cxn apply-first
            ;; + item-based pattern delta cxn apply-last and apply-first
            (append
             (list (second generalisation-cxns-and-categories)
                   (second source-delta-cxns-and-categories))
             (when pattern-delta-cxns-and-categories
               (list (first pattern-delta-cxns-and-categories)
                     (second pattern-delta-cxns-and-categories)))))
           (categories-to-add
            ;; top lvl category generalisation cxn + top lvl category source delta cxn
            ;; + slot category source delta cxn + top lvl category pattern delta cxn
            ;; + slot category pattern delta cxn
            (append
             (cons (third generalisation-cxns-and-categories)
                   (cons (third source-delta-cxns-and-categories)
                         (fourth source-delta-cxns-and-categories)))
             (when pattern-delta-cxns-and-categories
               (cons (third pattern-delta-cxns-and-categories)
                     (fourth pattern-delta-cxns-and-categories)))))
           ;; link generalisation cxn to slot of source delta cxn
           ;; + link generalisation cxn to slot of pattern delta cxn
           ;; + link pattern delta cxn to slots that were filled by anti-unified cxn
           (links-to-add
            (append
             (list (cons (first (fourth source-delta-cxns-and-categories))
                         (third generalisation-cxns-and-categories)))
             (when pattern-delta-cxns-and-categories
               (append
                (list (cons (first (fourth pattern-delta-cxns-and-categories))
                            (third generalisation-cxns-and-categories)))
                (link-filler-to-previous-slots
                 anti-unified-cxn
                 (third pattern-delta-cxns-and-categories)
                 cxn-inventory))))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))

(defun link-filler-to-previous-slots (anti-unified-cxn filler-category cxn-inventory)
  (let* ((anti-unified-cxn-category (extract-top-category-holistic-cxn anti-unified-cxn))
         (slot-categories (neighbouring-categories anti-unified-cxn-category (categorial-network cxn-inventory))))
    (loop for slot in slot-categories
          collect (cons slot filler-category))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from item-based generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-item-based-generalisation (anti-unification-results args cxn-inventory)
  "When anti-unifying with an item-based cxn, make
   1) an item-based cxn from the generalisation,
   2) a holistic cxn from the source delta, and
   3) an item-based cxn from the pattern delta (can be empty!)
   The cxns from both delta's fill the slot of the cxn from the generalisation.
   Also add links such that the slots of the pattern delta cxn take the same
   fillers as the slots of the original item-based cxn used for anti-unification!
   This ensures that the previous observations are now also covered with these
   new cxns."
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; all form-args and meaning-args
           (form-args (compute-form-args form-anti-unification anti-unified-cxn args))
           (meaning-args (compute-meaning-args meaning-anti-unification anti-unified-cxn args))
           ;; item-based cxn from generalisation
           (generalisation-cxns-and-categories
            (make-generalisation-cxn (generalisation form-anti-unification)
                                     (generalisation meaning-anti-unification)
                                     (find-data form-args :generalisation-top-lvl-args)
                                     (find-data meaning-args :generalisation-top-lvl-args)
                                     (find-data form-args :generalisation-slot-args)
                                     (find-data meaning-args :generalisation-slot-args)
                                     cxn-inventory))
           ;; holistic cxn from source delta
           (source-delta-cxns-and-categories
            (make-holistic-cxn (source-delta form-anti-unification)
                               (source-delta meaning-anti-unification)
                               (find-data form-args :source-top-lvl-args)
                               (find-data meaning-args :source-top-lvl-args)
                               cxn-inventory))
           ;; item-based cxn from pattern delta
           (pattern-delta-form-arg-groups
            (loop for unit in (extract-slot-units anti-unified-cxn)
                  collect (cons (extract-category-unit unit)
                                (first (fcg-unit-feature-value unit 'form-args)))))
           (pattern-delta-meaning-arg-groups
            (loop for unit in (extract-slot-units anti-unified-cxn)
                  collect (cons (extract-category-unit unit)
                                (first (fcg-unit-feature-value unit 'meaning-args)))))
           (pattern-delta-cxns-and-categories
            (when (and (pattern-delta form-anti-unification)
                       (pattern-delta meaning-anti-unification))
              (make-generalisation-cxn-with-n-units (pattern-delta form-anti-unification)
                                                    (pattern-delta meaning-anti-unification)
                                                    (find-data form-args :pattern-top-lvl-args)
                                                    (find-data meaning-args :pattern-top-lvl-args)
                                                    (find-data form-args :pattern-slot-args)
                                                    (find-data meaning-args :pattern-slot-args)
                                                    pattern-delta-form-arg-groups
                                                    pattern-delta-meaning-arg-groups
                                                    cxn-inventory)))
           ;; build results
           (cxns-to-apply
            ;; holistic cxn source delta apply-first + item-based cxn generalisation apply-last
            (list (first source-delta-cxns-and-categories)
                  (first generalisation-cxns-and-categories)))
           (cxns-to-consolidate
            ;; holistic cxn source delta apply-last + item-based cxn generalisation apply-first
            ;; + item-based cxn pattern delta apply-last and apply-first
            (append
             (list (second source-delta-cxns-and-categories)
                   (second generalisation-cxns-and-categories))
             (when pattern-delta-cxns-and-categories
               (list (first pattern-delta-cxns-and-categories)
                     (second pattern-delta-cxns-and-categories)))))
           (categories-to-add
            ;; top lvl category source delta cxn + top lvl category generalisation cxn
            ;; + slot category generalisation cxn
            ;; + top lvl category pattern delta cxn + slot categories pattern delta cxn
            (append
             (cons (third source-delta-cxns-and-categories)
                   (cons (third generalisation-cxns-and-categories)
                         (fourth generalisation-cxns-and-categories)))
             (when pattern-delta-cxns-and-categories
               (cons (third pattern-delta-cxns-and-categories)
                     (fourth pattern-delta-cxns-and-categories)))))
           (links-to-add
            ;; link between slot of generalisation cxn and source delta cxn
            ;; + link between slot of generalisation cxn and pattern delta cxn
            ;; + link between slots of pattern delta cxn and fillers of anti-unified item-based cxn!
            (append
             (list (cons (first (fourth generalisation-cxns-and-categories))
                         (third source-delta-cxns-and-categories)))
             (when pattern-delta-cxns-and-categories
               (append
                (list (cons (first (fourth generalisation-cxns-and-categories))
                            (third pattern-delta-cxns-and-categories)))
                (link-slots-to-previous-fillers
                 pattern-delta-meaning-arg-groups
                 (first pattern-delta-cxns-and-categories)
                 cxn-inventory))))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))

(defun link-slots-to-previous-fillers (arg-groups new-cxn cxn-inventory)
  "Link the slots of the new item-based cxn to the fillers of the slots of
   the item-based cxn that was used for anti-unification. To know which slot
   should take which fillers, use the grammatical category that is present in
   the top-arg/slot-arg predicates."
  (loop for arg-group in arg-groups
        for category-anti-unified-cxn = (first arg-group)
        for args = (rest arg-group)
        for fillers-anti-unified-cxn-slots
          = (neighbouring-categories category-anti-unified-cxn (categorial-network cxn-inventory))
        for unit = (loop for unit in (conditional-part new-cxn)
                         when (or (equal args (first (fcg-unit-feature-value unit 'meaning-args)))
                                  (equal args (first (fcg-unit-feature-value unit 'form-args))))
                           return unit)
        for unit-category = (extract-category-unit unit)
        append (loop for filler in fillers-anti-unified-cxn-slots
                     collect (cons filler unit-category))))
