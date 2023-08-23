(in-package :pf)

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
                     cxns-to-consolidate
                     cats-to-add
                     cat-links-to-add
                     (extract-contributing-category (last-elt cxns-to-apply))
                     t
                     node
                     repair-type))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find cipn and anti-unify ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod find-cipn-and-anti-unify (observation-form observation-meaning (args blackboard) (cxn-inventory fcg-construction-set))
  "Given form and meaning of an observation and a cxn inventory,
   find the best transient structure that partially covers the observation
   and learn new cxn(s) from the remainder"
  (let* (;; 1) run comprehension (routine and meta set)
         ;;    and obtain partial analyses
         (cipns-with-routine-cxns (compatible-cipns-with-routine-cxns observation-form observation-meaning cxn-inventory))
         (cipns-with-meta-cxns (compatible-cipns-with-meta-cxns observation-form observation-meaning cxn-inventory))
         (partial-analysis-cipns (append cipns-with-routine-cxns cipns-with-meta-cxns))
         
         ;; 2) find the least general generalisations through anti-unification
         (least-general-generalisations
          (loop for cipn in partial-analysis-cipns
                ;; returns all valid form anti unification results
                for form-anti-unification-results
                  = (anti-unify-form observation-form cipn)
                ;; returns all valid meaning anti unification results
                for meaning-anti-unification-results
                  = (anti-unify-meaning observation-meaning cipn)
                ;; make all combinations and filter for valid combinations
                for all-anti-unification-combinations
                  = (remove-if-not #'valid-au-combination-p
                                   (combinations meaning-anti-unification-results
                                                 form-anti-unification-results))
                when all-anti-unification-combinations
                ;; store all valid combinations with the cxn used for anti unification
                append (loop for combo in all-anti-unification-combinations
                             collect (cons cipn combo))
                into anti-unification-results
                ;; return the best anti unification combination (costs and cxn score)
                finally (return (sort-anti-unification-combinations anti-unification-results)))))
    
    ;; 3) when there are anti-unification results, learn cxns from them!
    (when least-general-generalisations
      (dolist (generalisation least-general-generalisations)
        (let* ((applied-cxn-labels
                (mapcar #'(lambda (cxn) (attr-val cxn :label))
                        (original-applied-constructions (first generalisation))))
               (new-cxns-and-links
                (cond ((every #'(lambda (elem) (eql elem 'fcg::routine)) applied-cxn-labels)
                       (make-item-based-cxn-from-partial-analysis
                        generalisation observation-form observation-meaning cxn-inventory))
                      ((every #'(lambda (elem) (eql elem 'fcg::meta-only)) applied-cxn-labels)
                       (make-holistic-cxns-from-partial-analysis
                        generalisation observation-form observation-meaning cxn-inventory)))))
          (when new-cxns-and-links
            (return new-cxns-and-links)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make item-based cxn from partial analysis ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-top-lvl-units (cipn)
  (let* ((ts-units (fcg-get-transient-unit-structure cipn))
         (root-unit (get-root ts-units)))
    (remove-child-units (remove root-unit ts-units))))

(defun get-open-slot-units (cipn)
  (let* ((ts-units (fcg-get-transient-unit-structure cipn))
         (root-unit (get-root ts-units))
         (all-slot-units (get-child-units (remove root-unit ts-units))))
    (loop for unit in all-slot-units
          unless (and (unit-feature unit 'form) (unit-feature unit 'meaning))
          collect unit)))

(defun cipn-form-slot-args (cipn &key by-category-p)
  (loop for unit in (get-top-lvl-units cipn)
        for category = (extract-category-unit unit)
        if by-category-p
          collect (cons category (unit-feature-value unit 'form-args))
        else
          append (unit-feature-value unit 'form-args)))

(defun cipn-meaning-slot-args (cipn &key by-category-p)
  (loop for unit in (get-top-lvl-units cipn)
        for category = (extract-category-unit unit)
        if by-category-p
            collect (cons category (unit-feature-value unit 'meaning-args))
        else
          append (unit-feature-value unit 'meaning-args)))

(defun cipn-form-top-args (cipn &key by-category-p)
  (loop for unit in (get-open-slot-units cipn)
        for category = (extract-category-unit unit)
        if by-category-p
            collect (cons category (unit-feature-value unit 'form-args))
        else
          append (unit-feature-value unit 'form-args)))

(defun cipn-meaning-top-args (cipn &key by-category-p)
  (loop for unit in (get-open-slot-units cipn)
        for category = (extract-category-unit unit)
        if by-category-p
            collect (cons category (unit-feature-value unit 'meaning-args))
        else
          append (unit-feature-value unit 'meaning-args)))

(defun map-var-from-pattern-to-source (var anti-unification-result)
  "Map a variable from the pattern delta to the same variable in the source delta,
   using the bindings lists."
  (let* ((var-in-generalisation (rest (assoc var (pattern-bindings anti-unification-result)))))
    (first (rassoc var-in-generalisation (source-bindings anti-unification-result)))))

(defun group-cipn-args-by-unit (cipn lists-of-args)
  (loop for args in lists-of-args
        collect (loop for unit in (fcg-get-transient-unit-structure cipn)
                      when (or (equal (first (unit-feature-value unit 'form-args)) args)
                               (equal (first (unit-feature-value unit 'meaning-args)) args))
                      return (cons (extract-category-unit unit) args))))

(defun make-item-based-cxn-from-partial-analysis (anti-unification-results observation-form observation-meaning cxn-inventory)
  (destructuring-bind (anti-unified-cipn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* ((form-top-args
            (loop for arg in (cipn-form-top-args anti-unified-cipn)
                  collect (map-var-from-pattern-to-source (variablify arg) form-anti-unification)))
           (meaning-top-args
            (loop for arg in (cipn-meaning-top-args anti-unified-cipn)
                  collect (map-var-from-pattern-to-source arg meaning-anti-unification)))
           (form-slot-arg-groups
            (loop for (category . args) in (cipn-form-slot-args anti-unified-cipn :by-category-p t)
                  collect (cons category
                                (loop for arg in args
                                      collect (map-var-from-pattern-to-source (variablify arg) form-anti-unification)))))
           (meaning-slot-arg-groups
            (loop for (category . args) in (cipn-meaning-slot-args anti-unified-cipn :by-category-p t)
                  collect (cons category
                                (loop for arg in args
                                      collect (map-var-from-pattern-to-source arg meaning-anti-unification)))))
           ;; learn cxns from source delta
           (source-delta-cxns-and-categories
            (make-generalisation-cxn-with-n-units (source-delta form-anti-unification)
                                                  (source-delta meaning-anti-unification)
                                                  form-top-args
                                                  meaning-top-args
                                                  (mappend #'rest form-slot-arg-groups)
                                                  (mappend #'rest meaning-slot-arg-groups)
                                                  form-slot-arg-groups
                                                  meaning-slot-arg-groups
                                                  cxn-inventory))
           ;; apply new cxns in sandbox to extract categorial links
           (applied-cxns
            (loop for cxn in (applied-constructions anti-unified-cipn)
                  collect (original-cxn (if (routine-cxn-p cxn) cxn
                                          (alter-ego-cxn cxn (construction-inventory anti-unified-cipn))))))
           (sandbox-cxns
            (append (afr-cxns-to-apply source-delta-cxns-and-categories) applied-cxns))
           (sandbox-categories
            (append (mappend #'extract-conditional-categories sandbox-cxns)
                    (mapcar #'extract-contributing-category sandbox-cxns)))
           (sandbox-cipn
            (comprehend-in-sandbox observation-form cxn-inventory
                                   :gold-standard-meaning observation-meaning
                                   :cxns-to-add sandbox-cxns
                                   :categories-to-add sandbox-categories))
           ;; build results
           (cxns-to-apply sandbox-cxns)
           (cxns-to-consolidate (afr-cxns-to-consolidate source-delta-cxns-and-categories))
           (categories-to-add (afr-categories-to-add source-delta-cxns-and-categories))
           (links-to-add (extract-used-categorial-links sandbox-cipn)))
      ;; done!
      (when (and sandbox-cipn (succeeded-cipn-p sandbox-cipn))
        (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make holistic cxns from partial analysis ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-holistic-cxns-from-partial-analysis (anti-unification-results observation-form observation-meaning cxn-inventory)
  (destructuring-bind (anti-unified-cipn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* (;; deal with args...
           (form-top-arg-groups
            (loop for (category . args) in (cipn-form-top-args anti-unified-cipn :by-category-p t)
                  collect (cons category
                                (loop for arg in args
                                      collect (map-var-from-pattern-to-source (variablify arg) form-anti-unification)))))
           (meaning-top-arg-groups
            (loop for (category . args) in (cipn-meaning-top-args anti-unified-cipn :by-category-p t)
                  collect (cons category
                                (loop for arg in args
                                      collect (map-var-from-pattern-to-source arg meaning-anti-unification)))))
           ;; learn cxns from source delta
           (source-delta-cxns-and-categories
            (make-n-holistic-cxns (source-delta form-anti-unification)
                                  (source-delta meaning-anti-unification)
                                  form-top-arg-groups
                                  meaning-top-arg-groups
                                  cxn-inventory))
           ;; apply new cxns in sandbox to extract categorial links
           (applied-cxns
            (loop for cxn in (applied-constructions anti-unified-cipn)
                  collect (original-cxn (if (routine-cxn-p cxn) cxn
                                          (alter-ego-cxn cxn (construction-inventory anti-unified-cipn))))))
           (sandbox-cxns
            (append (mappend #'afr-cxns-to-apply source-delta-cxns-and-categories)
                    applied-cxns))
           (sandbox-categories
            (append (mappend #'extract-conditional-categories sandbox-cxns)
                    (mapcar #'extract-contributing-category sandbox-cxns)))
           (sandbox-cipn
            (comprehend-in-sandbox observation-form cxn-inventory
                                   :gold-standard-meaning observation-meaning
                                   :cxns-to-add sandbox-cxns
                                   :categories-to-add sandbox-categories))
           ;; build results
           (cxns-to-apply sandbox-cxns)
           (cxns-to-consolidate (mappend #'afr-cxns-to-consolidate source-delta-cxns-and-categories))
           (categories-to-add (mappend #'afr-categories-to-add source-delta-cxns-and-categories))
           (links-to-add (extract-used-categorial-links sandbox-cipn)))
      ;; done!
      (when (and sandbox-cipn (succeeded-cipn-p sandbox-cipn))
        (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add)))))