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
    (let* ((mode (get-configuration cxn-inventory :partial-analysis-mode))
           (new-cxns-and-links (find-cipn-and-anti-unify observation-form observation-meaning args (original-cxn-set cxn-inventory) mode)))
      (when new-cxns-and-links
        (destructuring-bind (cxns-to-apply
                             cxns-to-consolidate
                             cats-to-add
                             cat-links-to-add
                             top-lvl-category
                             anti-unified-cxns
                             partial-analysis-cxns) new-cxns-and-links
          (apply-fix :form-constraints observation-form
                     :cxns-to-apply cxns-to-apply
                     :cxns-to-consolidate cxns-to-consolidate
                     :categories-to-add cats-to-add
                     :categorial-links cat-links-to-add
                     :top-level-category top-lvl-category
                     :node node
                     :repair-name repair-type
                     :anti-unified-cxns anti-unified-cxns
                     :partial-analysis-cxns partial-analysis-cxns))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find cipn and anti-unify ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event learn-from-partial-analysis (anti-unification-results list))

(defgeneric find-cipn-and-anti-unify (observation-form observation-meaning args cxn-inventory mode)
  (:documentation "Given form and meaning of an observation and a cxn inventory,
   find the best transient structure that partially covers the observation
   and learn new cxn(s) from the remainder"))

(defmethod find-cipn-and-anti-unify (observation-form observation-meaning (args blackboard) (cxn-inventory fcg-construction-set) (mode (eql :heuristic)))
  (let* (;; 1) run comprehension (routine and meta set) and obtain partial analyses
         (cipns-with-routine-cxns (compatible-cipns-with-routine-cxns observation-form observation-meaning cxn-inventory))
         (cipns-with-meta-cxns (compatible-cipns-with-meta-cxns observation-form observation-meaning cxn-inventory))
         
         ;; 2) sort all cipns by depth, created-at, and avg cxn score
         (sorted-cipns (sort-by-depth-created-at-and-avg-score (append cipns-with-routine-cxns cipns-with-meta-cxns))))

    ;; 3) anti-unify and learn cxns
    (loop with no-string-cxns = (get-configuration cxn-inventory :allow-cxns-with-no-strings)
          for cipn in sorted-cipns
          ;; returns all valid form anti unification results
          for form-anti-unification-results
            = (anti-unify-form observation-form cipn :no-string-cxns no-string-cxns)
          for meaning-anti-unification-results
            = (anti-unify-meaning observation-meaning cipn)
          ;; make all combinations and filter for valid combinations
          for all-anti-unification-combinations
            = (remove-if-not #'valid-au-combination-p
                             (combinations meaning-anti-unification-results
                                           form-anti-unification-results))
          for combinations-with-cipn
            = (loop for combo in all-anti-unification-combinations
                    collect (cons cipn combo))
          for applied-cxn-labels
            = (mapcar #'(lambda (cxn) (attr-val cxn :label))
                      (original-applied-constructions cipn))
           for new-cxns-and-links
             = (when combinations-with-cipn
                 (loop for generalisation in (sort-anti-unification-combinations combinations-with-cipn)
                       for new-cxns-and-links
                         = (cond ((every #'(lambda (elem) (eql elem 'fcg::routine)) applied-cxn-labels)
                                  (make-item-based-cxn-from-partial-analysis
                                   generalisation observation-form observation-meaning args cxn-inventory))
                                 ((every #'(lambda (elem) (eql elem 'fcg::meta-only)) applied-cxn-labels)
                                  (make-holistic-cxns-from-partial-analysis
                                   generalisation observation-form observation-meaning args cxn-inventory)))
                       when new-cxns-and-links
                         return new-cxns-and-links))
          when new-cxns-and-links
            return new-cxns-and-links)))
  

(defmethod find-cipn-and-anti-unify (observation-form observation-meaning (args blackboard) (cxn-inventory fcg-construction-set) (mode (eql :exhaustive)))
  (let* (;; 1) run comprehension (routine and meta set)
         ;;    and obtain partial analyses
         (cipns-with-routine-cxns (compatible-cipns-with-routine-cxns observation-form observation-meaning cxn-inventory))
         (cipns-with-meta-cxns (compatible-cipns-with-meta-cxns observation-form observation-meaning cxn-inventory))
         (partial-analysis-cipns (append cipns-with-routine-cxns cipns-with-meta-cxns))
         
         ;; 2) find the least general generalisations through anti-unification
         (least-general-generalisations
          (loop with no-string-cxns = (get-configuration cxn-inventory :allow-cxns-with-no-strings)
                for cipn in partial-analysis-cipns
                ;; returns all valid form anti unification results
                for form-anti-unification-results
                  = (anti-unify-form observation-form cipn :no-string-cxns no-string-cxns)
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
                        generalisation observation-form observation-meaning args cxn-inventory))
                      ((every #'(lambda (elem) (eql elem 'fcg::meta-only)) applied-cxn-labels)
                       (make-holistic-cxns-from-partial-analysis
                        generalisation observation-form observation-meaning args cxn-inventory)))))
          (when new-cxns-and-links
            (notify learn-from-partial-analysis generalisation)
            (return new-cxns-and-links)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make item-based cxn from partial analysis ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-item-based-cxn-from-partial-analysis (anti-unification-results observation-form observation-meaning args cxn-inventory)
  (declare (ignore args)) ;; partial analysis is not applied in the recursion
  (destructuring-bind (anti-unified-cipn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (let* ((form-top-args
            (or
             (loop for arg in (cipn-form-top-args anti-unified-cipn)
                   collect (map-var-from-pattern-to-source (variablify arg) form-anti-unification))
             (holistic-form-top-args observation-form (get-configuration cxn-inventory :form-representation-formalism))))
           (meaning-top-args
            (or
             (loop for arg in (cipn-meaning-top-args anti-unified-cipn)
                   collect (map-var-from-pattern-to-source arg meaning-anti-unification))
             (holistic-meaning-top-args observation-meaning (get-configuration cxn-inventory :meaning-representation-formalism))))
           (form-slot-args
            (sort
             (loop for (category . args) in (cipn-form-slot-args anti-unified-cipn :by-category-p t)
                   collect (cons category
                                 (loop for arg in args
                                       collect (map-var-from-pattern-to-source (variablify arg) form-anti-unification))))
             #'string< :key #'car))
           (meaning-slot-args
            (sort
             (loop for (category . args) in (cipn-meaning-slot-args anti-unified-cipn :by-category-p t)
                   collect (cons category
                                 (loop for arg in args
                                       collect (map-var-from-pattern-to-source arg meaning-anti-unification))))
             #'string< :key #'car))
           ;; learn cxns from source delta
           (source-delta-cxns-and-categories
            (let ((recursion-args
                   (make-blackboard
                    :data-fields
                    (list (cons :top-lvl-form-args form-top-args)
                          (cons :top-lvl-meaning-args meaning-top-args)
                          (cons :slot-form-args (mapcar #'rest form-slot-args))
                          (cons :slot-meaning-args (mapcar #'rest meaning-slot-args))))))
              (handle-potential-holistic-cxn (source-delta form-anti-unification)
                                             (source-delta meaning-anti-unification)
                                             recursion-args cxn-inventory))
            ;(make-item-based-cxn (source-delta form-anti-unification)
            ;                     (source-delta meaning-anti-unification)
            ;                     form-top-args
            ;                     meaning-top-args
            ;                     (mapcar #'rest form-slot-args)
            ;                     (mapcar #'rest meaning-slot-args)
            ;                     cxn-inventory)
            )
           ;; apply new cxns in sandbox to extract categorial links
           (applied-cxns
            (loop for cxn in (applied-constructions anti-unified-cipn)
                  collect (original-cxn (if (routine-cxn-p cxn) cxn
                                          (alter-ego-cxn cxn (construction-inventory anti-unified-cipn))))))
           (sandbox-cxns
            (append applied-cxns (afr-cxns-to-apply source-delta-cxns-and-categories)))
           (sandbox-categories
            (append (mappend #'extract-conditional-categories sandbox-cxns)
                    (mapcar #'extract-contributing-category sandbox-cxns)))
           (sandbox-cipn
            (comprehend-in-sandbox observation-form cxn-inventory
                                   :gold-standard-meaning observation-meaning
                                   :cxns-to-add sandbox-cxns
                                   :categories-to-add sandbox-categories)))
      ;; done!
      (when (and sandbox-cipn (succeeded-cipn-p sandbox-cipn))
        (list sandbox-cxns  ; cxns to apply
              (afr-cxns-to-consolidate source-delta-cxns-and-categories)  ; cxns to consolidate
              (afr-categories-to-add source-delta-cxns-and-categories)  ; categories to add
              (extract-used-categorial-links sandbox-cipn)  ; categorial links
              (afr-top-lvl-category source-delta-cxns-and-categories)  ; top lvl category
              (afr-anti-unified-cxns source-delta-cxns-and-categories)  ; anti unified cxns
              applied-cxns  ; partial analysis cxns
              )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make holistic cxns from partial analysis ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-holistic-cxns-from-partial-analysis (anti-unification-results observation-form observation-meaning args cxn-inventory)
  (declare (ignore args)) ;; partial analysis is not applied in the recursion
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
            (append (mappend #'afr-cxns-to-apply source-delta-cxns-and-categories) applied-cxns))
           (sandbox-categories
            (append (mappend #'extract-conditional-categories sandbox-cxns)
                    (mapcar #'extract-contributing-category sandbox-cxns)))
           (sandbox-cipn
            (comprehend-in-sandbox observation-form cxn-inventory
                                   :gold-standard-meaning observation-meaning
                                   :cxns-to-add sandbox-cxns
                                   :categories-to-add sandbox-categories))
           ;; build results
           (links-to-add
            (append (mappend #'afr-categorial-links source-delta-cxns-and-categories)
                    (extract-used-categorial-links sandbox-cipn))))
      ;; done!
      (when (and sandbox-cipn (succeeded-cipn-p sandbox-cipn))
        (list sandbox-cxns  ; cxns to apply
              (mappend #'afr-cxns-to-consolidate source-delta-cxns-and-categories)  ; cxns to consolidate
              (mappend #'afr-categories-to-add source-delta-cxns-and-categories)  ; categories to add
              links-to-add  ; links to add
              (extract-top-category-cxn (last-elt applied-cxns))  ; top lvl category
              (mappend #'afr-anti-unified-cxns source-delta-cxns-and-categories)  ; anti unified cxns
              applied-cxns  ; partial analysis cxns
              )))))