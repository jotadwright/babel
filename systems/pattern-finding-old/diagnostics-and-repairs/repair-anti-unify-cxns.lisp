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
  (when (constructions cxn-inventory)
    (let ((new-cxns-and-links (find-cxns-and-anti-unify observation-form observation-meaning (original-cxn-set cxn-inventory))))
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

(defun find-cxns-and-anti-unify (observation-form observation-meaning cxn-inventory)
  "Given form and meaning of an observation and a cxn inventory,
   find the cxn that leads to the smallest generalisation
   and learn new cxn(s) from this generalisation."
  (let* (;; 1) select cxns by hasing the observation
         (hash-compatible-cxns
          (constructions-for-anti-unification-hashed observation-form observation-meaning cxn-inventory))
         
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
                for best-form-anti-unification-result
                  = (anti-unify-form observation-form cxn)
                for best-meaning-anti-unification-result
                  = (anti-unify-meaning observation-meaning cxn)
                when (and best-form-anti-unification-result
                          best-meaning-anti-unification-result
                          (<= (cost best-form-anti-unification-result) max-au-cost)
                          (<= (cost best-meaning-anti-unification-result) max-au-cost))
                collect (list cxn best-form-anti-unification-result best-meaning-anti-unification-result)
                into anti-unification-results
                finally (return (first (sort-anti-unification-results anti-unification-results))))))
    ;; 4) learn cxn(s) from the anti-unification results
    ;;    - no anti-unification results -> fail -> learn holistic
    ;;    - yes anti-unification results -> learn cxns for generalisation and delta's
    (when least-general-generalisation
      (make-cxns-from-generalisations least-general-generalisation cxn-inventory))))

;;;;;;;;;;;;;;;;;;;;;;
;; anti-unify utils ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-form (source-form cxn)
  "Anti-unify the observation with the given cxn on the form side."
  (let* ((pattern-form
          (fresh-variables (extract-form-predicates cxn)))
         (anti-unification-results
          (anti-unify-predicate-network pattern-form source-form))
         (results-with-non-empty-parts
          (remove-if #'(lambda (result)
                         (or (null (pattern-delta result))
                             (null (source-delta result))
                             (null (generalisation result))))
                     anti-unification-results)))
    (first (sort results-with-non-empty-parts #'< :key #'fcg::cost))))

(defun anti-unify-meaning (source-meaning cxn)
  "Anti-unify the observation with the given cxn on the meaning side."
  (let* ((pattern-meaning
          (fresh-variables (fcg::extract-meaning-predicates cxn)))
         (anti-unification-results
          (anti-unify-predicate-network pattern-meaning source-meaning))
         (results-with-non-empty-parts
          (remove-if #'(lambda (result)
                         (or (null (pattern-delta result))
                             (null (source-delta result))
                             (null (generalisation result))))
                     anti-unification-results)))
    (first (sort results-with-non-empty-parts #'< :key #'fcg::cost))))

(defun sort-anti-unification-results (list-of-anti-unification-results)
  "Sort the anti-unifcation results based on cost (of both form- and
   meaning-anti-unification) and avg cxn score as a tie breaker."
  (sort list-of-anti-unification-results
        #'(lambda (au-1 au-2)
            (let ((au-cost-1 (+ (cost (second au-1)) (cost (third au-1))))
                  (au-cost-2 (+ (cost (second au-2)) (cost (third au-2))))
                  (cxn-score-1 (get-cxn-score (first au-1)))
                  (cxn-score-2 (get-cxn-score (first au-2))))
              (if (= au-cost-1 au-cost-2)
                (> cxn-score-1 cxn-score-2)
                (< au-cost-1 au-cost-2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make cxns from generalisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-cxns-from-generalisations (anti-unification-results cxn-inventory)
  (destructuring-bind (anti-unified-cxn
                       form-anti-unification
                       meaning-anti-unification) anti-unification-results
    (declare (ignore anti-unified-cxn))
    (let* (;; all form-args and meaning-args
           (form-args (compute-args form-anti-unification))
           (meaning-args (compute-args meaning-anti-unification))
           ;; dispatch to helper functions to make generalisation-cxn and delta cxns
           (generalisation-cxns-and-categories
            (make-generalisation-cxn (generalisation form-anti-unification)
                                     (generalisation meaning-anti-unification)
                                     (get-data form-args :top-lvl-args)
                                     (get-data meaning-args :top-lvl-args)
                                     (get-data form-args :generalisation-slot-args)
                                     (get-data meaning-args :generalisation-slot-args)
                                     cxn-inventory))
           (pattern-delta-cxns-and-categories
            (make-holistic-cxn (pattern-delta form-anti-unification)
                               (pattern-delta meaning-anti-unification)
                               (get-data form-args :pattern-args)
                               (get-data meaning-args :pattern-args)
                               cxn-inventory))
           (source-delta-cxns-and-categories
            (make-holistic-cxn (source-delta form-anti-unification)
                               (source-delta meaning-anti-unification)
                               (get-data form-args :source-args)
                               (get-data meaning-args :source-args)
                               cxn-inventory))
           ;; build result
           (cxns-to-apply
            (append (first source-delta-cxns-and-categories)
                    (list (first generalisation-cxns-and-categories))))
           (cxns-to-consolidate
            (append (list (second generalisation-cxns-and-categories))
                    (second source-delta-cxns-and-categories)
                    (first pattern-delta-cxns-and-categories)
                    (second pattern-delta-cxns-and-categories)))
           (categories-to-add
            (append (list (third generalisation-cxns-and-categories))
                    (list (fourth generalisation-cxns-and-categories))
                    (third pattern-delta-cxns-and-categories)
                    (third source-delta-cxns-and-categories)))
           (links-to-add
            (append (fourth pattern-delta-cxns-and-categories)
                    (fourth source-delta-cxns-and-categories)
                    (list (cons (fourth generalisation-cxns-and-categories)
                                (first (third pattern-delta-cxns-and-categories)))
                          (cons (fourth generalisation-cxns-and-categories)
                                (first (third source-delta-cxns-and-categories)))))))
      (list cxns-to-apply cxns-to-consolidate categories-to-add links-to-add))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make generalisation cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-generalisation-cxn (form meaning top-lvl-form-args top-lvl-meaning-args slot-form-args slot-meaning-args cxn-inventory)
  (let* (;; cxn names
         (bare-cxn-name
          (make-cxn-name form cxn-inventory :item-based-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" bare-cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" bare-cxn-name))))
         ;; cxn inventory
         (cxn-inventory-copy (copy-object cxn-inventory))
         ;; lex classes
         (lex-class-item-based
          (make-lex-class (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t))
         (lex-class-slot
          (make-lex-class (symbol-name bare-cxn-name) :trim-cxn-suffix t :numeric-suffix t :slotp t))
         ;; build cxns!
         (item-based-cxn-apply-last
          (second
           (multiple-value-list
            (eval
             (item-based-cxn-apply-last-skeleton bare-cxn-name cxn-name-apply-last
                                                 lex-class-item-based lex-class-slot
                                                 form meaning
                                                 top-lvl-form-args top-lvl-meaning-args
                                                 slot-form-args slot-meaning-args
                                                 (get-configuration cxn-inventory :initial-cxn-score)
                                                 cxn-inventory-copy)))))
         (item-based-cxn-apply-first
          (second
           (multiple-value-list
            (eval
             (item-based-cxn-apply-first-skeleton bare-cxn-name cxn-name-apply-first
                                                  lex-class-item-based lex-class-slot
                                                  form meaning
                                                  top-lvl-form-args top-lvl-meaning-args
                                                  slot-form-args slot-meaning-args
                                                  (get-configuration cxn-inventory :initial-cxn-score)
                                                  cxn-inventory-copy))))))
    (list item-based-cxn-apply-last
          item-based-cxn-apply-first
          lex-class-item-based
          lex-class-slot)))

;;;;;;;;;;;;;;;;;;;;;;;
;; make holistic cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;
        
(defun make-holistic-cxn (form meaning form-args meaning-args cxn-inventory)
  (let* (;; make the cxn names
         (cxn-name
          (make-cxn-name form cxn-inventory :holistic-suffix t :numeric-suffix t))
         (cxn-name-apply-last
          (intern (upcase (format nil "~a-apply-last" cxn-name))))
         (cxn-name-apply-first
          (intern (upcase (format nil "~a-apply-first" cxn-name))))
         ;; unit name
         (unit-name-holistic-cxn
          (make-unit-name (mkstr cxn-name) cxn-inventory :trim-cxn-suffix t))
         ;; lex class
         (lex-class-holistic-cxn
          (make-lex-class cxn-name :trim-cxn-suffix t :numeric-suffix t))
         ;; temp cxn inventory
         (cxn-inventory-copy
          (copy-object cxn-inventory))
         ;; apply first cxn
         (holistic-cxn-apply-first
          (holistic-cxn-apply-first-skeleton cxn-name cxn-name-apply-first
                                             unit-name-holistic-cxn lex-class-holistic-cxn
                                             form meaning form-args meaning-args
                                             (get-configuration cxn-inventory :initial-cxn-score)
                                             nil cxn-inventory-copy))
         ;; apply last cxn
         (holistic-cxn-apply-last
          (holistic-cxn-apply-last-skeleton cxn-name cxn-name-apply-last
                                            unit-name-holistic-cxn lex-class-holistic-cxn
                                            form meaning form-args meaning-args
                                            (get-configuration cxn-inventory :initial-cxn-score)
                                            nil cxn-inventory-copy))
         ;; build the result
         (cxns-to-apply (list holistic-cxn-apply-first))
         (cxns-to-consolidate (list holistic-cxn-apply-last))
         (cats-to-add (list lex-class-holistic-cxn))
         (links-to-add nil))
    ;; done!
    (list cxns-to-apply cxns-to-consolidate cats-to-add links-to-add)))