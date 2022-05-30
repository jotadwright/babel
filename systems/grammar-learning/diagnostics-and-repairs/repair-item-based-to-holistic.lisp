(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair item-based->holistic     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass item-based->holistic (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair item-based->holistic)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holistic construction."
  (when (initial-node-p node)
    (let ((holistic-cxn-and-categorial-link (create-holistic-cxn-from-partial-analysis problem node)))
      (when holistic-cxn-and-categorial-link
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data holistic-cxn-and-categorial-link)))))
  
(defmethod repair ((repair item-based->holistic)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holistic construction."
  (when (initial-node-p node)
    (let ((holistic-cxn-and-categorial-link (create-holistic-cxn-from-partial-analysis problem node)))
      (when holistic-cxn-and-categorial-link 
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data holistic-cxn-and-categorial-link)))))

  

(defun create-holistic-cxn-from-partial-analysis (problem node)
  (let* ((original-cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning-representation-formalism (get-configuration original-cxn-inventory :meaning-representation-formalism))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings)) meaning-representation-formalism))
         (best-partial-analysis-node (get-best-partial-analysis-cipn
                                      utterance
                                      gold-standard-meaning
                                      original-cxn-inventory
                                      :optimal-form-coverage-item-based-first))
         (applied-cxns (when best-partial-analysis-node
                         (applied-constructions best-partial-analysis-node)))
         (item-based-cxn (first (filter-by-phrase-type 'item-based applied-cxns)))
         (applied-holistic-cxns (filter-by-phrase-type 'holistic applied-cxns)))
    (when item-based-cxn
      (let* ((root-form-constraints (form-predicates-with-variables (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car best-partial-analysis-node)))) 'form)))
             (inverted-cxn-meanings (get-inverted-cxn-meanings applied-cxns gold-standard-meaning))
             (remaining-meaning (subtract-cxn-meanings-from-gold-standard-meaning inverted-cxn-meanings gold-standard-meaning)))
        (when (and (check-meets-continuity root-form-constraints) ;there is one continuous string in root
                   (cxn-meaning-is-valid-gold-standard-subset-p inverted-cxn-meanings)) ;; the subtracted meaning must not be nil
          (let* ((holistic-cxn-name (make-cxn-name root-form-constraints original-cxn-inventory :add-numeric-tail t :add-cxn-suffix t))
                 (cxn-name-holistic-cxn-apply-last (concatenate 'string (symbol-name holistic-cxn-name) "-APPLY-LAST"))
                 (cxn-name-holistic-cxn-apply-first (concatenate 'string (symbol-name holistic-cxn-name) "-APPLY-FIRST"))
                 (lex-class-holistic-cxn (make-lex-class holistic-cxn-name :trim-cxn-suffix t))
                 (categorial-network (categorial-network original-cxn-inventory))
                 (boundaries-holistic-cxn (get-boundary-units root-form-constraints))
                 (leftmost-unit-holistic-cxn (first boundaries-holistic-cxn))
                 (rightmost-unit-holistic-cxn (second boundaries-holistic-cxn))
                 (args-holistic-cxn (extract-args-from-meaning-networks remaining-meaning (first inverted-cxn-meanings) meaning-representation-formalism)) ;take args from item-based; filling in the bindings
                 (existing-holistic-cxn-apply-first
                (find-cxn-by-form-and-meaning root-form-constraints remaining-meaning original-cxn-inventory :cxn-set 'fcg::routine :cxn-type 'holistic))
                 (existing-holistic-cxn-apply-last
                (find-cxn-by-form-and-meaning root-form-constraints remaining-meaning original-cxn-inventory :cxn-set 'fcg::meta-only :cxn-type 'holistic))
                 (new-holistic-cxn-apply-first
                  (or existing-holistic-cxn-apply-first
                      (second (multiple-value-list (eval
                                                    `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-first
                                                                  ((,leftmost-unit-holistic-cxn
                                                                    (args ,args-holistic-cxn)
                                                                    (syn-cat (phrase-type holistic)
                                                                             (lex-class ,lex-class-holistic-cxn))
                                                                    (boundaries
                                                                     (left ,leftmost-unit-holistic-cxn)
                                                                     (right ,rightmost-unit-holistic-cxn)))
                                                                   <-
                                                                   (,leftmost-unit-holistic-cxn
                                                                    (HASH meaning ,remaining-meaning)
                                                                    --
                                                                    (HASH form ,root-form-constraints)))
                                                                  :attributes (:label fcg::routine
                                                                               :cxn-type holistic
                                                                               :repair item-based->holistic
                                                                               :meaning ,(fourth (find 'bind remaining-meaning :key #'first))
                                                                               :string ,(third (find 'string root-form-constraints :key #'first)))
                                                                  :cxn-inventory ,(copy-object original-cxn-inventory)))))))
                 (new-holistic-cxn-apply-last
                  (or existing-holistic-cxn-apply-last
                      (second (multiple-value-list (eval
                                                    `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-last
                                                                  (
                                                                   <-
                                                                   (?holistic-unit
                                                                    (HASH meaning ,remaining-meaning)
                                                                    (args ,args-holistic-cxn)
                                                                    (syn-cat (phrase-type holistic)
                                                                             (lex-class ,lex-class-holistic-cxn))
                                                                    (boundaries
                                                                     (left ,leftmost-unit-holistic-cxn)
                                                                     (right ,rightmost-unit-holistic-cxn))
                                                                    --
                                                                    (HASH form ,root-form-constraints)
                                                                    (args ,args-holistic-cxn)
                                                                    (syn-cat (phrase-type holistic)
                                                                             (lex-class ,lex-class-holistic-cxn))
                                                                    (boundaries
                                                                     (left ,leftmost-unit-holistic-cxn)
                                                                     (right ,rightmost-unit-holistic-cxn))))
                                                                  :attributes (:label fcg::meta-only
                                                                               :cxn-type holistic
                                                                               :repair item-based->holistic
                                                                               :meaning ,(fourth (find 'bind remaining-meaning :key #'first))
                                                                               :string ,(third (find 'string root-form-constraints :key #'first)))
                                                                  :cxn-inventory ,(copy-object original-cxn-inventory)))))))
                 
                 (all-holistic-cxns (sort-cxns-by-form-string (append
                                                               (list new-holistic-cxn-apply-last)
                                                               applied-holistic-cxns) utterance original-cxn-inventory))
                 (lex-classes-holistic-cxns (when all-holistic-cxns (mapcar #'lex-class-apply-last-cxn all-holistic-cxns)))
                 (lex-classes-item-based-units (when item-based-cxn (get-all-unit-lex-classes item-based-cxn)))
                 ;; assign all categorial links
                 (categorial-links (when (and lex-classes-holistic-cxns
                                              lex-classes-item-based-units
                                              (= (length lex-classes-holistic-cxns)
                                                 (length lex-classes-item-based-units)))
                                     (create-new-categorial-links lex-classes-holistic-cxns lex-classes-item-based-units categorial-network)))

                 (cxns-to-apply (append (list item-based-cxn) all-holistic-cxns))
                 (cxns-to-consolidate (unless existing-holistic-cxn-apply-first (list new-holistic-cxn-apply-first))))
            (add-element (make-html new-holistic-cxn-apply-last))
            (when categorial-links
              (list
               cxns-to-apply
               categorial-links
               cxns-to-consolidate
               ))))))))
          
                 


