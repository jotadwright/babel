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
                                      original-cxn-inventory
                                      (get-configuration original-cxn-inventory :learning-strategy)))
         (applied-cxns (applied-constructions best-partial-analysis-node))
         (item-based-cxn (first (filter-by-phrase-type 'item-based applied-cxns)))
         (applied-holistic-cxns (filter-by-phrase-type 'holistic applied-cxns)))
    (when item-based-cxn
      (let* ((root-form-constraints (form-predicates-with-variables (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car best-partial-analysis-node)))) 'form))))
        (when (check-meets-continuity root-form-constraints) ;there is one continuous string in root
          (let* ((remaining-meaning (subtract-cxn-meanings-from-gold-standard-meaning applied-cxns gold-standard-meaning))
                 (holistic-cxn-name (make-cxn-name root-form-constraints original-cxn-inventory :add-numeric-tail t :add-cxn-suffix t))
                 (lex-class-holistic-cxn (make-lex-class holistic-cxn-name :trim-cxn-suffix t))
                 (categorial-network (categorial-network original-cxn-inventory))
                 (boundaries-holistic-cxn (get-boundary-units root-form-constraints))
                 (leftmost-unit-holistic-cxn (first boundaries-holistic-cxn))
                 (rightmost-unit-holistic-cxn (second boundaries-holistic-cxn))
                 (args-holistic-cxn (extract-args-from-irl-network remaining-meaning))
                 (existing-holistic-cxn (find-cxn-by-form-and-meaning root-form-constraints remaining-meaning original-cxn-inventory :cxn-type 'holistic))
                 (holistic-cxn (or existing-holistic-cxn
                                (second (multiple-value-list (eval
                                                             `(def-fcg-cxn ,holistic-cxn-name
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
                                                                           :attributes (:cxn-type holistic
                                                                                        :repair item-based->holistic
                                                                                        :meaning ,(fourth (find 'bind remaining-meaning :key #'first))
                                                                                        :string ,(third (find 'string root-form-constraints :key #'first)))
                                                                           :cxn-inventory ,(copy-object original-cxn-inventory)))))))
                 (all-holistic-cxns (sort-cxns-by-form-string (append
                                                               (list holistic-cxn)
                                                               applied-holistic-cxns) utterance))
                 (lex-classes-holistic-cxns (when all-holistic-cxns (mapcar #'lex-class-cxn all-holistic-cxns)))
                 (lex-classes-item-based-units (when item-based-cxn (get-all-unit-lex-classes item-based-cxn)))
                 ;; assign all categorial links
                 (categorial-links (when (and lex-classes-holistic-cxns
                                              lex-classes-item-based-units
                                              (= (length lex-classes-holistic-cxns)
                                                 (length lex-classes-item-based-units)))
                                     (create-new-categorial-links lex-classes-holistic-cxns lex-classes-item-based-units categorial-network)))

                 (cxns-to-apply (append all-holistic-cxns (list item-based-cxn)))
                 (cxns-to-consolidate (unless existing-holistic-cxn (list holistic-cxn))))
            (when categorial-links
              (list
               cxns-to-apply
               categorial-links
               cxns-to-consolidate
               ))))))))
          
                 


