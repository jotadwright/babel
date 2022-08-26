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
  
(defun create-holistic-cxn-from-partial-analysis (problem node)
  (do-create-holistic-cxn-from-partial-analysis
   (form-constraints-with-variables
    (random-elt (get-data problem :utterances))
    (get-configuration (construction-inventory node) :de-render-mode))
   (meaning-predicates-with-variables
    (random-elt (get-data problem :meanings))
    (get-configuration (construction-inventory node) :meaning-representation-formalism))
   (construction-inventory node)))

(defun do-create-holistic-cxn-from-partial-analysis (form-constraints meaning cxn-inventory)
  
  (let* ((original-cxn-inventory (original-cxn-set cxn-inventory))
         
         (meaning-representation-formalism (get-configuration original-cxn-inventory :meaning-representation-formalism))
         
         (best-partial-analysis-node (get-best-partial-analysis-cipn
                                      form-constraints
                                      meaning
                                      original-cxn-inventory
                                      :optimal-form-coverage-item-based-first))
         (applied-cxns (when best-partial-analysis-node
                         (applied-constructions best-partial-analysis-node))))

    (when (filter-by-phrase-type 'item-based applied-cxns) ;; at least one item-based cxn applied
      (let* ((remaining-form-constraints (form-predicates-with-variables (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car best-partial-analysis-node)))) 'form)))
             (inverted-cxn-meanings (get-inverted-cxn-meanings applied-cxns meaning))
             
             (remaining-meaning (subtract-cxn-meanings-from-gold-standard-meaning inverted-cxn-meanings meaning))
             (args-holistic-cxn (extract-args-from-meaning-networks remaining-meaning (first inverted-cxn-meanings) meaning-representation-formalism)))
             
        (when (and inverted-cxn-meanings
                   (if (equal meaning-representation-formalism :irl)
                        (<= (length args-holistic-cxn) 2))
                   remaining-meaning
                   (connected-semantic-network remaining-meaning)
                   (check-meets-continuity remaining-form-constraints) ;there is one continuous string in root
                   (irl::embedding remaining-meaning meaning)) ;; the subtracted meaning must not be nil
          (let* (;; cxns and links from iterating over all repairs
                 (cxns-and-links-holistic-part-observation (handle-potential-holistic-cxn remaining-form-constraints remaining-meaning (first inverted-cxn-meanings) original-cxn-inventory))
                 
                 (temp-cxns-to-add (append
                                      (first cxns-and-links-holistic-part-observation)
                                      (mapcar #'(lambda (cxn) (alter-ego-cxn (original-cxn cxn) original-cxn-inventory)) applied-cxns)))
                                           
                 (temp-cats-to-add (append (mapcar #'extract-contributing-lex-class temp-cxns-to-add)
                                           (mappend #'get-all-conditional-unit-lex-classes temp-cxns-to-add)))
                 (solution-cipn (comprehend-in-sandbox form-constraints original-cxn-inventory
                                                       :apply-sequentially nil
                                                       :gold-standard-meaning meaning
                                                       :cxns-to-add temp-cxns-to-add
                                                       :categories-to-add temp-cats-to-add))
                   
                 ;; build result
                 (cxns-to-apply (reverse (mapcar #'original-cxn (applied-constructions solution-cipn))))
                 (cat-links-to-add (remove-duplicates (remove nil (append (second cxns-and-links-holistic-part-observation)
                                                                          (extract-used-categorial-links solution-cipn))) :test #'equal))
                 (cxns-to-consolidate (third cxns-and-links-holistic-part-observation))
                 (cats-to-add (fourth cxns-and-links-holistic-part-observation)))
        
              (list
               cxns-to-apply
               cat-links-to-add
               cxns-to-consolidate
               cats-to-add
               (extract-contributing-lex-class (last-elt cxns-to-apply))
               t
               )))))))
            
          
                 

