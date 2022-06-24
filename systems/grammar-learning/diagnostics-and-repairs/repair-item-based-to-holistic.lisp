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
                         (applied-constructions best-partial-analysis-node)))
         (item-based-cxn (first (filter-by-phrase-type 'item-based applied-cxns)))
         (applied-holistic-cxns (filter-by-phrase-type 'holistic applied-cxns)))
    (when item-based-cxn
      (let* ((root-form-constraints (form-predicates-with-variables (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car best-partial-analysis-node)))) 'form)))
             (inverted-cxn-meanings (get-inverted-cxn-meanings applied-cxns meaning))
             (remaining-meaning (subtract-cxn-meanings-from-gold-standard-meaning inverted-cxn-meanings meaning))
             (args-holistic-cxn (extract-args-from-meaning-networks remaining-meaning (first inverted-cxn-meanings) meaning-representation-formalism))) ;take args from item-based; filling in the bindings
        (when (and remaining-meaning
                   (<= (length args-holistic-cxn) 2)
                   (check-meets-continuity root-form-constraints) ;there is one continuous string in root
                   (cxn-meaning-is-valid-gold-standard-subset-p inverted-cxn-meanings)) ;; the subtracted meaning must not be nil
          (let* (;; cxns and links from iterating over all repairs
               (cxns-and-links-holistic-part-observation (handle-potential-holistic-cxn root-form-constraints remaining-meaning original-cxn-inventory))
               (dummy-holistic-cxns-and-links (do-create-holistic-cxn root-form-constraints remaining-meaning (processing-cxn-inventory original-cxn-inventory)))
               
               (all-holistic-cxns (sort-cxns-by-meets-constraints (append
                                                             (third dummy-holistic-cxns-and-links)
                                                             applied-holistic-cxns) form-constraints))
               (lex-classes-holistic-cxns (loop for lc in (mapcar #'lex-class-apply-last-cxn all-holistic-cxns)
                                                collect (if (equal lc (first (fourth dummy-holistic-cxns-and-links)))
                                                          (first (fourth cxns-and-links-holistic-part-observation))
                                                          lc)))
                                                  ;;substitute the dummy category with the recursive category!
               (lex-classes-item-based-units (when item-based-cxn (get-all-unit-lex-classes item-based-cxn)))
               

               ;; create all categorial links
               (categorial-links (when (and lex-classes-holistic-cxns
                                            lex-classes-item-based-units
                                            (= (length lex-classes-holistic-cxns)
                                               (length lex-classes-item-based-units)))
                                   (create-new-categorial-links lex-classes-holistic-cxns lex-classes-item-based-units (categorial-network original-cxn-inventory))))

               ;; build result
               (cxns-to-apply (append (first cxns-and-links-holistic-part-observation)
                                      (mapcar #'(lambda (cxn) (alter-ego-cxn cxn original-cxn-inventory)) applied-holistic-cxns)
                                      (list (alter-ego-cxn item-based-cxn original-cxn-inventory))))
               (cat-links-to-add (remove nil (append (second cxns-and-links-holistic-part-observation)
                                                     categorial-links)))
               (cxns-to-consolidate (third cxns-and-links-holistic-part-observation))
                                     
               (cats-to-add (remove nil (append (list (extract-contributing-lex-class item-based-cxn))
                                                (fourth cxns-and-links-holistic-part-observation)))))
        
          (list
           cxns-to-apply
           cat-links-to-add
           cxns-to-consolidate
           cats-to-add
           ))
          )))))
          
                 
#|(all-holistic-cxns (sort-cxns-by-form-string (append
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
  (alter-item-based-cxn (alter-ego-cxn item-based-cxn original-cxn-inventory))
  (alter-applied-holistic-cxns (mapcar #'(lambda (cxn) (alter-ego-cxn cxn original-cxn-inventory)) applied-holistic-cxns))
  (all-alter-holistic-cxns (sort-cxns-by-form-string (append
                                                      (list new-holistic-cxn-apply-first)
                                                      alter-applied-holistic-cxns) utterance original-cxn-inventory))
 ;(cxns-to-apply (append (list item-based-cxn) all-holistic-cxns))
  (cxns-to-apply (append all-alter-holistic-cxns (list alter-item-based-cxn)))
 ;(cxns-to-consolidate (unless existing-holistic-cxn-apply-first (list new-holistic-cxn-apply-first))))
  (cxns-to-consolidate (unless existing-holistic-cxn-apply-last (list new-holistic-cxn-apply-last))))
 ;(add-element (make-html new-holistic-cxn-apply-last))
|#

