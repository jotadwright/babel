(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Holophrase Deletion  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass holistic->item-based--deletion (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node))) ;; it's always fcg::new-node, we created a new node in the search process

(defmethod repair ((repair holistic->item-based--deletion)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction, holophrase and holistic cxn."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (repair-holophrase->item-based+holistic+holophrase--deletion problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))


(defun repair-holophrase->item-based+holistic+holophrase--deletion (problem node)
  (do-repair-holophrase->item-based+holistic+holophrase--deletion
      (form-constraints-with-variables
    (random-elt (get-data problem :utterances))
    (get-configuration (construction-inventory node) :de-render-mode))
   (meaning-predicates-with-variables
    (random-elt (get-data problem :meanings))
    (get-configuration (construction-inventory node) :meaning-representation-formalism))
   nil
   (construction-inventory node)))

(defun do-repair-holophrase->item-based+holistic+holophrase--deletion (form-constraints meaning parent-meaning cxn-inventory) 
  (let* ((cxn-inventory (original-cxn-set cxn-inventory))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism)))
    (multiple-value-bind (cxn
                          non-overlapping-form
                          non-overlapping-meaning
                          overlapping-form
                          overlapping-meaning)
        (find-superset-holistic-cxn cxn-inventory form-constraints meaning meaning-representation-formalism)

      (when cxn
        
        (let* (;; cxns and links from iterating over all repairs
               (cxns-and-links-holistic-part-cxn (handle-potential-holistic-cxn non-overlapping-form non-overlapping-meaning overlapping-meaning cxn-inventory))
               (cxns-and-links-holistic-part-observation (do-create-holistic-cxn form-constraints meaning parent-meaning (processing-cxn-inventory cxn-inventory)))
               
               
               ;; surrounding item-based cxn
               (item-based-cxn-variants (multiple-value-list (create-item-based-cxn cxn-inventory
                                                                                    overlapping-form
                                                                                    non-overlapping-form
                                                                                    overlapping-meaning
                                                                                    non-overlapping-meaning
                                                                                    (extract-meaning-predicates cxn)
                                                                                    parent-meaning;; todo get the parent meaning with substituted variables from bindings! (get-subtracted-meaning-from-cxn cxn gold-standard-meaning) ; we don't have the original gold standard here 
                                                                                    meaning-representation-formalism
                                                                                    'holistic->item-based--addition)))
               (new-item-based-cxn-apply-first (first item-based-cxn-variants))
               (new-item-based-cxn-apply-last (second item-based-cxn-variants))
               (lex-class-item-based-cxn (third item-based-cxn-variants))
               (lex-class-item-based-cxn-slot (fourth item-based-cxn-variants))

               ;; build result
               (cxns-to-apply (first cxns-and-links-holistic-part-observation))
               (cat-links-to-add (remove nil (append (second cxns-and-links-holistic-part-cxn)
                                                     (list (cons (fifth cxns-and-links-holistic-part-cxn)
                                                                 lex-class-item-based-cxn-slot)))))
               (cxns-to-consolidate (append
                                     (first cxns-and-links-holistic-part-cxn)
                                     (third cxns-and-links-holistic-part-cxn)
                                     (third cxns-and-links-holistic-part-observation)
                                     (list new-item-based-cxn-apply-last new-item-based-cxn-apply-first)))
                                     
               (cats-to-add (remove nil (append (fourth cxns-and-links-holistic-part-observation)
                                                (list lex-class-item-based-cxn)
                                                (fourth cxns-and-links-holistic-part-cxn)
                                                ))))
        
          (list
           cxns-to-apply
           cat-links-to-add
           cxns-to-consolidate
           cats-to-add
           lex-class-item-based-cxn
           t
           ))))))