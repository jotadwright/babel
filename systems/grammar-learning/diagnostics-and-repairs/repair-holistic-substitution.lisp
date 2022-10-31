(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add item-based construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass holistic->item-based--substitution (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holistic->item-based--substitution)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (repair-holophrase->item-based+holistic+holistic--substitution problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defun repair-holophrase->item-based+holistic+holistic--substitution (problem node)
  (do-repair-holophrase->item-based+holistic+holistic--substitution
   (form-constraints-with-variables
    (random-elt (get-data problem :utterances))
    (get-configuration (construction-inventory node) :de-render-mode))
   (meaning-predicates-with-variables
    (random-elt (get-data problem :meanings))
    (get-configuration (construction-inventory node) :meaning-representation-formalism))
   nil
   (construction-inventory node)))

(defun do-repair-holophrase->item-based+holistic+holistic--substitution (form-constraints meaning parent-meaning cxn-inventory)
  "Creates item-based construction and holistic constructions
based on existing construction with sufficient overlap."
  (let* ((cxn-inventory (original-cxn-set cxn-inventory))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (top-args (extract-args-from-meaning-networks meaning parent-meaning meaning-representation-formalism))
         ) 

    (multiple-value-bind (non-overlapping-meaning-observation
                          non-overlapping-meaning-cxn
                          non-overlapping-form-observation
                          non-overlapping-form-cxn
                          overlapping-meaning-observation
                          overlapping-meaning-cxn
                          overlapping-form-observation
                          cxn)
        (select-cxn-for-making-item-based-cxn cxn-inventory
                                              form-constraints
                                              meaning
                                              top-args
                                              meaning-representation-formalism
                                              #'check-substitution-conditions)
      
      (when cxn
        (let* (;; cxns and links from iterating over all repairs
               (dummy-top-args-predicate (list (append (list 'top-args) top-args)))
               (arg-bindings (equivalent-meaning-networks overlapping-meaning-observation overlapping-meaning-cxn))
               (renamed-top-args '()) ;; todo, do renamings: take the bindings from (arg-bindings
               (cxns-and-links-holistic-part-observation (handle-potential-holistic-cxn non-overlapping-form-observation 
                                                                                        non-overlapping-meaning-observation
                                                                                        (append overlapping-meaning-observation dummy-top-args-predicate)
                                                                                        cxn-inventory))
               (cxns-and-links-holistic-part-cxn (handle-potential-holistic-cxn non-overlapping-form-cxn non-overlapping-meaning-cxn
                                                                                (append overlapping-meaning-cxn renamed-top-args) cxn-inventory))
               ;; surrounding item-based cxn
               (item-based-cxn-variants (multiple-value-list (create-item-based-cxn cxn-inventory
                                                                                    overlapping-form-observation
                                                                                    non-overlapping-form-observation
                                                                                    overlapping-meaning-observation
                                                                                    non-overlapping-meaning-observation
                                                                                    meaning
                                                                                    top-args
                                                                                    (extract-args-from-meaning-networks non-overlapping-meaning-observation (append overlapping-meaning-observation dummy-top-args-predicate) meaning-representation-formalism) ;; slot args
                                                                                    meaning-representation-formalism
                                                                                    'holistic->item-based--substitution)))
               (new-item-based-cxn-apply-first (first item-based-cxn-variants))
               (new-item-based-cxn-apply-last (second item-based-cxn-variants))
               (lex-class-item-based-cxn (third item-based-cxn-variants))
               (lex-class-item-based-cxn-slot (fourth item-based-cxn-variants))

               ;; build result
               (cxns-to-apply (append (first cxns-and-links-holistic-part-observation) (list new-item-based-cxn-apply-last)))
               (cat-links-to-add (remove nil (append (second cxns-and-links-holistic-part-observation)
                                                     (second cxns-and-links-holistic-part-cxn)
                                                     (list (cons (fifth cxns-and-links-holistic-part-observation)
                                                                 lex-class-item-based-cxn-slot)
                                                           (cons (fifth cxns-and-links-holistic-part-cxn)
                                                                 lex-class-item-based-cxn-slot)))))
               (cxns-to-consolidate (append
                                     (first cxns-and-links-holistic-part-cxn)
                                     (third cxns-and-links-holistic-part-observation)
                                     (list new-item-based-cxn-apply-first)
                                     (third cxns-and-links-holistic-part-cxn)))
               (cats-to-add (remove nil (append (list lex-class-item-based-cxn)
                                                (fourth cxns-and-links-holistic-part-observation)
                                                (fourth cxns-and-links-holistic-part-cxn)))))
                
                                  
          (list
           cxns-to-apply
           cat-links-to-add
           cxns-to-consolidate
           cats-to-add
           lex-class-item-based-cxn
           t
           ))))))


