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
   (construction-inventory node)
   node))

(defun do-repair-holophrase->item-based+holistic+holophrase--deletion (form-constraints meaning parent-meaning cxn-inventory node) 
  (let* ((cxn-inventory (original-cxn-set cxn-inventory))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (top-args (extract-args-from-meaning-networks meaning parent-meaning meaning-representation-formalism)))
    (multiple-value-bind (non-overlapping-meaning-observation
                          non-overlapping-meaning-cxn ; holistic part
                          non-overlapping-form-observation
                          non-overlapping-form-cxn ; holistic part
                          overlapping-meaning-observation
                          overlapping-meaning-cxn ; item-based part
                          overlapping-form-observation
                          overlapping-form-cxn ; item-based part
                          cxn)
        (select-cxn-for-making-item-based-cxn cxn-inventory
                                              form-constraints
                                              meaning
                                              top-args
                                              meaning-representation-formalism
                                              #'check-deletion-conditions)
      (declare (ignore non-overlapping-meaning-observation
                       non-overlapping-form-observation
                       overlapping-meaning-observation
                       overlapping-form-observation))

      (when cxn
        (let* (;; cxns and links from iterating over all repairs
               (cxns-and-links-holistic-part-cxn (handle-potential-holistic-cxn non-overlapping-form-cxn non-overlapping-meaning-cxn (append overlapping-meaning-cxn parent-meaning) cxn-inventory))
               (cxns-and-links-holistic-part-observation (do-create-holistic-cxn form-constraints meaning parent-meaning (processing-cxn-inventory cxn-inventory) nil))
               
               
               ;; surrounding item-based cxn
               (item-based-cxn-variants (multiple-value-list (create-item-based-cxn cxn-inventory
                                                                                    overlapping-form-cxn
                                                                                    non-overlapping-form-cxn
                                                                                    overlapping-meaning-cxn
                                                                                    non-overlapping-meaning-cxn
                                                                                    (extract-meaning-predicates cxn)
                                                                                    top-args                                                                                    (extract-args-from-meaning-networks non-overlapping-meaning-cxn (append overlapping-meaning-cxn parent-meaning) meaning-representation-formalism)
                                                                                    meaning-representation-formalism
                                                                                    'holistic->item-based--deletion)))
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
        
          (apply-fix
           form-constraints
           cxns-to-apply
           cat-links-to-add
           cxns-to-consolidate
           cats-to-add
           lex-class-item-based-cxn
           t
           node
           'holistic->item-based--deletion
           ))))))