(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add item-based construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass holophrase->item-based+holistic+holistic--substitution (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holophrase->item-based+holistic+holistic--substitution)
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

(defmethod repair ((repair holophrase->item-based+holistic+holistic--substitution)
                   (problem non-gold-standard-utterance)
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
   (construction-inventory node)))

(defun handle-potential-holistic-cxn (form meaning cxn-inventory)
  (cond ((do-create-categorial-links form meaning (processing-cxn-inventory cxn-inventory)))
        ;((do-create-item-based-cxn-from-partial-holistic-analysis form meaning (processing-cxn-inventory cxn-inventory)))
        ((do-repair-holophrase->item-based+holistic+holistic--substitution form meaning (processing-cxn-inventory cxn-inventory)))
        ;((do-repair-holophrase->item-based+holistic--addition form meaning (processing-cxn-inventory cxn-inventory)))
        ;((do-repair-holophrase->item-based+holistic+holophrase--deletion form meaning (processing-cxn-inventory cxn-inventory)))
        ;((do-create-holistic-cxn-from-partial-analysis form meaning (processing-cxn-inventory cxn-inventory)))
        (t
         (do-create-holistic-cxn form meaning (processing-cxn-inventory cxn-inventory))))
  )



(defun do-repair-holophrase->item-based+holistic+holistic--substitution (form-constraints meaning cxn-inventory)
  "Creates item-based construction and holistic constructions
based on existing construction with sufficient overlap."
  (let* ((cxn-inventory (original-cxn-set cxn-inventory))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         ) 

    (multiple-value-bind (non-overlapping-meaning-observation
                          non-overlapping-meaning-cxn
                          non-overlapping-form-observation
                          non-overlapping-form-cxn
                          overlapping-meaning-observation
                          overlapping-form-observation
                          cxn)
        (select-cxn-for-making-item-based-cxn cxn-inventory form-constraints meaning meaning-representation-formalism)
      
      (when cxn
        
        (let* ((cxn-name-item-based-cxn
                (make-cxn-name (substitute-slot-meets-constraints non-overlapping-form-observation overlapping-form-observation) cxn-inventory :add-numeric-tail t))
               (cxn-name-item-based-cxn-apply-last
                (intern (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-LAST")))
               (cxn-name-item-based-cxn-apply-first
                (intern (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-FIRST")))
               

               ;; holistic cxn boundaries (leftmost/rightmost)
               (boundaries-cxn-2 (get-boundary-units non-overlapping-form-observation))
               (overlapping-form-and-rewritten-boundaries
                (multiple-value-list (add-boundaries-to-form-constraints overlapping-form-observation boundaries-cxn-2)))
               (overlapping-form-with-rewritten-boundaries (first overlapping-form-and-rewritten-boundaries))
               (rewritten-boundaries (second overlapping-form-and-rewritten-boundaries))
               (dummy-slot-fc (list (list 'fcg::meets (first rewritten-boundaries) (second rewritten-boundaries))))
               (rewritten-item-based-boundaries (get-boundary-units (append dummy-slot-fc overlapping-form-with-rewritten-boundaries)))
               ;(rewritten-item-based-boundaries (fix-dummy-edge-boundaries temp-item-based-boundaries rewritten-boundaries))
               
               
               
               
               

               ;; cxns and links from iterating over all repairs
               (cxns-and-links-holistic-part-observation (handle-potential-holistic-cxn non-overlapping-form-observation non-overlapping-meaning-observation cxn-inventory))
               (cxns-and-links-holistic-part-cxn (handle-potential-holistic-cxn non-overlapping-form-cxn non-overlapping-meaning-cxn cxn-inventory))

               ;; args
               (slot-args (extract-args-from-meaning-networks non-overlapping-meaning-observation meaning meaning-representation-formalism))
               ;(slot-args (extract-args-apply-first (last-elt (first cxns-and-links-holistic-part-observation)))) ; this should work too!
               
               
               
               (item-based-args (extract-args-from-meaning-networks meaning nil meaning-representation-formalism))
               
               
               (existing-item-based-cxn-apply-last (find-cxn-by-form-and-meaning
                                                    overlapping-form-with-rewritten-boundaries
                                                    overlapping-meaning-observation
                                                    (list slot-args)
                                                    cxn-inventory
                                                    :cxn-type 'item-based
                                                    :cxn-set 'fcg::routine))

               (existing-item-based-cxn-apply-first (when existing-item-based-cxn-apply-last
                                                      (alter-ego-cxn existing-item-based-cxn-apply-last cxn-inventory)))
               
               ;; lex classes
               (lex-class-item-based-cxn
                (if existing-item-based-cxn-apply-first
                  (extract-contributing-lex-class existing-item-based-cxn-apply-first)
                  (make-lex-class (symbol-name cxn-name-item-based-cxn) :trim-cxn-suffix t)))
               (lex-class-item-based-cxn-slot
                (if existing-item-based-cxn-apply-first
                  (lex-class-cxn existing-item-based-cxn-apply-first)
                  (make-lex-class (concatenate 'string (symbol-name lex-class-item-based-cxn) "-(x)"))))

               
               (new-item-based-cxn-apply-last
                (or existing-item-based-cxn-apply-last 
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-item-based-cxn-apply-last
                                                                ((?item-based-unit
                                                                  (syn-cat (phrase-type item-based)
                                                                           (lex-class ,lex-class-item-based-cxn))
                                                                  (boundaries
                                                                   (left ,(first rewritten-item-based-boundaries))
                                                                   (right ,(second rewritten-item-based-boundaries)))
                                                                  (args ,item-based-args)
                                                                  (subunits (?slot-unit)))
                                                                 <-
                                                                 (?item-based-unit
                                                                  (HASH meaning ,overlapping-meaning-observation)
                                                                  --
                                                                  (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                                 (?slot-unit
                                                                  (args ,slot-args)
                                                                  --
                                                                  (syn-cat (lex-class ,lex-class-item-based-cxn-slot))
                                                                  (boundaries
                                                                   (left ,(first rewritten-boundaries))
                                                                   (right ,(second rewritten-boundaries)))
                                                                  ))
                                                                :attributes (:label fcg::routine
                                                                             :cxn-type item-based
                                                                             :bare-cxn-name ,cxn-name-item-based-cxn
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(loop for predicate in overlapping-meaning-observation
                                                                                             unless (or
                                                                                                     (equal (first predicate) 'get-context)
                                                                                                     (equal (first predicate) 'bind))
                                                                                             return (first predicate))
                                                                             :string ,(third (find 'string overlapping-form-observation :key #'first)))
                                                                           
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (new-item-based-cxn-apply-first
                (or existing-item-based-cxn-apply-first
                    (second (multiple-value-list (eval
                                                `(def-fcg-cxn ,cxn-name-item-based-cxn-apply-first
                                                                ((?item-based-unit
                                                                  (syn-cat (phrase-type item-based)
                                                                           (lex-class ,lex-class-item-based-cxn))
                                                                  (boundaries
                                                                   (left ,(first rewritten-item-based-boundaries))
                                                                   (right ,(second rewritten-item-based-boundaries)))
                                                                  (args ,item-based-args)
                                                                  (subunits (?slot-unit)))
                                                                 (?slot-unit 
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-item-based-cxn-slot))
                                                                  (args ,slot-args)
                                                                  (boundaries
                                                                   (left ,(first rewritten-boundaries))
                                                                   (right ,(second rewritten-boundaries)))
                                                                  )
                                                                 <-
                                                                 (?item-based-unit
                                                                  (HASH meaning ,overlapping-meaning-observation)
                                                                  --
                                                                  (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                                 )
                                                                :attributes (:label fcg::meta-only
                                                                             :cxn-type item-based
                                                                             :bare-cxn-name ,cxn-name-item-based-cxn
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(loop for predicate in overlapping-meaning-observation
                                                                                             unless (or
                                                                                                     (equal (first predicate) 'get-context)
                                                                                                     (equal (first predicate) 'bind))
                                                                                             return (first predicate))
                                                                             :string ,(third (find 'string overlapping-form-observation :key #'first)))
                                                                           
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               
               (cxns-to-apply (append (first cxns-and-links-holistic-part-observation) (list new-item-based-cxn-apply-last)))
               (cat-links-to-add (remove nil (append (second cxns-and-links-holistic-part-observation)
                                         (second cxns-and-links-holistic-part-cxn)
                                         (list (when (first (fourth cxns-and-links-holistic-part-observation)) ;add-categorial links can return nil!
                                                 (cons (first (fourth cxns-and-links-holistic-part-observation))
                                                       lex-class-item-based-cxn-slot))
                                               (when (first (fourth cxns-and-links-holistic-part-cxn))
                                                   (cons (first (fourth cxns-and-links-holistic-part-cxn))
                                                     lex-class-item-based-cxn-slot))))))
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
           ))))))


