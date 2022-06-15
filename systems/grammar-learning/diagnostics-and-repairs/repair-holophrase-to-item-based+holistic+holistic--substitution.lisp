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

(defun create-item-based-cxn (cxn-inventory
                              overlapping-form
                              non-overlapping-form
                              overlapping-meaning
                              non-overlapping-meaning
                              meaning
                              meaning-representation-formalism)             
  (let* (;; cxn names
         (cxn-name-item-based-cxn
          (make-cxn-name (substitute-slot-meets-constraints non-overlapping-form overlapping-form) cxn-inventory :add-numeric-tail t))
         (cxn-name-item-based-cxn-apply-last (intern (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-LAST")))
         (cxn-name-item-based-cxn-apply-first (intern (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-FIRST")))
         ;; slot boundaries (leftmost/rightmost)
         (slot-boundaries (get-boundary-units non-overlapping-form))
         (overlapping-form-and-rewritten-boundaries
          (multiple-value-list (add-boundaries-to-form-constraints overlapping-form slot-boundaries)))
         (overlapping-form-with-rewritten-boundaries (first overlapping-form-and-rewritten-boundaries))
         (rewritten-boundaries (second overlapping-form-and-rewritten-boundaries))
         (dummy-slot-fc (list (list 'fcg::meets (first rewritten-boundaries) (second rewritten-boundaries))))
         (rewritten-item-based-boundaries (get-boundary-units (append dummy-slot-fc overlapping-form-with-rewritten-boundaries)))
         
         ;; args
         (slot-args (extract-args-from-meaning-networks non-overlapping-meaning meaning meaning-representation-formalism))
         ;(alt-slot-args (extract-args-apply-first (last-elt (first cxns-and-links-holistic-part)))) ; this should work too!
         (item-based-args (extract-args-from-meaning-networks meaning nil meaning-representation-formalism))
         (existing-item-based-cxn-apply-last (find-cxn-by-form-and-meaning
                                              overlapping-form-with-rewritten-boundaries
                                              overlapping-meaning
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
                                                           (?slot-unit 
                                                            (footprints (used-as-slot-filler)))
                                                           <-
                                                           (?item-based-unit
                                                            (HASH meaning ,overlapping-meaning)
                                                            --
                                                            (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                           (?slot-unit
                                                            (footprints (NOT used-as-slot-filler))
                                                            (args ,slot-args)
                                                            --
                                                            (footprints (NOT used-as-slot-filler))
                                                            (syn-cat (lex-class ,lex-class-item-based-cxn-slot))
                                                            (boundaries
                                                             (left ,(first rewritten-boundaries))
                                                             (right ,(second rewritten-boundaries)))
                                                            ))
                                                          :attributes (:label fcg::routine
                                                                       :cxn-type item-based
                                                                       :bare-cxn-name ,cxn-name-item-based-cxn
                                                                       :repair holophrase->item-based+holistic+holistic--substitution
                                                                       :meaning ,(loop for predicate in overlapping-meaning
                                                                                       unless (or
                                                                                               (equal (first predicate) 'get-context)
                                                                                               (equal (first predicate) 'bind))
                                                                                       return (first predicate))
                                                                       :string ,(third (find 'string overlapping-form :key #'first)))
                                                                           
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
                                                            (footprints (used-as-slot-filler))
                                                            (syn-cat (phrase-type holistic)
                                                                     (lex-class ,lex-class-item-based-cxn-slot))
                                                            (args ,slot-args)
                                                            (boundaries
                                                             (left ,(first rewritten-boundaries))
                                                             (right ,(second rewritten-boundaries)))
                                                            )
                                                           <-
                                                           (?item-based-unit
                                                            (HASH meaning ,overlapping-meaning)
                                                            --
                                                            (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                           )
                                                          :attributes (:label fcg::meta-only
                                                                       :cxn-type item-based
                                                                       :bare-cxn-name ,cxn-name-item-based-cxn
                                                                       :repair holophrase->item-based+holistic+holistic--substitution
                                                                       :meaning ,(loop for predicate in overlapping-meaning
                                                                                       unless (or
                                                                                               (equal (first predicate) 'get-context)
                                                                                               (equal (first predicate) 'bind))
                                                                                       return (first predicate))
                                                                       :string ,(third (find 'string overlapping-form :key #'first)))
                                                                           
                                                          :cxn-inventory ,(copy-object cxn-inventory))))))))
    (values new-item-based-cxn-apply-first
            new-item-based-cxn-apply-last
            lex-class-item-based-cxn
            lex-class-item-based-cxn-slot)))


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
        
        (let* (;; cxns and links from iterating over all repairs
               (cxns-and-links-holistic-part-observation (handle-potential-holistic-cxn non-overlapping-form-observation non-overlapping-meaning-observation cxn-inventory))
               (cxns-and-links-holistic-part-cxn (handle-potential-holistic-cxn non-overlapping-form-cxn non-overlapping-meaning-cxn cxn-inventory))
               ;; surrounding item-based cxn
               (item-based-cxn-variants (multiple-value-list (create-item-based-cxn cxn-inventory
                                                           overlapping-form-observation
                                                           non-overlapping-form-observation
                                                           overlapping-meaning-observation
                                                           non-overlapping-meaning-observation
                                                           meaning
                                                           meaning-representation-formalism)))
               (new-item-based-cxn-apply-first (first item-based-cxn-variants))
               (new-item-based-cxn-apply-last (second item-based-cxn-variants))
               (lex-class-item-based-cxn (third item-based-cxn-variants))
               (lex-class-item-based-cxn-slot (fourth item-based-cxn-variants))

               ;; build result
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


