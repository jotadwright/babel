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
  "Creates item-based construction and holistic constructions
based on existing construction with sufficient overlap."
  (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (random-elt (get-data problem :utterances)))
         (utterance-form-constraints (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings)) meaning-representation-formalism))
         ) 

    (multiple-value-bind (non-overlapping-meaning-observation
                          non-overlapping-meaning-cxn
                          non-overlapping-form-observation
                          non-overlapping-form-cxn
                          overlapping-meaning-observation
                          overlapping-form-observation
                          overlapping-form-cxn
                          cxn)
        (select-cxn-for-making-item-based-cxn cxn-inventory utterance-form-constraints meaning meaning-representation-formalism)
      
      (when (and cxn overlapping-form-cxn)
        
        (let* ((cxn-name-item-based-cxn
                (make-cxn-name (substitute-slot-meets-constraints non-overlapping-form-observation overlapping-form-observation) cxn-inventory :add-numeric-tail t))
               (cxn-name-holistic-cxn-1 (make-cxn-name non-overlapping-form-cxn cxn-inventory :add-numeric-tail t))
               (cxn-name-holistic-cxn-2 (make-cxn-name non-overlapping-form-observation cxn-inventory :add-numeric-tail t))
               (holistic-cxn-1
                (find-cxn-by-form-and-meaning non-overlapping-form-cxn non-overlapping-meaning-cxn cxn-inventory))
               (holistic-cxn-2
                (find-cxn-by-form-and-meaning non-overlapping-form-observation non-overlapping-meaning-observation cxn-inventory))
               
               ;; holistic cxn boundaries (leftmost/rightmost)
               (boundaries-cxn-1 (get-boundary-units non-overlapping-form-cxn))
               (leftmost-unit-holistic-cxn-1 (first boundaries-cxn-1))
               (rightmost-unit-holistic-cxn-1 (second boundaries-cxn-1))

               (boundaries-cxn-2 (get-boundary-units non-overlapping-form-observation))
               (leftmost-unit-holistic-cxn-2 (first boundaries-cxn-2))
               (rightmost-unit-holistic-cxn-2 (second boundaries-cxn-2))
               
               (overlapping-form-and-rewritten-boundaries (multiple-value-list (add-boundaries-to-form-constraints overlapping-form-observation boundaries-cxn-2)))
               (overlapping-form-with-rewritten-boundaries (first overlapping-form-and-rewritten-boundaries))
               (rewritten-boundaries (second overlapping-form-and-rewritten-boundaries))

               (existing-item-based-cxn (find-cxn-by-form-and-meaning
                                         overlapping-form-with-rewritten-boundaries
                                         overlapping-meaning-observation
                                         cxn-inventory
                                         :cxn-type 'item-based))
               ;; unit names
               (unit-name-holistic-cxn-1
                leftmost-unit-holistic-cxn-1)
               (unit-name-holistic-cxn-2
                leftmost-unit-holistic-cxn-2)
               ;; lex classes
               (lex-class-holistic-cxn-1
                (if holistic-cxn-1
                  (lex-class-cxn holistic-cxn-1)
                  (make-lex-class cxn-name-holistic-cxn-1 :trim-cxn-suffix t)))
               (lex-class-holistic-cxn-2
                (if holistic-cxn-2
                  (lex-class-cxn holistic-cxn-2)
                  (make-lex-class cxn-name-holistic-cxn-2 :trim-cxn-suffix t)))
               (lex-class-item-based-cxn
                (if existing-item-based-cxn
                  (lex-class-cxn existing-item-based-cxn)
                  (make-lex-class (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-(x)") :trim-cxn-suffix t))) 
               ;; categorial links
               (categorial-link-1
                (cons lex-class-holistic-cxn-1 lex-class-item-based-cxn))
               (categorial-link-2
                (cons lex-class-holistic-cxn-2 lex-class-item-based-cxn))
               ;; args
               (args-holistic-cxn-1
                (extract-args-from-meaning-network non-overlapping-meaning-cxn meaning-representation-formalism))
               (args-holistic-cxn-2
                (extract-args-from-meaning-network non-overlapping-meaning-observation meaning-representation-formalism))
               (hash-string (third (find 'string non-overlapping-form-cxn :key #'first)))
               ;; cxns
               
               (new-holistic-cxn-1
                (or holistic-cxn-1
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-1
                                                                ((,unit-name-holistic-cxn-1
                                                                  (args ,args-holistic-cxn-1)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-1))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-1)
                                                                   (right ,rightmost-unit-holistic-cxn-1)))
                                                                 <-
                                                                 (,unit-name-holistic-cxn-1
                                                                  (HASH meaning ,non-overlapping-meaning-cxn)
                                                                  --
                                                                  (HASH form ,non-overlapping-form-cxn)))
                                                                :attributes (:cxn-type holistic
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-cxn :key #'first))
                                                                             :string ,hash-string)
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (new-holistic-cxn-2
                (or holistic-cxn-2
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-2
                                                                ((,unit-name-holistic-cxn-2
                                                                  (args ,args-holistic-cxn-2)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-2))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-2)
                                                                   (right ,rightmost-unit-holistic-cxn-2)))
                                                                 <-
                                                                 (,unit-name-holistic-cxn-2
                                                                  (HASH meaning ,non-overlapping-meaning-observation)
                                                                  --
                                                                  (HASH form ,non-overlapping-form-observation)))
                                                                :attributes (:cxn-type holistic
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-observation :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form-observation :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (new-item-based-cxn
                (or existing-item-based-cxn ; todo, check if it can apply! the order of args could be different...
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-item-based-cxn
                                                                ((?item-based-unit
                                                                  (syn-cat (phrase-type item-based))
                                                                  (subunits (,unit-name-holistic-cxn-2)))
                                                                 (,unit-name-holistic-cxn-2
                                                                  (syn-cat (lex-class ,lex-class-item-based-cxn))
                                                                  (boundaries
                                                                   (left ,(first rewritten-boundaries))
                                                                   (right ,(second rewritten-boundaries)))
                                                                  )
                                                                 <-
                                                                 (?item-based-unit
                                                                  (HASH meaning ,overlapping-meaning-observation)
                                                                  --
                                                                  (HASH form ,overlapping-form-with-rewritten-boundaries)) ;; (HASH form ,overlapping-form-observation) ; this works
                                                                 (,unit-name-holistic-cxn-2
                                                                  (args ,args-holistic-cxn-2)
                                                                  (syn-cat (phrase-type holistic))
                                                                  --
                                                                  ))
                                                                :attributes (:cxn-type item-based
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(loop for predicate in overlapping-meaning-observation
                                                                                             unless (or
                                                                                                     (equal (first predicate) 'get-context)
                                                                                                     (equal (first predicate) 'bind))
                                                                                             return (first predicate))
                                                                             :string ,(third (find 'string overlapping-form-observation :key #'first)))
                                                                           
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (existing-cxns (list holistic-cxn-2 holistic-cxn-1 existing-item-based-cxn))
               (cxns-to-apply (list new-holistic-cxn-2 new-item-based-cxn))
               (cat-links-to-add (list categorial-link-1 categorial-link-2)) 
               (cxns-to-consolidate (loop for cxn in (list new-holistic-cxn-1 new-holistic-cxn-2 new-item-based-cxn)
                                          when (not (member cxn existing-cxns))
                                          collect cxn)))
                                          
          (list
           cxns-to-apply
           cat-links-to-add
           cxns-to-consolidate
           ))))))


