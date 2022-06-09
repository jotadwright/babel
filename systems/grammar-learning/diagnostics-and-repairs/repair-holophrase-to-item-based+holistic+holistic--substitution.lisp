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
                          ;overlapping-meaning-cxn
                          overlapping-form-observation
                          args-holistic-cxn-1
                          args-holistic-cxn-2
                          cxn)
        (select-cxn-for-making-item-based-cxn cxn-inventory utterance-form-constraints meaning meaning-representation-formalism)
      
      (when cxn
        
        (let* ((cxn-name-item-based-cxn
                (make-cxn-name (substitute-slot-meets-constraints non-overlapping-form-observation overlapping-form-observation) cxn-inventory :add-numeric-tail t))
               (cxn-name-item-based-cxn-apply-last
                (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-LAST"))
               (cxn-name-item-based-cxn-apply-first
                (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-FIRST"))
               (cxn-name-holistic-cxn-1 (make-cxn-name non-overlapping-form-cxn cxn-inventory :add-numeric-tail t))
               (cxn-name-holistic-cxn-1-apply-last (concatenate 'string (symbol-name cxn-name-holistic-cxn-1) "-APPLY-LAST"))
               (cxn-name-holistic-cxn-1-apply-first (concatenate 'string (symbol-name cxn-name-holistic-cxn-1) "-APPLY-FIRST"))
               (cxn-name-holistic-cxn-2 (make-cxn-name non-overlapping-form-observation cxn-inventory :add-numeric-tail t))
               (cxn-name-holistic-cxn-2-apply-last (concatenate 'string (symbol-name cxn-name-holistic-cxn-2) "-APPLY-LAST"))
               (cxn-name-holistic-cxn-2-apply-first (concatenate 'string (symbol-name cxn-name-holistic-cxn-2) "-APPLY-FIRST"))
               
               ;; check for existing cxns
               (holistic-cxn-1-apply-first
                (find-cxn-by-form-and-meaning non-overlapping-form-cxn non-overlapping-meaning-cxn cxn-inventory :cxn-set 'fcg::routine :cxn-type 'holistic))
               (holistic-cxn-1-apply-last
                (find-cxn-by-form-and-meaning non-overlapping-form-cxn non-overlapping-meaning-cxn cxn-inventory :cxn-set 'fcg::meta-only :cxn-type 'holistic))
               (holistic-cxn-2-apply-first
                (find-cxn-by-form-and-meaning non-overlapping-form-observation non-overlapping-meaning-observation cxn-inventory :cxn-set 'fcg::routine :cxn-type 'holistic))
               (holistic-cxn-2-apply-last
                (find-cxn-by-form-and-meaning non-overlapping-form-observation non-overlapping-meaning-observation cxn-inventory :cxn-set 'fcg::meta-only :cxn-type 'holistic))
               
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

               (existing-item-based-cxn-apply-first (find-cxn-by-form-and-meaning
                                                     overlapping-form-with-rewritten-boundaries
                                                     overlapping-meaning-observation
                                                     cxn-inventory
                                                     :cxn-type 'item-based
                                                     :cxn-set 'fcg::meta-only))
               (existing-item-based-cxn-apply-last (find-cxn-by-form-and-meaning
                                                    overlapping-form-with-rewritten-boundaries
                                                    overlapping-meaning-observation
                                                    cxn-inventory
                                                    :cxn-type 'item-based
                                                    :cxn-set 'fcg::routine))
               ;; unit names
               (unit-name-holistic-cxn-1
                leftmost-unit-holistic-cxn-1)
               (unit-name-holistic-cxn-2
                ;; fix for j-unit bug, the unit name of a unit with an empty comprehension lock needs to be part of the meets constraints
                (if (member leftmost-unit-holistic-cxn-2 (apply 'concatenate 'list overlapping-form-with-rewritten-boundaries))
                  leftmost-unit-holistic-cxn-2
                  rightmost-unit-holistic-cxn-2))
               ;; lex classes
               (lex-class-holistic-cxn-1
                (if holistic-cxn-1-apply-first
                  (lex-class-cxn holistic-cxn-1-apply-first)
                  (make-lex-class cxn-name-holistic-cxn-1 :trim-cxn-suffix t)))
               (lex-class-holistic-cxn-2
                (if holistic-cxn-2-apply-first
                  (lex-class-cxn holistic-cxn-2-apply-first)
                  (make-lex-class cxn-name-holistic-cxn-2 :trim-cxn-suffix t)))
               (lex-class-item-based-cxn
                (if existing-item-based-cxn-apply-first
                  (lex-class-cxn existing-item-based-cxn-apply-first)
                  (make-lex-class (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-(x)") :trim-cxn-suffix t)))
               
               ;; categorial links
               (categorial-link-1
                (cons lex-class-holistic-cxn-1 lex-class-item-based-cxn))
               (categorial-link-2
                (cons lex-class-holistic-cxn-2 lex-class-item-based-cxn))
                
               ;; cxns
               (new-holistic-cxn-1
                (or holistic-cxn-1-apply-first
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-1-apply-first
                                                                ((?holistic-unit
                                                                  (args ,args-holistic-cxn-1)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-1))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-1)
                                                                   (right ,rightmost-unit-holistic-cxn-1)))
                                                                 <-
                                                                 (?holistic-unit
                                                                  (HASH meaning ,non-overlapping-meaning-cxn)
                                                                  --
                                                                  (HASH form ,non-overlapping-form-cxn)))
                                                                :attributes (:label fcg::routine
                                                                             :cxn-type holistic
                                                                             :bare-cxn-name ,cxn-name-holistic-cxn-1
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-cxn :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form-cxn :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               
               (new-holistic-cxn-1-apply-last
                (or holistic-cxn-1-apply-last
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-1-apply-last
                                                                (
                                                                 <-
                                                                 (?holistic-unit
                                                                  (HASH meaning ,non-overlapping-meaning-cxn)
                                                                  (args ,args-holistic-cxn-1)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-1))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-1)
                                                                   (right ,rightmost-unit-holistic-cxn-1))
                                                                  --
                                                                  (HASH form ,non-overlapping-form-cxn)
                                                                  (args ,args-holistic-cxn-1)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-1))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-1)
                                                                   (right ,rightmost-unit-holistic-cxn-1))))
                                                                :attributes (:label fcg::meta-only
                                                                             :cxn-type holistic
                                                                             :bare-cxn-name ,cxn-name-holistic-cxn-1
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-cxn :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form-cxn :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (new-holistic-cxn-2
                (or holistic-cxn-2-apply-first
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-2-apply-first
                                                                ((?holistic-unit
                                                                  (args ,args-holistic-cxn-2)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-2))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-2)
                                                                   (right ,rightmost-unit-holistic-cxn-2)))
                                                                 <-
                                                                 (?holistic-unit
                                                                  (HASH meaning ,non-overlapping-meaning-observation)
                                                                  --
                                                                  (HASH form ,non-overlapping-form-observation)))
                                                                :attributes (:label fcg::routine
                                                                             :cxn-type holistic
                                                                             :bare-cxn-name ,cxn-name-holistic-cxn-2
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-observation :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form-observation :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (new-holistic-cxn-2-apply-last
                (or holistic-cxn-2-apply-last
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-2-apply-last
                                                                (
                                                                 <-
                                                                 (?holistic-unit
                                                                  (HASH meaning ,non-overlapping-meaning-observation)
                                                                  (args ,args-holistic-cxn-2)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-2))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-2)
                                                                   (right ,rightmost-unit-holistic-cxn-2))
                                                                  --
                                                                  (HASH form ,non-overlapping-form-observation)
                                                                  (args ,args-holistic-cxn-2)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-2))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-2)
                                                                   (right ,rightmost-unit-holistic-cxn-2))))
                                                                :attributes (:label fcg::meta-only
                                                                             :cxn-type holistic
                                                                             :bare-cxn-name ,cxn-name-holistic-cxn-2
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-observation :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form-observation :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (new-item-based-cxn-apply-last
                (or existing-item-based-cxn-apply-last ; todo, check if it can apply! the order of args could be different...
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-item-based-cxn-apply-last
                                                                ((?item-based-unit
                                                                  (syn-cat (phrase-type item-based))
                                                                  (subunits (,unit-name-holistic-cxn-2)))
                                                                 <-
                                                                 (?item-based-unit
                                                                  (HASH meaning ,overlapping-meaning-observation)
                                                                  --
                                                                  (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                                 (,unit-name-holistic-cxn-2
                                                                  (args ,args-holistic-cxn-2)
                                                                  --
                                                                  (syn-cat (lex-class ,lex-class-item-based-cxn))
                                                                  (boundaries
                                                                   (left ,(first rewritten-boundaries)) ;todo make new var that isn't equal to unit name
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
                                                                  (syn-cat (phrase-type item-based))
                                                                  (subunits (,unit-name-holistic-cxn-2)))
                                                                 (,unit-name-holistic-cxn-2 
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-item-based-cxn))
                                                                  (args ,args-holistic-cxn-2)
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
               (existing-cxns (list holistic-cxn-2-apply-first
                                    holistic-cxn-2-apply-last
                                    holistic-cxn-1-apply-first
                                    holistic-cxn-1-apply-last
                                    existing-item-based-cxn-apply-first
                                    existing-item-based-cxn-apply-last))
               (new-cxns (list new-holistic-cxn-1
                               new-holistic-cxn-2
                               new-holistic-cxn-1-apply-last
                               new-holistic-cxn-2-apply-last
                               new-item-based-cxn-apply-first
                               new-item-based-cxn-apply-last))
               (cxns-to-apply (list new-holistic-cxn-2 new-item-based-cxn-apply-last))
               (cat-links-to-add (list categorial-link-1 categorial-link-2)) 
               (cxns-to-consolidate (loop for cxn in new-cxns
                                          unless (or (member cxn existing-cxns)
                                                     (member cxn cxns-to-apply))
                                          collect cxn)))
                                  
          (list
           cxns-to-apply
           cat-links-to-add
           cxns-to-consolidate
           ))))))


