(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Holophrase Deletion  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass holophrase->item-based+holistic+holophrase--deletion (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node))) ;; it's always fcg::new-node, we created a new node in the search process

(defmethod repair ((repair holophrase->item-based+holistic+holophrase--deletion)
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

(defmethod repair ((repair holophrase->item-based+holistic+holophrase--deletion)
                   (problem non-gold-standard-utterance)
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

(defun repair-holophrase->item-based+holistic+holophrase--deletion (problem node) ;;node = cip node (transient struct, applied cxns, cxn-inventory, ..)
  "Creates item-based construction, a holophrase and a holistic construction
   based on an existing holophrase construction of which the form/meaning are a superset of the observed phrase.
   "
  (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))
                                                                   meaning-representation-formalism))
         
         (utterance (random-elt (get-data problem :utterances))))
    (multiple-value-bind (superset-holophrase-cxn
                          non-overlapping-form
                          non-overlapping-meaning)
        (find-superset-holophrase-cxn cxn-inventory gold-standard-meaning utterance meaning-representation-formalism)

      (when superset-holophrase-cxn
        (let* ((overlapping-form
                (set-difference (extract-form-predicates superset-holophrase-cxn) non-overlapping-form :test #'equal))
               (overlapping-meaning
                (set-difference (extract-meaning-predicates superset-holophrase-cxn) non-overlapping-meaning :test #'equal))
               (existing-holistic-cxn-apply-first
                (find-cxn-by-form-and-meaning non-overlapping-form non-overlapping-meaning cxn-inventory :cxn-set 'routine))
               (existing-holistic-cxn-apply-last
                (find-cxn-by-form-and-meaning non-overlapping-form non-overlapping-meaning cxn-inventory :cxn-set 'meta-only))
               (boundaries-holistic-cxn (get-boundary-units non-overlapping-form))
               (overlapping-form-and-rewritten-boundaries (multiple-value-list (add-boundaries-to-form-constraints overlapping-form boundaries-holistic-cxn)))
               (overlapping-form-with-rewritten-boundaries (first overlapping-form-and-rewritten-boundaries))
               (rewritten-boundaries (second overlapping-form-and-rewritten-boundaries))                                         
               (leftmost-unit-holistic-cxn (first boundaries-holistic-cxn))
               (rightmost-unit-holistic-cxn (second boundaries-holistic-cxn))
               (holistic-cxn-name
                (make-cxn-name non-overlapping-form cxn-inventory :add-numeric-tail t))
               (cxn-name-holistic-cxn-apply-last (concatenate 'string (symbol-name holistic-cxn-name) "-APPLY-LAST"))
               (cxn-name-holistic-cxn-apply-first (concatenate 'string (symbol-name holistic-cxn-name) "-APPLY-FIRST"))
               (cxn-name-item-based-cxn (make-cxn-name
                                         (substitute-slot-meets-constraints non-overlapping-form overlapping-form) cxn-inventory :add-numeric-tail t))
               (cxn-name-item-based-cxn-apply-last
                (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-LAST"))
               (cxn-name-item-based-cxn-apply-first
                (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-FIRST"))
               (existing-item-based-cxn-apply-first (find-cxn-by-form-and-meaning
                                         overlapping-form-with-rewritten-boundaries
                                         overlapping-meaning
                                         cxn-inventory
                                         :cxn-type 'item-based
                                         :cxn-set 'meta-only))
               (existing-item-based-cxn-apply-last (find-cxn-by-form-and-meaning
                                         overlapping-form-with-rewritten-boundaries
                                         overlapping-meaning
                                         cxn-inventory
                                         :cxn-type 'item-based
                                         :cxn-set 'routine))
               (unit-name-holistic-cxn
                ;; fix for j-unit bug, the unit name of a unit with an empty comprehension lock needs to be part of the meets constraints
                (if (member leftmost-unit-holistic-cxn (apply 'concatenate 'list overlapping-form-with-rewritten-boundaries))
                  leftmost-unit-holistic-cxn
                  rightmost-unit-holistic-cxn))
               ;; lex-class
               (lex-class-holistic-cxn
                (if existing-holistic-cxn-apply-first
                  (lex-class-cxn existing-holistic-cxn-apply-first)
                  (make-lex-class holistic-cxn-name :trim-cxn-suffix t)))
               (lex-class-item-based-cxn
                (if existing-item-based-cxn-apply-first
                  (lex-class-cxn existing-item-based-cxn-apply-first)
                  (make-lex-class (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-(x)") :trim-cxn-suffix t)))
               ;; type hierachy links
               (categorial-link
                (cons lex-class-holistic-cxn lex-class-item-based-cxn))
               
               

               (meaning
                (meaning-predicates-with-variables (random-elt (get-data problem :meanings))
                                                   meaning-representation-formalism))
               ;; args: 
               (args-holistic-cxn
                (extract-args-from-meaning-networks non-overlapping-meaning overlapping-meaning meaning-representation-formalism))
               (args-holophrase-cxn (extract-args-from-meaning-networks meaning nil meaning-representation-formalism))
               (cxn-name
                (make-cxn-name utterance cxn-inventory :add-numeric-tail t))
               (form-constraints
                (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
               (boundaries-holophrase-cxn (get-boundary-units form-constraints))
               (leftmost-unit-holophrase-cxn (first boundaries-holophrase-cxn))
               (rightmost-unit-holophrase-cxn (second boundaries-holophrase-cxn))
               (holophrase-cxn
                (second (multiple-value-list  (eval
                                               `(def-fcg-cxn ,cxn-name
                                                             ((?holophrase-unit
                                                               (syn-cat (phrase-type holophrase))
                                                               (args ,args-holophrase-cxn)
                                                               (boundaries
                                                                   (left ,leftmost-unit-holophrase-cxn)
                                                                   (right ,rightmost-unit-holophrase-cxn)))
                                                               
                                                              <-
                                                              (?holophrase-unit
                                                               (HASH meaning ,meaning)
                                                               --
                                                               (HASH form ,form-constraints)))
                                                             :attributes (:cxn-type holophrase
                                                                          :repair holophrase->item-based+holistic+holophrase--deletion)
                                                             :cxn-inventory ,(copy-object cxn-inventory))))))
               (new-holistic-cxn-apply-first
                (or existing-holistic-cxn-apply-first
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-first
                                                                ((,unit-name-holistic-cxn
                                                                  (args ,args-holistic-cxn)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                   (right ,rightmost-unit-holistic-cxn)))
                                                                 <-
                                                                 (,unit-name-holistic-cxn
                                                                  (HASH meaning ,non-overlapping-meaning)
                                                                  --
                                                                  (HASH form ,non-overlapping-form)))
                                                                :attributes (:label routine
                                                                             :cxn-type holistic
                                                                             :repair holophrase->item-based+holistic+holophrase--deletion
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               
               (new-holistic-cxn-apply-last
                (or existing-holistic-cxn-apply-last
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-apply-last
                                                                (
                                                                 <-
                                                                 (,unit-name-holistic-cxn
                                                                  (HASH meaning ,non-overlapping-meaning)
                                                                  (args ,args-holistic-cxn)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                   (right ,rightmost-unit-holistic-cxn))
                                                                  --
                                                                  (HASH form ,non-overlapping-form)
                                                                  (args ,args-holistic-cxn)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                   (right ,rightmost-unit-holistic-cxn))))
                                                                :attributes (:label meta-only
                                                                             :cxn-type holistic
                                                                             :repair holophrase->item-based+holistic+holophrase--deletion
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
                 
               (new-item-based-cxn-apply-last
                (or existing-item-based-cxn-apply-last ; todo, check if it can apply! the order of args could be different...
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-item-based-cxn-apply-last
                                                                ((?item-based-unit
                                                                  (syn-cat (phrase-type item-based))
                                                                  (subunits (,unit-name-holistic-cxn)))
                                                                 <-
                                                                 (?item-based-unit
                                                                  (HASH meaning ,overlapping-meaning)
                                                                  --
                                                                  (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                                 (,unit-name-holistic-cxn
                                                                  (args ,args-holistic-cxn)
                                                                  --
                                                                  (syn-cat (lex-class ,lex-class-item-based-cxn))
                                                                  (boundaries
                                                                   (left ,(first rewritten-boundaries)) ;todo make new var that isn't equal to unit name
                                                                   (right ,(second rewritten-boundaries)))
                                                                  ))
                                                                :attributes (:label routine
                                                                             :cxn-type item-based
                                                                             :repair holophrase->item-based+holistic+holophrase--deletion
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
                                                                  (syn-cat (phrase-type item-based))
                                                                  (subunits (,unit-name-holistic-cxn)))
                                                                 (,unit-name-holistic-cxn 
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-item-based-cxn))
                                                                  (args ,args-holistic-cxn)
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
                                                                :attributes (:label meta-only
                                                                             :cxn-type item-based
                                                                             :repair holophrase->item-based+holistic+holophrase--deletion
                                                                             :meaning ,(loop for predicate in overlapping-meaning
                                                                                             unless (or
                                                                                                     (equal (first predicate) 'get-context)
                                                                                                     (equal (first predicate) 'bind))
                                                                                             return (first predicate))
                                                                             :string ,(third (find 'string overlapping-form :key #'first)))
                                                                           
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))

               (existing-cxns (list existing-holistic-cxn-apply-first
                                    existing-holistic-cxn-apply-last
                                    existing-item-based-cxn-apply-first
                                    existing-item-based-cxn-apply-last))
               (new-cxns (list new-holistic-cxn-apply-first
                               new-holistic-cxn-apply-last
                               new-item-based-cxn-apply-first
                               new-item-based-cxn-apply-last
                               holophrase-cxn))
               (cxns-to-apply (list holophrase-cxn))
               (cat-links-to-add (list categorial-link)) 
               (cxns-to-consolidate (loop for cxn in new-cxns
                                          unless (member cxn existing-cxns)
                                          collect cxn)))

          ;; return
          (list
           cxns-to-apply
           cat-links-to-add
           cxns-to-consolidate
           )
          )))))