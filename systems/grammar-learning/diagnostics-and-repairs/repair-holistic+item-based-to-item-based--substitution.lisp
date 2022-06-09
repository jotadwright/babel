(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair from holistic+item-based to item-based cxn through substitution;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass holistic+item-based->item-based--substitution (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holistic+item-based->item-based--substitution)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn-from-partial-holistic-analysis+similar-item-based-cxn--substitution problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defmethod repair ((repair holistic+item-based->item-based--substitution)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn-from-partial-holistic-analysis+similar-item-based-cxn--substitution problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defun create-item-based-cxn-from-partial-holistic-analysis+similar-item-based-cxn--substitution (problem node)
  
  "Creates item-based construction around matching holistic constructions"
  (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         
         (cxns-and-links (create-item-based-cxn-from-partial-holistic-analysis problem node)))
    (when cxns-and-links
      (let* ((cxns-to-apply (first cxns-and-links))
             (updated-boundary-name-and-args-list (fourth cxns-and-links))
             (intermediary-item-based-cxn (last-elt cxns-to-apply))
             
             
             (applied-holistic-cxns (remove intermediary-item-based-cxn cxns-to-apply))
             (alter-applied-holistic-cxns (mapcar #'(lambda (cxn) (alter-ego-cxn cxn cxn-inventory)) applied-holistic-cxns)))
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
            (select-item-based-cxn-for-making-item-based-cxn cxn-inventory intermediary-item-based-cxn meaning-representation-formalism)
          (when cxn
            ;; create newly learned holistic cxns, there should be two + two ordering variations (see substitution repair)
            (let* ((cxn-name-holistic-cxn-1 (make-cxn-name non-overlapping-form-cxn cxn-inventory :add-numeric-tail t))
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

                   ;; lex classes
                   (lex-class-holistic-cxn-1
                    (if holistic-cxn-1-apply-first
                      (lex-class-cxn holistic-cxn-1-apply-first)
                      (make-lex-class cxn-name-holistic-cxn-1 :trim-cxn-suffix t)))
                   (lex-class-holistic-cxn-2
                    (if holistic-cxn-2-apply-first
                      (lex-class-cxn holistic-cxn-2-apply-first)
                      (make-lex-class cxn-name-holistic-cxn-2 :trim-cxn-suffix t)))

                
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

                   (applied-holistic-cxns (sort-cxns-by-form-string (push new-holistic-cxn-2 applied-holistic-cxns) utterance cxn-inventory))
                   (alter-applied-holistic-cxns (sort-cxns-by-form-string (push new-holistic-cxn-2-apply-last alter-applied-holistic-cxns) utterance cxn-inventory))
               
                   (chunk-item-based-cxn-form-constraints (make-item-based-name-form-constraints-from-cxns overlapping-form-with-rewritten-boundaries (append (list rewritten-boundaries)
                                                                                                                                                              (mapcar #'first updated-boundary-name-and-args-list ))))
                   (placeholder-var-string-predicates (variablify-missing-form-strings chunk-item-based-cxn-form-constraints))
                   (cxn-name-item-based-cxn (make-cxn-name
                                             (append placeholder-var-string-predicates chunk-item-based-cxn-form-constraints)
                                             cxn-inventory :add-numeric-tail t :add-cxn-suffix nil))
                   (known-holistic-cxn-subunit-blocks (multiple-value-list
                                                       (loop for item in updated-boundary-name-and-args-list
                                                             for holistic-cxn-unit-name = (second item)
                                                             for updated-boundaries = (first item)
                                                             for holistic-slot-lex-class = (create-item-based-lex-class-with-var
                                                                                            placeholder-var-string-predicates
                                                                                            cxn-name-item-based-cxn
                                                                                            (first updated-boundaries)) ;; look up the X and Y in bindings
                                                             for args = (third item)
                                                             for holistic-cxn-lex-class = (fourth item) 
                                                             for categorial-link = (cons holistic-cxn-lex-class holistic-slot-lex-class)
                                                             collect holistic-cxn-unit-name into holistic-subunit-names
                                                             collect categorial-link into categorial-links
                                                             collect `(,holistic-cxn-unit-name
                                                                       (syn-cat (phrase-type holistic)
                                                                                (lex-class ,holistic-slot-lex-class))
                                                                       (args ,args)
                                                                       (boundaries
                                                                        (left ,(first updated-boundaries))
                                                                        (right ,(second updated-boundaries)))) into contributing-units-apply-first
                                                             collect `(,holistic-cxn-unit-name
                                                                       (args ,args)
                                                                       --
                                                                       (syn-cat (gl::lex-class ,holistic-slot-lex-class))
                                                                       (boundaries
                                                                        (left ,(first updated-boundaries))
                                                                        (right ,(second updated-boundaries)))) into conditional-units-apply-last
                                                             finally (return (values conditional-units-apply-last contributing-units-apply-first holistic-subunit-names categorial-links)))))
                   (holistic-slot-lex-class (create-item-based-lex-class-with-var
                                             placeholder-var-string-predicates
                                             cxn-name-item-based-cxn
                                             (first rewritten-boundaries)))
                   (categorial-link (cons lex-class-holistic-cxn-2 holistic-slot-lex-class))
                   (new-holistic-subunit-block-apply-first `(,leftmost-unit-holistic-cxn-2
                                                             (syn-cat (phrase-type holistic)
                                                                      (lex-class ,holistic-slot-lex-class))
                                                             (args ,args-holistic-cxn-2)
                                                             (boundaries
                                                              (left ,(first rewritten-boundaries))
                                                              (right ,(second rewritten-boundaries)))))
                   (new-holistic-subunit-block-apply-last `(,leftmost-unit-holistic-cxn-2
                                                            (args ,args-holistic-cxn-2)
                                                            --
                                                            (syn-cat (gl::lex-class ,holistic-slot-lex-class))
                                                            (boundaries
                                                             (left ,(first rewritten-boundaries))
                                                             (right ,(second rewritten-boundaries)))))
                   (combined-conditional-units (push new-holistic-subunit-block-apply-last (first known-holistic-cxn-subunit-blocks)))
                   (combined-contributing-units (push new-holistic-subunit-block-apply-first (second known-holistic-cxn-subunit-blocks)))
                   (dummy-meets-constraints (make-dummy-fc-from-unit-boundaries combined-conditional-units))
                                           
                     
                   (combined-fc (append overlapping-form-with-rewritten-boundaries dummy-meets-constraints))
                   (holistic-cxn-conditional-units (sort-units-by-meets-constraints combined-conditional-units
                                                                                    combined-fc
                                                                                    ))
                   (holistic-cxn-contributing-units (sort-units-by-meets-constraints combined-contributing-units
                                                                                     combined-fc))
                   (holistic-subunit-names (push leftmost-unit-holistic-cxn-2 (third known-holistic-cxn-subunit-blocks)))
                   (cat-links-to-add (push categorial-link (fourth known-holistic-cxn-subunit-blocks)))
                 
             
                   (cxn-name-item-based-cxn-apply-last
                    (concatenate 'string (symbol-name (add-cxn-suffix cxn-name-item-based-cxn)) "-APPLY-LAST"))
                   (cxn-name-item-based-cxn-apply-first
                    (concatenate 'string (symbol-name (add-cxn-suffix cxn-name-item-based-cxn)) "-APPLY-FIRST"))

                   (item-based-cxn-apply-last  
                    (or existing-item-based-cxn-apply-last
                        (second (multiple-value-list (eval
                                                      `(def-fcg-cxn ,cxn-name-item-based-cxn-apply-last
                                                                    ((?item-based-unit
                                                                      (syn-cat (phrase-type item-based))
                                                                      (subunits ,holistic-subunit-names))
                                                                     <-
                                                                     (?item-based-unit
                                                                      (HASH meaning ,overlapping-meaning-observation)
                                                                      --
                                                                      (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                                     ,@holistic-cxn-conditional-units)
                                                                    :attributes (:label fcg::routine
                                                                                 :cxn-type item-based
                                                                                 :bare-cxn-name ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                                 :repair holistic+item-based->item-based--substitution
                                                                                 :meaning ,(loop for predicate in overlapping-meaning-observation
                                                                                                 unless (or
                                                                                                         (equal (first predicate) 'get-context)
                                                                                                         (equal (first predicate) 'bind))
                                                                                                 return (first predicate))
                                                                                 :string ,(third (find 'string overlapping-form-with-rewritten-boundaries :key #'first)))
                                                                    :cxn-inventory ,(copy-object cxn-inventory)))))))
                   (item-based-cxn-apply-first
                    (or existing-item-based-cxn-apply-first
                        (second (multiple-value-list (eval
                                                      `(def-fcg-cxn ,cxn-name-item-based-cxn-apply-first
                                                                    ((?item-based-unit
                                                                      (syn-cat (phrase-type item-based))
                                                                      (subunits ,holistic-subunit-names))
                                                                     ,@holistic-cxn-contributing-units
                                                                     <-
                                                                     (?item-based-unit
                                                                      (HASH meaning ,overlapping-meaning-observation)
                                                                      --
                                                                      (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                                     )
                                                                    :attributes (:label fcg::meta-only
                                                                                 :cxn-type item-based
                                                                                 :bare-cxn-name ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                                 :repair holistic+item-based->item-based--substitution
                                                                                 :meaning ,(loop for predicate in overlapping-meaning-observation
                                                                                                 unless (or
                                                                                                         (equal (first predicate) 'get-context)
                                                                                                         (equal (first predicate) 'bind))
                                                                                                 return (first predicate))
                                                                                 :string ,(third (find 'string overlapping-form-with-rewritten-boundaries :key #'first)))
                                                                           
                                                                    :cxn-inventory ,(copy-object cxn-inventory)))))))

             
             ;(cxns-to-consolidate (list item-based-cxn-apply-first new-holistic-cxn-2))
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
                                   item-based-cxn-apply-first
                                   item-based-cxn-apply-last))
                   (cxns-to-apply (append (list item-based-cxn-apply-first) alter-applied-holistic-cxns))
              
                   (cxns-to-consolidate (loop for cxn in new-cxns
                                              unless (or (member cxn existing-cxns)
                                                         (member cxn cxns-to-apply))
                                              collect cxn)))
              (when existing-item-based-cxn-apply-first ; we ordered the units, so they'll always be in the order in which they appear in the utterance
                (loop for item-lc in (get-all-unit-lex-classes existing-item-based-cxn-apply-first)
                      for cxn in applied-holistic-cxns
                      for holistic-lc = (lex-class-cxn cxn)
                      collect (cons holistic-lc item-lc) into new-cat-links
                      finally (setf cat-links-to-add new-cat-links))
                )
              ;; add cat link to the alternative holistic cxn
              (push (loop for link in cat-links-to-add
                          for holistic-cxn-lc = (car link)
                          for slot-lc = (cdr link)
                          when (equal lex-class-holistic-cxn-2 holistic-cxn-lc)
                          return (cons lex-class-holistic-cxn-1 slot-lc))
                    cat-links-to-add)
                    
                                  
              (list
               cxns-to-apply
               cat-links-to-add
               cxns-to-consolidate
               ))))))))
               





  
  

