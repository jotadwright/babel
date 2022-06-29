(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair from holistic to item-based cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass holistic->item-based (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holistic->item-based)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (create-item-based-cxn-from-partial-holistic-analysis problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defun create-item-based-cxn-from-partial-holistic-analysis (problem node)
  (do-create-item-based-cxn-from-partial-holistic-analysis
   (form-constraints-with-variables
    (random-elt (get-data problem :utterances))
    (get-configuration (construction-inventory node) :de-render-mode))
   (meaning-predicates-with-variables
    (random-elt (get-data problem :meanings))
    (get-configuration (construction-inventory node) :meaning-representation-formalism))
   (construction-inventory node)))

(defun do-create-item-based-cxn-from-partial-holistic-analysis (form-constraints meaning cxn-inventory)
  "Creates item-based construction around matching holistic constructions"
  (let* ((original-cxn-set (original-cxn-set cxn-inventory))
         
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         
         (best-partial-analysis-node (get-best-partial-analysis-cipn
                                      form-constraints
                                      meaning
                                      original-cxn-set
                                      :optimal-form-coverage))
         (applied-cxns (when best-partial-analysis-node
                         (applied-constructions best-partial-analysis-node))))
    (when (and (filter-by-phrase-type 'holistic applied-cxns)
               (get-root-form-predicates best-partial-analysis-node))
      (let* ((item-based-cxn-form-constraints (variablify-form-constraints-with-constants (get-root-form-predicates best-partial-analysis-node)))
             (resulting-left-pole-structure (left-pole-structure (car-resulting-cfs (cipn-car best-partial-analysis-node))))
             (resulting-root (get-root resulting-left-pole-structure))
             (resulting-units (sort-unvariablified-units-by-meets-constraints (remove resulting-root resulting-left-pole-structure) item-based-cxn-form-constraints))
             (chunk-item-based-cxn-form-constraints (make-item-based-name-form-constraints-from-units item-based-cxn-form-constraints resulting-units))
             (placeholder-var-string-predicates (variablify-missing-form-strings chunk-item-based-cxn-form-constraints))
             (cxn-name-item-based-cxn (make-cxn-name
                                       (append placeholder-var-string-predicates chunk-item-based-cxn-form-constraints)
                                       original-cxn-set :add-numeric-tail t :add-cxn-suffix nil))
             
             (holistic-cxn-subunit-blocks (multiple-value-list
                                           (loop for unit in resulting-units
                                                 for form-constraints = (variablify-form-constraints-with-constants (unit-feature-value unit 'form))
                                                 for boundaries = (unit-feature-value unit 'boundaries)
                                                 
                                                 for string-var = (first (get-boundary-units form-constraints))
                                                 for subtracted-meaning-list = (multiple-value-list (commutative-irl-subset-diff meaning (unit-feature-value unit 'meaning)))
                                                 for parent-meaning = (first subtracted-meaning-list)
                                                 for subtracted-meaning = (second subtracted-meaning-list)
                                                 for args = (extract-args-from-meaning-networks subtracted-meaning parent-meaning meaning-representation-formalism)
                                                 for boundary-list = (list (variablify (second (first boundaries))) (variablify (second (second boundaries))))
                                                 for holistic-slot-lex-class = (create-item-based-lex-class-with-var placeholder-var-string-predicates cxn-name-item-based-cxn string-var) ;; look up the X and Y in bindings
                                                 for placeholder-var = (third (find string-var placeholder-var-string-predicates :key #'second))
                                                 for updated-form-constraints-and-boundaries = (multiple-value-list (add-boundaries-to-form-constraints item-based-cxn-form-constraints boundary-list :placeholder-var placeholder-var))
                                                 for updated-form-constraints = (first updated-form-constraints-and-boundaries)
                                                 for updated-boundaries = (second updated-form-constraints-and-boundaries)
                                                 for holistic-cxn-unit-name = (first boundary-list)
                                                 ;for holistic-cxn-lex-class = (unit-feature-value (unit-feature-value unit 'syn-cat) 'lex-class)
                                                 do (setf item-based-cxn-form-constraints updated-form-constraints)
                                                 
                                                 collect subtracted-meaning into subtracted-meanings
                                                 unless (member 'gl::used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
                                                 collect args into slot-args-list
                                                 unless (member 'gl::used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
                                                 collect holistic-cxn-unit-name into holistic-subunit-names
                                                 unless (member 'gl::used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
                                                 collect `(,holistic-cxn-unit-name 
                                                           (footprints (used-as-slot-filler))) into contributing-footprints
                                                 unless (member 'gl::used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
                                                 collect `(,holistic-cxn-unit-name
                                                           (footprints (used-as-slot-filler))
                                                           (syn-cat (lex-class ,holistic-slot-lex-class))
                                                           (args ,args)
                                                           (boundaries
                                                            (left ,(first updated-boundaries))
                                                            (right ,(second updated-boundaries)))) into contributing-units-apply-first
                                                 unless (member 'gl::used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
                                                 collect `(,holistic-cxn-unit-name
                                                           (footprints (NOT used-as-slot-filler))
                                                           (args ,args)
                                                           --
                                                           (footprints (NOT used-as-slot-filler))
                                                           (syn-cat (lex-class ,holistic-slot-lex-class))
                                                           (boundaries
                                                            (left ,(first updated-boundaries))
                                                            (right ,(second updated-boundaries)))) into conditional-units-apply-last
                                                 unless (member 'gl::used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
                                                 collect (list 'fcg::meets (first updated-boundaries) (second updated-boundaries)) into dummy-slot-fcs
                                                 finally (return (values conditional-units-apply-last
                                                                         contributing-units-apply-first
                                                                         holistic-subunit-names
                                                                         subtracted-meanings
                                                                         slot-args-list
                                                                         contributing-footprints
                                                                         dummy-slot-fcs)))))
             (holistic-cxn-conditional-units
              (first holistic-cxn-subunit-blocks))
             (holistic-cxn-contributing-units
              (second holistic-cxn-subunit-blocks))
             (holistic-subunit-names
              (third holistic-cxn-subunit-blocks))
             (subtracted-meanings (fourth holistic-cxn-subunit-blocks))
             (item-based-args (extract-args-from-meaning-networks meaning nil meaning-representation-formalism))
             (slot-args-list (fifth holistic-cxn-subunit-blocks))
             )
        (when (and slot-args-list
                   (loop for args in slot-args-list
                         always args))
          (let* ((contributing-footprints (sixth holistic-cxn-subunit-blocks))
                 (dummy-slot-fcs (seventh holistic-cxn-subunit-blocks))
                 (item-based-cxn-meaning (subtract-holistic-from-item-based-meaning meaning subtracted-meanings))
                 (existing-item-based-cxn-apply-first (find-cxn-by-form-and-meaning
                                                       item-based-cxn-form-constraints
                                                       item-based-cxn-meaning
                                                       slot-args-list
                                                       original-cxn-set
                                                       :cxn-type 'item-based
                                                       :cxn-set 'fcg::meta-only))
                 (existing-item-based-cxn-apply-last (when existing-item-based-cxn-apply-first
                                                       (alter-ego-cxn existing-item-based-cxn-apply-first original-cxn-set)))

                 (lex-class-item-based-cxn
                  (if existing-item-based-cxn-apply-first
                    (extract-contributing-lex-class existing-item-based-cxn-apply-first)
                    (make-lex-class (symbol-name cxn-name-item-based-cxn) :trim-cxn-suffix t)))
             
                 (cxn-name-item-based-cxn-apply-last
                  (intern (concatenate 'string (symbol-name (add-cxn-suffix cxn-name-item-based-cxn)) "-APPLY-LAST")))
                 (cxn-name-item-based-cxn-apply-first
                  (intern (concatenate 'string (symbol-name (add-cxn-suffix cxn-name-item-based-cxn)) "-APPLY-FIRST")))

                 ;; boundaries
                 (rewritten-item-based-boundaries (get-boundary-units (append dummy-slot-fcs item-based-cxn-form-constraints)))
                 (cxn-inventory-copy (copy-object original-cxn-set))
                 (item-based-cxn-apply-last
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
                                                                    (subunits ,holistic-subunit-names))
                                                                   ,@contributing-footprints
                                                                   <-
                                                                   (?item-based-unit
                                                                    (HASH meaning ,item-based-cxn-meaning)
                                                                    --
                                                                    (HASH form ,item-based-cxn-form-constraints))
                                                                   ,@holistic-cxn-conditional-units)
                                                                  :attributes (:label fcg::routine
                                                                               :cxn-type item-based
                                                                               :bare-cxn-name ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                               :repair holistic->item-based
                                                                               :meaning ,(loop for predicate in item-based-cxn-meaning
                                                                                               unless (or
                                                                                                       (equal (first predicate) 'get-context)
                                                                                                       (equal (first predicate) 'bind))
                                                                                               return (first predicate))
                                                                               :string ,(third (find 'string item-based-cxn-form-constraints :key #'first)))
                                                                           
                                                                  :cxn-inventory ,cxn-inventory-copy))))))
                 (item-based-cxn-apply-first
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
                                                                    (subunits ,holistic-subunit-names))
                                                                   ,@holistic-cxn-contributing-units
                                                                   <-
                                                                   (?item-based-unit
                                                                    (HASH meaning ,item-based-cxn-meaning)
                                                                    --
                                                                    (HASH form ,item-based-cxn-form-constraints))
                                                                   )
                                                                  :attributes (:label fcg::meta-only
                                                                               :cxn-type item-based
                                                                               :bare-cxn-name ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                               :repair holistic->item-based
                                                                               :meaning ,(loop for predicate in item-based-cxn-meaning
                                                                                               unless (or
                                                                                                       (equal (first predicate) 'get-context)
                                                                                                       (equal (first predicate) 'bind))
                                                                                               return (first predicate))
                                                                               :string ,(third (find 'string item-based-cxn-form-constraints :key #'first)))
                                                                           
                                                                  :cxn-inventory ,cxn-inventory-copy))))))
                 (temp-cxn-inventory (create-temp-cxn-inventory original-cxn-set))
                 (temp-cxns-to-add (append (mapcar #'original-cxn applied-cxns)
                                           (list item-based-cxn-apply-last)))
                 (temp-cats-to-add (append (mapcar #'extract-contributing-lex-class temp-cxns-to-add)
                                           (mappend #'get-all-conditional-unit-lex-classes temp-cxns-to-add))))

            ;; add categories and temp cxns
            (add-categories temp-cats-to-add (categorial-network temp-cxn-inventory) :recompute-transitive-closure nil)
            (dolist (cxn temp-cxns-to-add)
              (add-cxn cxn temp-cxn-inventory))
            (let* ((solution-cipn (second (multiple-value-list (comprehend form-constraints :gold-standard-meaning meaning :cxn-inventory temp-cxn-inventory :silent t))))
                   
                   ;; build result
                   (cxns-to-apply (reverse (mapcar #'original-cxn (applied-constructions solution-cipn))))
                   (cat-links-to-add (extract-used-categorial-links solution-cipn))
                   (cxns-to-consolidate (list item-based-cxn-apply-first))     
                   (cats-to-add (list lex-class-item-based-cxn)))
        
              (list
               cxns-to-apply
               cat-links-to-add
               cxns-to-consolidate
               cats-to-add
               lex-class-item-based-cxn
               ))))))))
            
#|
                 (cxns-to-apply (append applied-cxns (list item-based-cxn-apply-last)))
                 (cxns-to-consolidate (unless existing-item-based-cxn-apply-first
                                        (list item-based-cxn-apply-first)))
                 (cats-to-add (list lex-class-item-based-cxn)))
            (when existing-item-based-cxn-apply-first ; we ordered the units, so they'll always be in the order in which they appear in the utterance
              (loop for item-lc in (get-all-unit-lex-classes existing-item-based-cxn-apply-first)
                    for cat-link in cat-links-to-add
                    for holistic-lc = (first cat-link)
                    collect (cons holistic-lc item-lc) into new-cat-links
                    finally (setf cat-links-to-add new-cat-links))
          ;(add-element (make-html (categorial-network original-cxn-set)))
              )
        ;(add-element (make-html item-based-cxn-apply-last))
        
            (list
             cxns-to-apply
             cat-links-to-add
             cxns-to-consolidate
             cats-to-add)
            ))))))|#