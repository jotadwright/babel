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
    (let ((constructions-and-th-links (create-item-based-cxn-from-partial-holistic-analysis problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defmethod repair ((repair holistic->item-based)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn-from-partial-holistic-analysis problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defun create-item-based-cxn-from-partial-holistic-analysis (problem node)
  "Creates item-based construction around matching holistic constructions"
  (let* ((cxn-inventory (construction-inventory node))
         (original-cxn-set (original-cxn-set cxn-inventory))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings)) meaning-representation-formalism))
         (matching-holistic-cxns (find-all-matching-cxn-cars-for-node cxn-inventory node)))
    (when matching-holistic-cxns
      (let* (
             (optimal-coverage-cars (find-optimal-coverage-cars matching-holistic-cxns node))
             (last-car (last-elt optimal-coverage-cars))
             (car-res-cfs (car-resulting-cfs last-car))
             (resulting-left-pole-structure (left-pole-structure car-res-cfs))
             (resulting-root (get-root resulting-left-pole-structure))
             (resulting-units (sort-units-by-form-string (remove resulting-root resulting-left-pole-structure) utterance original-cxn-set))
             (item-based-cxn-form-constraints (variablify-form-constraints-with-constants (unit-feature-value resulting-root 'form)))
             (chunk-item-based-cxn-form-constraints (make-item-based-name-form-constraints-from-units item-based-cxn-form-constraints resulting-units))
             (placeholder-var-string-predicates (variablify-missing-form-strings chunk-item-based-cxn-form-constraints))
             (cxn-name-item-based-cxn (make-cxn-name
                                       (append placeholder-var-string-predicates chunk-item-based-cxn-form-constraints)
                                       original-cxn-set :add-numeric-tail t :add-cxn-suffix nil))
             
             (holistic-cxn-subunit-blocks (multiple-value-list
                                           (loop for unit in resulting-units
                                                 for form-constraints = (variablify-form-constraints-with-constants (unit-feature-value unit 'form))
                                                 for holistic-cxn-unit-name = (unit-ify (make-cxn-name form-constraints original-cxn-set :add-cxn-suffix nil))
                                                 for string-var = (first (get-boundary-units form-constraints))
                                                 for car = (get-car-for-unit unit optimal-coverage-cars)
                                                 for subtracted-meaning = (get-subtracted-meaning-from-car car gold-standard-meaning)
                                                 for args = (extract-args-from-irl-network subtracted-meaning)
                                                 for boundaries = (unit-feature-value unit 'boundaries)
                                                 for boundary-list = (list (variablify (second (first boundaries))) (variablify (second (second boundaries))))
                                                 for holistic-slot-lex-class = (create-item-based-lex-class-with-var placeholder-var-string-predicates cxn-name-item-based-cxn string-var) ;; look up the X and Y in bindings
                                                 for placeholder-var = (third (find string-var placeholder-var-string-predicates :key #'second))
                                                 for updated-form-constraints-and-boundaries = (multiple-value-list (add-boundaries-to-form-constraints item-based-cxn-form-constraints boundary-list :placeholder-var placeholder-var))
                                                 for updated-form-constraints = (first updated-form-constraints-and-boundaries)
                                                 for updated-boundaries = (second updated-form-constraints-and-boundaries)
                                                 for holistic-cxn-lex-class = (unit-feature-value (unit-feature-value unit 'syn-cat) 'lex-class)
                                                 for categorial-link = (cons holistic-cxn-lex-class holistic-slot-lex-class)
                                                 do (setf item-based-cxn-form-constraints updated-form-constraints)
                                                 collect subtracted-meaning into subtracted-meanings
                                                 collect categorial-link into categorial-links
                                                 collect holistic-cxn-unit-name into holistic-subunit-names
                                                 collect `(,holistic-cxn-unit-name
                                                           (syn-cat (gl::lex-class ,holistic-slot-lex-class))) into contributing-units
                                                 collect `(,holistic-cxn-unit-name
                                                           (args ,args)
                                                           --
                                                           (boundaries
                                                            (left ,(first updated-boundaries))
                                                            (right ,(second updated-boundaries)))
                                                           ) into conditional-units
                                                 finally (return (values conditional-units contributing-units holistic-subunit-names categorial-links subtracted-meanings)))))
             (holistic-cxn-conditional-units
              (first holistic-cxn-subunit-blocks))
             (holistic-cxn-contributing-units
              (second holistic-cxn-subunit-blocks))
             (holistic-subunit-names
              (third holistic-cxn-subunit-blocks))
             (cat-links-to-add (fourth holistic-cxn-subunit-blocks))
             (subtracted-meanings (fifth holistic-cxn-subunit-blocks))
             (item-based-cxn-meaning (subtract-holistic-from-item-based-meaning gold-standard-meaning subtracted-meanings))
             (existing-item-based-cxn (find-cxn-by-form-and-meaning
                                         item-based-cxn-form-constraints
                                         item-based-cxn-meaning
                                         original-cxn-set
                                         :cxn-type 'item-based))
             (item-based-cxn (or existing-item-based-cxn
                                 (second (multiple-value-list (eval
                                                           `(def-fcg-cxn ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                         ((?item-based-unit
                                                                           (syn-cat (phrase-type item-based))
                                                                           (subunits ,holistic-subunit-names))
                                                                          ,@holistic-cxn-contributing-units
                                                                          <-
                                                                          (?item-based-unit
                                                                           (HASH meaning ,item-based-cxn-meaning)
                                                                           --
                                                                           (HASH form ,item-based-cxn-form-constraints))
                                                                          ,@holistic-cxn-conditional-units)
                                                                         :attributes (:cxn-type item-based
                                                                                      :repair holistic->item-based
                                                                                      :meaning ,(loop for predicate in item-based-cxn-meaning
                                                                                                      unless (or
                                                                                                              (equal (first predicate) 'get-context)
                                                                                                              (equal (first predicate) 'bind))
                                                                                                      return (first predicate))
                                                                                      :string ,(third (find 'string item-based-cxn-form-constraints :key #'first)))              
                                                                         :cxn-inventory ,(copy-object original-cxn-set)))))))
             (cxns-to-apply (append (mapcar #'original-cxn (mapcar #'car-applied-cxn optimal-coverage-cars)) (list item-based-cxn)))
             (cxns-to-consolidate (unless existing-item-based-cxn
                                    (list item-based-cxn))))
        (when existing-item-based-cxn ; we ordered the units, so they'll always be in the order in which they appear in the utterance
          (loop for item-lc in (get-all-unit-lex-classes existing-item-based-cxn)
                for cat-link in cat-links-to-add
                for holistic-lc = (first cat-link)
                collect (cons holistic-lc item-lc) into new-cat-links
                finally do (setf cat-links-to-add new-cat-links))
          ;(add-element (make-html (categorial-network original-cxn-set)))
          )
        ;(add-element (make-html item-based-cxn))
        
        (list
         cxns-to-apply
         cat-links-to-add
         cxns-to-consolidate)
        ))))

