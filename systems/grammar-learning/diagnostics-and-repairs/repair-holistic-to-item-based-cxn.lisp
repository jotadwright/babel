(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair from holistic to item-based cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass repair-holistic->item-based-cxn (repair) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair repair-holistic->item-based-cxn)
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

(defmethod repair ((repair repair-holistic->item-based-cxn)
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

(defun create-item-based-lex-class-with-var (placeholder-var-string-predicates cxn-name-item-based-cxn slot-var)
  "create the what-is-the-size-of-the-?Y-?X-(X)-12 lex class for a specific slot var"
  (let ((placeholder (third (find slot-var placeholder-var-string-predicates :key #'second))))
    (make-lex-class (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-(" placeholder ")"))))
  

(defun create-item-based-cxn-from-partial-holistic-analysis (problem node)
  "Creates item-based construction around matching holistic constructions"
  (let* ((cxn-inventory (construction-inventory node))
         (original-cxn-set (original-cxn-set cxn-inventory))
         (utterance (random-elt (get-data problem :utterances)))
         (matching-holistic-cxns (find-all-matching-cxn-cars-for-node cxn-inventory node)))
    (when matching-holistic-cxns
      (let* (
             (optimal-coverage-cars (find-optimal-coverage-cars matching-holistic-cxns node))
         ;(ordered-holistic-cxns (sort-cxns-by-form-string optimal-coverage-cxns utterance)) ;  do we care about the order at all? no! but easier to read the cxn if units are ordered
             (car-res-cfs (car-resulting-cfs (first optimal-coverage-cars)))
             (resulting-left-pole-structure (left-pole-structure car-res-cfs))
             (resulting-root (get-root resulting-left-pole-structure))
             (resulting-units (remove resulting-root resulting-left-pole-structure))
             (item-based-cxn-form-constraints (unit-feature-value resulting-root 'form))
             (chunk-item-based-cxn-form-constraints (loop with item-based-fc = item-based-cxn-form-constraints
                                                     for unit in resulting-units
                                                     for fc = (unit-feature-value unit 'form)
                                                     do (setf item-based-fc (substitute-slot-meets-constraints fc item-based-fc))
                                                     finally return item-based-fc))
             (item-based-cxn-meaning nil)
             (placeholder-var-string-predicates (variablify-missing-form-strings chunk-item-based-cxn-form-constraints))
             (cxn-name-item-based-cxn (make-cxn-name
                                       (append placeholder-var-string-predicates chunk-item-based-cxn-form-constraints)
                                       original-cxn-set :add-numeric-tail t))
             (categorial-links nil)
             (holistic-cxn-subunit-blocks (multiple-value-list
                                           (loop for unit in resulting-units
                                                 for holistic-cxn-unit-name = (first unit)
                                                 for form-constraints = (unit-feature-value unit 'form)
                                                 for string-var = (first (get-boundary-units form-constraints))
                                                 for args = nil
                                                 for boundaries = (last-elt unit)
                                                 for holistic-slot-lex-class = (create-item-based-lex-class-with-var placeholder-var-string-predicates cxn-name-item-based-cxn string-var) ;; look up the X and Y in bindings
                                                 for categorial-link = nil
                                                 for leftmost-unit-holistic-cxn = nil
                                                 for rightmost-unit-holistic-cxn = nil
                                                 collect holistic-cxn-unit-name into holistic-subunit-names
                                                 collect `(,holistic-cxn-unit-name
                                                           (syn-cat (gl::lex-class ,holistic-slot-lex-class))) into contributing-units
                                                 collect `(,holistic-cxn-unit-name
                                                           (args ,args)
                                                           --
                                                           ,boundaries
                                                            ) into conditional-units
                                                 finally (return (values conditional-units contributing-units holistic-subunit-names)))))
                                                
                                                
              
             (holistic-cxn-conditional-units
              (first holistic-cxn-subunit-blocks))
             (holistic-cxn-contributing-units
              (second holistic-cxn-subunit-blocks))
             (holistic-subunit-names
              (third holistic-cxn-subunit-blocks))
             (item-based-cxn (second (multiple-value-list (eval
                                                           `(def-fcg-cxn ,cxn-name-item-based-cxn
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
                                                                         :cxn-inventory ,(copy-object cxn-inventory)))))))))) nil)


#|
(defun create-item-based-cxn-from-holistic (problem node)
  "Creates item-based construction and holistic constructions
based on existing construction with sufficient overlap."
  (let* ((processing-cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (random-elt (get-data problem :utterances)))
         (var-form
              (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))
                                                                   meaning-representation-formalism))
         (observed-form (extract-forms (left-pole-structure (car-source-cfs (cipn-car (initial-node node))))))
         (matching-holistic-cxns (find-matching-holistic-cxns cxn-inventory var-form gold-standard-meaning utterance)))
         
    ;; we need at least one matching lex cxn
    (when (< 0 (length matching-holistic-cxns))
      (let* ((subunit-names-and-non-overlapping-form
              (multiple-value-list (diff-non-overlapping-form var-form matching-holistic-cxns)))
             (boundaries
              (loop for name in (first subunit-names-and-non-overlapping-form)
                    collect (list name name)))
              ;(list (first (first subunit-names-and-non-overlapping-form)) (first (first subunit-names-and-non-overlapping-form)))) ; remove this!
             (subunit-names
              (loop for name in (first subunit-names-and-non-overlapping-form)
                    collect (unit-ify name))
              )
             
             (non-overlapping-form
              (second subunit-names-and-non-overlapping-form))
             (args-and-non-overlapping-meaning
              (multiple-value-list (diff-non-overlapping-meaning gold-standard-meaning matching-holistic-cxns)))
             (args
              (first args-and-non-overlapping-meaning))
             (non-overlapping-meaning
              (second args-and-non-overlapping-meaning))
             ;(existing-item-based-cxn
             ;     (find-cxn-by-type-form-and-meaning 'item-based
             ;                                        non-overlapping-form
             ;                                        non-overlapping-meaning
             ;                                        cxn-inventory))
             (cxn-name-item-based-cxn
              (make-cxn-name non-overlapping-form cxn-inventory :add-cxn-suffix nil))
             (rendered-cxn-name-list
              (make-cxn-placeholder-name non-overlapping-form cxn-inventory))
             (placeholder-list
              (extract-placeholder-var-list rendered-cxn-name-list))
             (th-links
              (create-type-hierarchy-links matching-holistic-cxns (format nil "~{~a~^-~}" rendered-cxn-name-list) placeholder-list))
             (holistic-cxn-subunit-blocks
              (multiple-value-list (subunit-blocks-for-holistic-cxns subunit-names boundaries args th-links)))
             (holistic-cxn-conditional-units
              (first holistic-cxn-subunit-blocks))
             (holistic-cxn-contributing-units
              (second holistic-cxn-subunit-blocks))
             (item-based-cxn (second (multiple-value-list (eval
                                                           `(def-fcg-cxn ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                         ((?item-based-unit
                                                                           (syn-cat (phrase-type item-based))
                                                                           (subunits ,subunit-names))
                                                                          ,@holistic-cxn-contributing-units
                                                                          <-
                                                                          (?item-based-unit
                                                                           (HASH meaning ,non-overlapping-meaning)
                                                                           --
                                                                           (HASH form ,non-overlapping-form))
                                                                          ,@holistic-cxn-conditional-units)
                                                                         :attributes (:cxn-type item-based
                                                                                      :repair holistic->item-based
                                                                                      :meaning ,(loop for predicate in non-overlapping-meaning
                                                                                         unless (or
                                                                                                 (equal (first predicate) 'get-context)
                                                                                                 (equal (first predicate) 'bind))
                                                                                         return (first predicate))
                                                                         :string ,(third (find 'string non-overlapping-form :key #'first)))
                                                                                      
                                                                         :cxn-inventory ,(copy-object cxn-inventory)))))))
        (add-element (make-html item-based-cxn))
        (add-element (make-html (first matching-holistic-cxns)))
        (list item-based-cxn matching-holistic-cxns th-links)))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair repair-holistic->item-based-cxn) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((item-based-cxn (get-processing-cxn (first (restart-data fix))))
           (holistic-cxns (map 'list #'get-processing-cxn (second (restart-data fix))))
           (th-links (third (restart-data fix)))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (categorial-network (construction-inventory node)))
           (temp-type-hierarchy (copy-object (categorial-network (construction-inventory node))))
           (th-flat-list nil)
           (th (loop for th-link in th-links
                     do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy :recompute-transitive-closure nil)
                     (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5 :recompute-transitive-closure nil)
                     (setf th-flat-list (append th-flat-list (list th-link)))
                     finally (set-categorial-network (construction-inventory node) temp-type-hierarchy)))
           (holistic-nodes (loop with last-node = (initial-node node)
                            for holistic-cxn in holistic-cxns
                            do (setf last-node (fcg::cip-add-child last-node (first (fcg-apply holistic-cxn (if (initial-node-p last-node)
                                                                                                              (car-source-cfs (cipn-car (initial-node last-node)))
                                                                                                              (car-resulting-cfs (cipn-car last-node)))
                                                                                               (direction (cip node))
                                                                                                      :configuration (configuration (construction-inventory node))
                                                                                                      :cxn-inventory (construction-inventory node)))))
                            collect last-node))
           (last-applied-node (last-elt holistic-nodes))
           
           (new-node-item-based (fcg::cip-add-child last-applied-node (first (fcg-apply item-based-cxn (car-resulting-cfs (cipn-car last-applied-node)) (direction (cip node))
                                                                                   :configuration (configuration (construction-inventory node))
                                                                                   :cxn-inventory (construction-inventory node)))))
           
           

           )
      ;; ignore
      (declare (ignore th))
      ;; Reset type hierarchy
      (set-categorial-network (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-cxns (append (second (restart-data fix)) (list (original-cxn item-based-cxn))))
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-categorial-links th-flat-list)
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node-item-based) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node-item-based))
      (push 'added-by-repair (statuses new-node-item-based))
      ;; enqueue only second new node; never backtrack over the first applied holistic construction, we applied them as a block
      (cip-enqueue new-node-item-based (cip node) (get-configuration node :queue-mode)))))


|#
