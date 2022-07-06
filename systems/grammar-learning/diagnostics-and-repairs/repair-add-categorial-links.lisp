(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add Categorial links     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass add-categorial-links (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair add-categorial-links)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by adding new th links for existing nodes that were not previously connected."
  (let ((cxns-and-categorial-links (create-categorial-links problem node)))
    (if cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links)
      nil)))
#|
(defmethod repair ((repair add-categorial-links)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by adding new th links for existing nodes that were not previously connected."
  (let ((cxns-and-categorial-links (create-categorial-links problem node)))
    (when cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))
|#
(defun filter-by-phrase-type (type cxns)
  "returns all cxns in the list for the given type"
  (loop for cxn in cxns
        for orig-cxn = (get-original-cxn cxn)
        for phrase-type = (attr-val cxn :cxn-type)
        when (equal phrase-type type)
        collect orig-cxn))

(defun create-new-categorial-links (lex-classes-holistic-cxns lex-classes-item-based-units categorial-network)
  "Creates all categorial links for matching holistic cxns using their original lex-class."
  (loop for holistic-cxn-lex-class in lex-classes-holistic-cxns
        for item-slot-lex-class in lex-classes-item-based-units
        unless (neighbouring-categories-p holistic-cxn-lex-class item-slot-lex-class categorial-network)
        collect (cons holistic-cxn-lex-class item-slot-lex-class)))

(defun create-categorial-links (problem node)
  (do-create-categorial-links
   (form-constraints-with-variables
    (random-elt (get-data problem :utterances))
    (get-configuration (construction-inventory node) :de-render-mode))
   (meaning-predicates-with-variables
    (random-elt (get-data problem :meanings))
    (get-configuration (construction-inventory node) :meaning-representation-formalism))
   (construction-inventory node)))

(defun do-create-categorial-links (form-constraints meaning cxn-inventory)
  "Return the categorial links and applied cxns from a comprehend with :category-linking-mode :path-exists instead of :neighbours"
    (disable-meta-layer-configuration cxn-inventory) 
    (with-disabled-monitor-notifications
      (multiple-value-bind (parsed-meaning cip-node)
          (comprehend form-constraints :cxn-inventory (original-cxn-set cxn-inventory) :gold-standard-meaning meaning)
        (enable-meta-layer-configuration cxn-inventory)
        (when (and
               (member 'succeeded (statuses cip-node) :test #'string=)
               (equivalent-meaning-networks parsed-meaning meaning (get-configuration cxn-inventory :meaning-representation-formalism)))
               
          (let* ((cxns-to-apply (mapcar #'original-cxn (reverse (applied-constructions cip-node))))
                 (top-level-category (extract-contributing-lex-class (last-elt cxns-to-apply))))
            (list
             cxns-to-apply
             (extract-used-categorial-links cip-node)
             nil
             nil
             top-level-category))))))


(defun extract-used-categorial-links (solution-cipn)
  "For a given solution-cipn, extracts categorial links that were used (based on lex-class)."
  (loop for cipn in (rest (reverse (cons solution-cipn (all-parents solution-cipn))))
          append (let* (
                        (processing-cxn (car-applied-cxn (cipn-car cipn)))
                        (processing-cxn (if (equal (attr-val processing-cxn :label) 'fcg::routine)
                                          processing-cxn
                                          (first (remove (name processing-cxn)
                                                  (find-all (attr-val processing-cxn :bare-cxn-name) (constructions-list (construction-inventory cipn)) :key #'(lambda(cxn) (attr-val cxn :bare-cxn-name))) :key #'name))))
                                                  
                        (units-matching-lex-class (loop for unit in (right-pole-structure processing-cxn)
                                                        for syn-cat = (rest (unit-feature-value unit 'syn-cat))
                                                        for lex-class = (second (find 'lex-class syn-cat :key #'first))
                                                        when lex-class
                                                          collect (cons (first unit) lex-class))))
                   (loop for (cxn-unit-name . cxn-lex-class) in units-matching-lex-class
                         for ts-unit-name = (cdr (find cxn-unit-name (car-second-merge-bindings (cipn-car cipn)) :key #'first))
                         for ts-unit = (find ts-unit-name (left-pole-structure (car-source-cfs (cipn-car cipn))):key #'first)
                         for ts-lex-class = (second (find 'lex-class (second (find 'syn-cat (rest ts-unit) :key #'first)) :key #'first))
                         if (and cxn-lex-class ts-lex-class)
                         collect (cons ts-lex-class cxn-lex-class)))))
                         ;else do (error "cxn-lex-class or ts-lex-class was nil!!")))))

;; (extract-used-categorial-links *saved-cipn*)
            
