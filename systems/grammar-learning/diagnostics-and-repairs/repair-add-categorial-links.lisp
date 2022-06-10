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
  (unless (find-data (blackboard (construction-inventory node)) :add-categorial-links-repair-failed)
    ;(break)
    (let ((cxns-and-categorial-links (create-categorial-links problem node)))
      (if cxns-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data cxns-and-categorial-links)
        (progn (set-data (blackboard (construction-inventory node)) :add-categorial-links-repair-failed t)
          nil)))))
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
   (random-elt (get-data problem :utterances))
   (random-elt (get-data problem :meanings))
   (construction-inventory node)))

(defun do-create-categorial-links (utterance gold-standard-meaning cxn-inventory)
  "Return the categorial links and applied cxns from a comprehend with :category-linking-mode :path-exists instead of :neighbours"
    (disable-meta-layer-configuration cxn-inventory) 
    (with-disabled-monitor-notifications
      (multiple-value-bind (meaning cip-node)
          (comprehend utterance :cxn-inventory (original-cxn-set cxn-inventory) :gold-standard-meaning gold-standard-meaning)
        (declare (ignore meaning))
        (enable-meta-layer-configuration cxn-inventory)
        (when (member 'succeeded (statuses cip-node) :test #'string=)
            (list
             (reverse (applied-constructions cip-node))
             (extract-used-categorial-links cip-node)
             nil
             nil)))))

(defun extract-used-categorial-links (solution-cipn)
  "For a given solution-cipn, extracts categorial links that were used (based on lex-class)."
  (loop for cipn in (rest (reverse (cons solution-cipn (all-parents solution-cipn))))
          append (let* ((processing-cxn (car-applied-cxn (cipn-car cipn)))
                        (original-cxn (original-cxn processing-cxn))
                        (units-matching-lex-class (loop for unit in (conditional-part original-cxn)
                                                        for features = (append (comprehension-lock unit) (formulation-lock unit))
                                                        for lex-class = (second (find 'lex-class (rest (find 'syn-cat features :key #'first)) :key #'first))
                                                        when lex-class
                                                          collect (cons (cdr (find (name unit) (renamings processing-cxn) :key #'first)) lex-class))))
                   (loop for (cxn-unit-name . cxn-lex-class) in units-matching-lex-class
                         for ts-unit-name = (cdr (find cxn-unit-name (car-second-merge-bindings (cipn-car cipn)) :key #'first))
                         for ts-unit = (find ts-unit-name (left-pole-structure (car-source-cfs (cipn-car cipn))):key #'first)
                         for ts-lex-class = (second (find 'lex-class (second (find 'syn-cat (rest ts-unit) :key #'first)) :key #'first))
                         collect (cons cxn-lex-class ts-lex-class)))))
            
