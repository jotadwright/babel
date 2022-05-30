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
    (let ((cxns-and-categorial-links (create-categorial-links problem node)))
      (if cxns-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data cxns-and-categorial-links)
        (progn (set-data (blackboard (construction-inventory node)) :add-categorial-links-repair-failed t)
          nil)))))

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
  "Return the categorial links and applied cxns from a comprehend with :category-linking-mode :path-exists instead of :neighbours"
  (let* ((utterance (random-elt (get-data problem :utterances)))
         (gold-standard-meaning (random-elt (get-data problem :meanings)))
         (cxn-inventory (construction-inventory node))
         (orig-cxn-set (original-cxn-set cxn-inventory))
         (categorial-network (categorial-network (construction-inventory node))))
    (disable-meta-layer-configuration cxn-inventory) ;(fcg::unify-atom
    (with-disabled-monitor-notifications
      (let* ((comprehension-result (multiple-value-list (comprehend utterance :cxn-inventory orig-cxn-set :gold-standard-meaning gold-standard-meaning)))
             (cip-node (second comprehension-result)))  
        (enable-meta-layer-configuration cxn-inventory)
        ;;there is a solution with connected links in the categorial-network
        (when (member 'succeeded (statuses cip-node) :test #'string=)
          (let* ((applied-cxns (applied-constructions cip-node))
                 (holistic-cxns (sort-cxns-by-form-string (filter-by-phrase-type 'holistic applied-cxns) utterance cxn-inventory)) ; why sort? reuse the same lookup function from the holistic->item-based repair
                 (lex-classes-holistic-cxns (when holistic-cxns
                                              (map 'list #'lex-class-cxn holistic-cxns)))
                 (item-based-cxn (first (filter-by-phrase-type 'item-based applied-cxns)))
                 (lex-classes-item-based-units (when item-based-cxn
                                                 (get-conditional-unit-lex-classes item-based-cxn)))
                 (categorial-links (when (and
                                          lex-classes-holistic-cxns
                                          lex-classes-item-based-units
                                          (= (length lex-classes-holistic-cxns) (length lex-classes-item-based-units)))
                                     (create-new-categorial-links lex-classes-holistic-cxns lex-classes-item-based-units categorial-network)))
                 (cxns-to-apply (append holistic-cxns (list item-based-cxn))))
            
            (list
             cxns-to-apply
             categorial-links
             nil)))))))

