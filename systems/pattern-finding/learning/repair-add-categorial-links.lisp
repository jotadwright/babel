(in-package :pattern-finding)


(defclass add-categorial-links (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair add-categorial-links)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by adding new th links for existing nodes that were not previously connected."
  (let ((cxns-and-categorial-links (create-categorial-links problem node)))
    (when cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))


(defun create-categorial-links (problem node)
  (do-repair
   (get-data problem :utterance)
   (get-data problem :meaning)
   nil
   nil
   (construction-inventory node)
   node
   'add-categorial-links))

;;;; QUESTION
;;;; Jonas filtered the cipns for 'compatible-args', i.e.
;;;; do the args in the top-most unit of the transient structure
;;;; correspond to the unconnected variables in the observed meaning.
;;;; Why is this?
;;;; Is this relevant for applying this repair in the recursion?
;;;; To make sure that the result of this repair clicks together with
;;;; the repair higher up in the recursion?


(defmethod do-repair (observation-form observation-meaning form-args meaning-args
                                       (cxn-inventory construction-inventory)
                                       node (repair-type (eql 'add-categorial-links)))
  "Return the categorial links and applied cxns from a comprehend
   with :category-linking-mode :categories-exist instead of :neighbours"
  (declare (ignore form-args meaning-args))
  (disable-meta-layer-configuration cxn-inventory) ;; category linking mode == :categories-exist
  (with-disabled-monitor-notifications
    (multiple-value-bind (meanings cipns)
        (comprehend-all observation-form
                        :cxn-inventory (original-cxn-set cxn-inventory)
                        :gold-standard-meaning observation-meaning)
      (declare (ignore parsed-meanings))
      (enable-meta-layer-configuration cxn-inventory)
      (let* ((solution-nodes
              (remove-if-not
               #'(lambda (cipn)
                   (find 'fcg::succeeded (statuses cipn)))
               cipns))
             (best-solution-node
              (the-biggest
               #'(lambda (cipn)
                   (average (mapcar #'get-cxn-score (original-applied-constructions cipn))))
               solution-nodes)))
        (when best-solution-node
          (let* ((cxns-to-apply (reverse (original-applied-constructions best-solution-node)))
                 (top-lvl-category (extract-lex-class-item-based-cxn (last-elt cxns-to-apply))))
            (when (> (length cxns-to-apply) 1) ;; avoid holophrase cxns
              (apply-fix observation-form
                         cxns-to-apply
                         (extract-used-categorial-links best-solution-node)
                         nil
                         nil
                         top-lvl-category
                         (gold-standard-consulted-p best-solution-node)
                         node
                         repair-type))))))))


(defun gold-standard-consulted-p (cipn)
  "was the gold standard consulted in this cip node?"
  (not (get-data (goal-test-data cipn) :result-goal-test-non-gold-standard-meaning)))


(defun extract-used-categorial-links (solution-cipn)
  "For a given solution-cipn, extracts categorial links that were used (based on lex-class)."
  (loop for cipn in (ignore-initial-nodes (reverse (cons solution-cipn (all-parents solution-cipn))))
        append
          (let* ((processing-cxn
                  (car-applied-cxn (cipn-car cipn)))
                 (processing-cxn
                  (if (equal (attr-val processing-cxn :label) 'fcg::routine)
                    processing-cxn
                    (alter-ego-cxn processing-cxn (construction-inventory cipn))))                             
                 (units-matching-lex-class
                  (loop for unit in (right-pole-structure processing-cxn)
                        for syn-cat = (rest (unit-feature-value unit 'syn-cat))
                        for lex-class = (second (find 'lex-class syn-cat :key #'first))
                        when lex-class
                        collect (cons (first unit) lex-class))))
            (loop for (cxn-unit-name . cxn-lex-class) in units-matching-lex-class
                  for ts-unit-name = (cdr (find cxn-unit-name (car-second-merge-bindings (cipn-car cipn)) :key #'first))
                  for ts-unit = (find ts-unit-name (left-pole-structure (car-source-cfs (cipn-car cipn))):key #'first)
                  for ts-lex-class = (second (find 'lex-class (second (find 'syn-cat (rest ts-unit) :key #'first)) :key #'first))
                  when (and cxn-lex-class ts-lex-class)
                  collect (cons ts-lex-class cxn-lex-class)))))
            
