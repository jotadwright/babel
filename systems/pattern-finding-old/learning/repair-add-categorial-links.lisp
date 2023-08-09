(in-package :pattern-finding-old)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add Categorial links ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass add-categorial-links (add-cxns-and-categorial-links) 
  ((trigger :initform 'fcg::new-node)))


(defmethod repair ((repair add-categorial-links)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by adding new th links for existing nodes that were not previously connected."
  (let ((cxns-and-categorial-links
         (do-repair
          (get-data problem :utterance)
          (get-data problem :meaning)
          (make-blackboard)
          (construction-inventory node)
          node
          'add-categorial-links)))
    (when cxns-and-categorial-links
      (make-instance 'fcg::cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data cxns-and-categorial-links))))

;;;; QUESTION
;;;; Jonas filtered the cipns for 'compatible-args', i.e.
;;;; do the args in the top-most unit of the transient structure
;;;; correspond to the unconnected variables in the observed meaning.
;;;; Why is this?
;;;; Is this relevant for applying this repair in the recursion?
;;;; To make sure that the result of this repair clicks together with
;;;; the repair higher up in the recursion?

(defmethod do-repair (observation-form observation-meaning (args blackboard) (cxn-inventory construction-inventory) node (repair-type (eql 'add-categorial-links)))
  "Return the categorial links and applied cxns from a comprehend
   with :category-linking-mode :categories-exist instead of :neighbours"
  (declare (ignore args))
  (disable-meta-layer-configuration cxn-inventory) 
  (with-disabled-monitor-notifications
    (multiple-value-bind (meanings cipns)
        (comprehend-all observation-form
                        :cxn-inventory (original-cxn-set cxn-inventory)
                        :gold-standard-meaning observation-meaning)
      (enable-meta-layer-configuration cxn-inventory)
      (let ((solution
             (first
              (loop for cipn in cipns
                    for meaning in meanings
                    when (and (succeeded-cipn-p cipn) ;(find 'fcg::succeeded (statuses cipn))
                              (> (length (applied-constructions cipn)) 1)
                              (equivalent-meaning-networks meaning observation-meaning
                                                           (get-configuration cxn-inventory :meaning-representation-formalism)))
                      collect cipn))))
        (when solution
          (let* ((cxns-to-apply (reverse (original-applied-constructions solution)))
                 (top-lvl-category (extract-top-category-item-based-cxn (last-elt cxns-to-apply))))
            (apply-fix 
             ;; form constraints
             observation-form
             ;; cxns to appply
             cxns-to-apply
             ;; categorial links
             (extract-used-categorial-links solution)
             ;; original cxns to consolidate
             nil
             ;; categories to add
             nil
             ;; top level category
             top-lvl-category
             ;; gold standard consulted p
             (gold-standard-consulted-p solution)
             ;; node
             node
             ;; repair name
             repair-type)))))))


(defun gold-standard-consulted-p (cipn)
  "the highest ranked cipn was disqualified by the gold-standard-meaning goal test"
  (not (get-data (goal-test-data cipn) :result-goal-test-non-gold-standard-meaning)))


(defun substitute-predicate-bindings (predicate bindings)
  (loop with frame-bindings = (irl::map-frame-bindings bindings)
        for elt in predicate
        for assoc-res = (assoc elt frame-bindings)
        if assoc-res
        collect (cdr assoc-res)
        else collect elt))

(defun get-top-level-ts-args (cip-node)
  (let* ((all-units (left-pole-structure (car-resulting-cfs (cipn-car cip-node))))
         (top-unit
          (loop for unit in all-units
                for footprints = (unit-feature-value unit 'fcg:footprints)
                unless (member 'pf::used-as-slot-filler footprints)
                return unit)))
    (second (find 'meaning-args (rest top-unit) :key #'first))))


(defun reject-solutions-with-incompatible-args (cip-nodes gold-standard-meaning required-args)
  "Check if the open variables of the gold standard meaning
   correspond with the args of the top unit of the ts of the cipn."
  (loop for cip-node in cip-nodes
        for parsed-meaning = (extract-meanings
                              (left-pole-structure
                               (car-resulting-cfs (cipn-car cip-node))))
        for ts-top-level-args = (get-top-level-ts-args cip-node)
        for embedding = (irl::embedding parsed-meaning gold-standard-meaning)
        for renamed-ts-args = (when embedding (substitute-predicate-bindings ts-top-level-args (first embedding)))
        when (or (not required-args) ;; if there aren't any required args but you still have some, succeed
                 (equal renamed-ts-args required-args))
        collect cip-node))


(defun extract-used-categorial-links (solution-cipn)
  "For a given solution-cipn, extracts categorial links that were used (based on category)."
  (loop for cipn in (ignore-initial-nodes (reverse (cons solution-cipn (all-parents solution-cipn))))
        append (let* ((processing-cxn
                       (car-applied-cxn (cipn-car cipn)))
                      (processing-cxn
                       (if (equal (attr-val processing-cxn :label) 'fcg::routine)
                         processing-cxn
                         (first (remove (name processing-cxn)
                                        (find-all (attr-val processing-cxn :bare-cxn-name)
                                                  (constructions-list (construction-inventory cipn))
                                                  :key #'(lambda (cxn) (attr-val cxn :bare-cxn-name)))
                                        :key #'name))))                               
                      (units-matching-category
                       (loop for unit in (right-pole-structure processing-cxn)
                             for category = (unit-feature-value unit 'category)
                             when category
                             collect (cons (first unit) category))))
                 (loop for (cxn-unit-name . cxn-category) in units-matching-category
                       for ts-unit-name = (cdr (find cxn-unit-name (car-second-merge-bindings (cipn-car cipn)) :key #'first))
                       for ts-unit = (find ts-unit-name (left-pole-structure (car-source-cfs (cipn-car cipn))):key #'first)
                       for ts-category = (second (find 'category (rest ts-unit) :key #'first))
                       when (and cxn-category ts-category)
                       collect (cons ts-category cxn-category)))))
            
