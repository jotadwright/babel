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
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings)) meaning-representation-formalism))
         (cxns-and-links (create-item-based-cxn-from-partial-holistic-analysis problem node)))
    (when cxns-and-links
         (let* ((cxns-to-apply (first cxns-and-links))
                (intermediary-item-based-cxn (last-elt cxns-to-apply))
                (intermediary-item-based-form-constraints (extract-form-predicates intermediary-item-based-cxn))
                (intermediary-item-based-meaning (extract-meaning-predicates intermediary-item-based-cxn))
                (applied-holistic-cxns (remove intermediary-item-based-cxn cxns-to-apply)))
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
        (select-item-based-cxn-for-making-item-based-cxn cxn-inventory intermediary-item-based-cxn meaning-representation-formalism)))))
  nil)

(defun create-dummy-predicates-for-args (args-list)
  (loop for args in args-list
        collect (list 'dummy (second args) (first args))))
  

(defun select-item-based-cxn-for-making-item-based-cxn (cxn-inventory intermediary-item-based-cxn meaning-representation-formalism)
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        do (when (and
                  (eql (attr-val cxn :cxn-type) 'item-based)
                  (eql (attr-val cxn :label) 'fcg::routine))
             (let* ((cxn-args (extract-args-apply-last cxn)) ;; fill up the gaps created by args with a dummy predicate, remove it in the end
                    (cxn-dummy-predicates (create-dummy-predicates-for-args cxn-args))
                    (cxn-meaning (append cxn-dummy-predicates (extract-meaning-predicates cxn)))
                    (intermediary-item-based-form-constraints (extract-form-predicates intermediary-item-based-cxn))
                    (intermediary-args (extract-args-apply-last intermediary-item-based-cxn))
                    (intermediary-dummy-predicates (create-dummy-predicates-for-args intermediary-args))
                    (intermediary-item-based-meaning (append intermediary-dummy-predicates (extract-meaning-predicates intermediary-item-based-cxn)))
                    (non-overlapping-meanings (multiple-value-list (diff-meaning-networks intermediary-item-based-meaning cxn-meaning meaning-representation-formalism)))
                    (non-overlapping-meaning-observation (first non-overlapping-meanings))
                    (non-overlapping-meaning-cxn (second non-overlapping-meanings))
                    (overlapping-meaning-observation (set-difference (extract-meaning-predicates intermediary-item-based-cxn) non-overlapping-meaning-observation :test #'equal))
                    (overlapping-meaning-cxn (set-difference (extract-meaning-predicates cxn) non-overlapping-meaning-cxn :test #'equal))
                    (non-overlapping-form-observation (non-overlapping-form intermediary-item-based-form-constraints cxn :nof-observation t))
                    (non-overlapping-form-cxn (non-overlapping-form intermediary-item-based-form-constraints cxn :nof-cxn t))
                    (overlapping-form-cxn (set-difference (extract-form-predicates cxn) non-overlapping-form-cxn :test #'equal))
                    (overlapping-form-observation (set-difference intermediary-item-based-form-constraints non-overlapping-form-observation :test #'equal))
                    ;; args
                    (args-holistic-cxn-1
                     (extract-args-from-meaning-networks non-overlapping-meaning-cxn overlapping-meaning-cxn meaning-representation-formalism))
                    (args-holistic-cxn-2
                     (extract-args-from-meaning-networks non-overlapping-meaning-observation overlapping-meaning-observation meaning-representation-formalism)))
               (when (and
                      (> (length overlapping-meaning-observation) 0)
                      (> (length overlapping-meaning-cxn) 0)
                      (> (length non-overlapping-meaning-observation) 0)
                      (> (length non-overlapping-meaning-cxn) 0)
                      (> (length non-overlapping-form-observation) 0)
                      (> (length non-overlapping-form-cxn) 0)
                      (> (length overlapping-form-observation) 0)
                      (<= (length args-holistic-cxn-1) 2) ; check if the meaning network is continuous
                      (<= (length args-holistic-cxn-2) 2) ; check if the meaning network is continuous
                      overlapping-form-cxn
                      cxn
                      (check-meets-continuity non-overlapping-form-cxn)
                      (check-meets-continuity non-overlapping-form-observation)
                      (equivalent-irl-programs?
                       (substitute-slot-meets-constraints non-overlapping-form-observation overlapping-form-observation)
                       (substitute-slot-meets-constraints non-overlapping-form-cxn overlapping-form-cxn)))
                 (return (values non-overlapping-meaning-observation
                                 non-overlapping-meaning-cxn
                                 non-overlapping-form-observation
                                 non-overlapping-form-cxn
                                 overlapping-meaning-observation
                                 ;overlapping-meaning-cxn
                                 overlapping-form-observation
                                 args-holistic-cxn-1
                                 args-holistic-cxn-2
                                 cxn
                                 )))))))