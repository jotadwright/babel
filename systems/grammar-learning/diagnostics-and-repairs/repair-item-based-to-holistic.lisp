(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add holistic construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass item-based->holistic (repair) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair item-based->holistic)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holistic construction."
  (when (initial-node-p node)
    (let ((holistic-cxn-and-categorial-link (create-holistic-cxn-from-partial-analysis problem node)))
      (when holistic-cxn-and-categorial-link
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data holistic-cxn-and-categorial-link)))))
  
(defmethod repair ((repair item-based->holistic)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new holistic construction."
  (when (initial-node-p node)
    (let ((holistic-cxn-and-categorial-link (create-holistic-cxn-from-partial-analysis problem node)))
      (when holistic-cxn-and-categorial-link 
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data holistic-cxn-and-categorial-link)))))

(defun subtract-cxn-meanings-from-gold-standard-meaning (cxns gold-standard-meaning)
  (loop with resulting-meaning = gold-standard-meaning
                       for cxn in cxns
                       for meaning = (get-subtracted-meaning-from-cxn cxn gold-standard-meaning)
                       do (setf resulting-meaning (set-difference resulting-meaning meaning :test #'equal))
                       finally (return resulting-meaning)))
                
(defun create-holistic-cxn-from-partial-analysis (problem node)
  (let* ((processing-cxn-inventory (construction-inventory node))
         (original-cxn-inventory (original-cxn-set processing-cxn-inventory))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning-representation-formalism (get-configuration processing-cxn-inventory :meaning-representation-formalism))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings)) meaning-representation-formalism))
         (best-partial-analysis-node (get-best-partial-analysis-cipn
                                      utterance
                                      original-cxn-inventory
                                      (get-configuration processing-cxn-inventory :learning-strategy)))
         (applied-cxns (applied-constructions best-partial-analysis-node))
         (item-based-cxn (first (filter-by-phrase-type 'item-based applied-cxns))))
    (when item-based-cxn
      (let* ((root-form-constraints (form-predicates-with-variables (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car best-partial-analysis-node)))) 'form))))
        (when (check-meets-continuity root-form-constraints) ;there is one continuous string in root
          (let* ((remaining-meaning (subtract-cxn-meanings-from-gold-standard-meaning applied-cxns gold-standard-meaning))
                 (holistic-cxn-name (make-cxn-name root-form-constraints original-cxn-inventory :add-numeric-tail t :add-cxn-suffix t))
                 (lex-class-holistic-cxn (make-lex-class holistic-cxn-name :trim-cxn-suffix t))
                 (lex-classes-item-based-cxn (get-all-unit-lex-classes item-based-cxn))
                 ;; todo: check which slot is not connected in the network, create the new link
                 ;; for indirectly connected nodes, also add the direct link
                 (boundaries-holistic-cxn (get-boundary-units root-form-constraints))
                 (leftmost-unit-holistic-cxn (first boundaries-holistic-cxn))
                 (rightmost-unit-holistic-cxn (second boundaries-holistic-cxn))
                 (args-holistic-cxn (extract-args-from-irl-network remaining-meaning))
                 (holistic-cxn (second (multiple-value-list (eval
                                                             `(def-fcg-cxn ,holistic-cxn-name
                                                                           ((,leftmost-unit-holistic-cxn
                                                                             (args ,args-holistic-cxn)
                                                                             (syn-cat (phrase-type holistic)
                                                                                      (lex-class ,lex-class-holistic-cxn))
                                                                             (boundaries
                                                                              (left ,leftmost-unit-holistic-cxn)
                                                                              (right ,rightmost-unit-holistic-cxn)))
                                                                            <-
                                                                            (,leftmost-unit-holistic-cxn
                                                                             (HASH meaning ,remaining-meaning)
                                                                             --
                                                                             (HASH form ,root-form-constraints)))
                                                                           :attributes (:cxn-type holistic
                                                                                        :repair item-based->holistic
                                                                                        :meaning ,(fourth (find 'bind remaining-meaning :key #'first))
                                                                                        :string ,(third (find 'string root-form-constraints :key #'first)))
                                                                           :cxn-inventory ,(copy-object original-cxn-inventory)))))))))))))
          
                 


