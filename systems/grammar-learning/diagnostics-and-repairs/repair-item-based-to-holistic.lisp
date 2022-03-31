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

      
                
(defun create-holistic-cxn-from-partial-analysis (problem node)
  (let* ((processing-cxn-inventory (construction-inventory node))
         (original-cxn-inventory (original-cxn-set processing-cxn-inventory))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning-representation-formalism (get-configuration processing-cxn-inventory :meaning-representation-formalism))
         (best-partial-analysis-node (get-best-partial-analysis-cipn
                                      utterance
                                      original-cxn-inventory
                                      (get-configuration processing-cxn-inventory :learning-strategy)))
         (applied-cxns (applied-constructions best-partial-analysis-node))
         (item-based-cxn (first (filter-by-phrase-type 'item-based applied-cxns))))
    (when item-based-cxn
      (let* ((root-form-constraints (form-predicates-with-variables (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car best-partial-analysis-node)))) 'form))))
        (when (check-meets-continuity root-form-constraints) ;there is one continuous string in root
          (let* ((all-cars (append (cipn-car best-partial-analysis-node) (all-parents best-partial-analysis-node)))
                 (remaining-meaning (loop for car in all-cars
                                          collect car)
    ))))))))

#|
(defun create-holistic-cxn (problem node)
  "Creates a holistic cxn if a surrounding item-based cxn with one empty slot exists."
  (with-disabled-monitor-notifications
    (let* ((processing-cxn-inventory (construction-inventory node))
           (original-cxn-inventory (original-cxn-set processing-cxn-inventory))
           (meaning-representation-formalism (get-configuration processing-cxn-inventory :meaning-representation-formalism))
           (resulting-cars (loop for cxn in (constructions processing-cxn-inventory) ; sort by score! there could be multiple combinations of holistic+item-based with different coverages, do we make multiple hypotheses and take the highest scored one?
                                 when (and
                                       (equal (attr-val cxn :cxn-type) 'item-based)
                                       (fcg-apply cxn (car-source-cfs (cipn-car (initial-node node)))
                                                  (direction (cip node))
                                                  :configuration (configuration processing-cxn-inventory)
                                                  :cxn-inventory processing-cxn-inventory))
                                 return it))
           (item-based-cxn (when resulting-cars (car-applied-cxn (first resulting-cars))))
           (observation (when resulting-cars (left-pole-structure (car-resulting-cfs (first resulting-cars)))))
           (string-predicates-in-root (when resulting-cars (form-predicates-with-variables (extract-string (get-root observation))))))
    ;; TODO: rewrite this logic: there can be more than one matching lex cxn without th links and it could still apply, so there can be no new lex cxn and still make the th links --> is this for add-categorial-links? 
    ;; there is more than one string in root, but there can be a matching lex cxn with missing th links that can be subtracted
    (when (and (> (length string-predicates-in-root) 0)
               item-based-cxn)
      (let* (
             (matching-holistic-cxns (find-matching-holistic-cxns-in-root original-cxn-inventory string-predicates-in-root)))
        ;; there are one or more lex cxns, and one remaining string in root
        (when (or (and matching-holistic-cxns
                       (= 1 (- (length string-predicates-in-root) (length matching-holistic-cxns)))) ; don't do this, just check for meets continuity of the remainder
                  (= (length string-predicates-in-root) 1))
          ;; construct the remaining cxn first
          (let* ((utterance (random-elt (get-data problem :utterances)))
                 (categorial-network (categorial-network original-cxn-inventory))
                 (meaning-predicates-gold (meaning-predicates-with-variables (first (get-data problem :meanings))
                                                                             meaning-representation-formalism))
                 (meaning-predicates-gold-minus-lex (subtract-holistic-cxn-meanings matching-holistic-cxns meaning-predicates-gold)) ;; todo: use test equal instead of unify-irl
                 (meaning-predicates-observed (extract-meanings observation))
                 (meaning-predicates-holistic-cxn (if (= 1 (length string-predicates-in-root))
                                               (set-difference meaning-predicates-gold meaning-predicates-observed :test #'unify)
                                               (set-difference meaning-predicates-gold-minus-lex meaning-predicates-observed :test #'unify)))
                 (form-predicates-holistic-cxn (if (= 1 (length string-predicates-in-root)) ;; why this check? just subtract always, even if it's ()
                                            string-predicates-in-root
                                            (subtract-holistic-cxn-forms matching-holistic-cxns string-predicates-in-root)))
                 (existing-holistic-cxn (find-cxn-by-form-and-meaning form-predicates-holistic-cxn meaning-predicates-holistic-cxn original-cxn-inventory :cxn-type 'holistic))
                 (cxn-name (make-cxn-name (third (first form-predicates-holistic-cxn)) original-cxn-inventory))
                 (unit-name (second (first form-predicates-holistic-cxn)))
                 (lex-class (if existing-holistic-cxn
                              (lex-class-cxn existing-holistic-cxn)
                              (intern (get-base-name unit-name) :grammar-learning)))
                 (args (mapcar #'(lambda (predicate) ;; rewrite args!
                                   (extract-args-from-predicate predicate meaning-representation-formalism))
                               meaning-predicates-holistic-cxn))
                 (new-holistic-cxn (or existing-holistic-cxn (second (multiple-value-list (eval
                                                                                 `(def-fcg-cxn ,cxn-name
                                                                                               ((,unit-name
                                                                                                 (syn-cat (phrase-type holistic)
                                                                                                          (lex-class ,lex-class))
                                                                                                 (args ,args))
                                                                                                <-
                                                                                                (,unit-name
                                                                                                 (HASH meaning ,meaning-predicates-holistic-cxn)
                                                                                                 --
                                                                                                 (HASH form ,form-predicates-holistic-cxn)))
                                                                                               :attributes (:cxn-type holistic
                                                                                                            :repair item-based->holistic
                                                                                                            :meaning ,(fourth (find 'bind meaning-predicates-holistic-cxn :key #'first))
                                                                                                            :string ,(third (find 'string form-predicates-holistic-cxn :key #'first)))
                                                                                               :cxn-inventory ,(copy-object original-cxn-inventory)))))))
                 ;; make a list of all cxns, sort them
                 (applied-holistic-cxns (filter-by-phrase-type 'holistic (applied-constructions node)))
                 (holistic-cxns (sort-cxns-by-form-string (append
                                                      (list new-holistic-cxn)
                                                      matching-holistic-cxns
                                                      applied-holistic-cxns) utterance))
                 (lex-classes-holistic-cxns (when holistic-cxns (map 'list #'lex-class-cxn holistic-cxns)))
                 (lex-classes-item-based-units (when item-based-cxn (get-all-unit-lex-classes (original-cxn item-based-cxn))))
                 ;; assign all th links
                 (categorial-links (when (and lex-classes-holistic-cxns
                                      lex-classes-item-based-units
                                      (= (length lex-classes-holistic-cxns) (length lex-classes-item-based-units)))
                             (create-new-categorial-links lex-classes-holistic-cxns lex-classes-item-based-units categorial-network))))
            ;; return
            (when categorial-links
              (list new-holistic-cxn (append (list item-based-cxn)
                                        (list (get-processing-cxn new-holistic-cxn))
                                        (unless (= 1 (length string-predicates-in-root))
                                          (map 'list #'get-processing-cxn matching-holistic-cxns))
                                        (map 'list #'get-processing-cxn applied-holistic-cxns)) categorial-links)))))))))
|#
