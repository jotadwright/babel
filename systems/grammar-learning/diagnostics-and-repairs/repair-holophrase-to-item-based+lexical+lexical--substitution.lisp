(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add item-based construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass holophrase->item-based+lexical+lexical--substitution (repair) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holophrase->item-based+lexical+lexical--substitution)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defmethod repair ((repair holophrase->item-based+lexical+lexical--substitution)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defun create-item-based-cxn (problem node)
  "Creates item-based construction and lexical constructions
based on existing construction with sufficient overlap."
  (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))) 

    (multiple-value-bind (non-overlapping-meaning-observation
                          non-overlapping-meaning-cxn
                          non-overlapping-form-observation
                          non-overlapping-form-cxn
                          overlapping-meaning-cxn
                          overlapping-form-cxn
                          cxn)
        (select-cxn-for-making-item-based-cxn cxn-inventory utterance meaning)
      
      (when cxn
        
        (let* ((cxn-name-item-based-cxn
                (make-cxn-name overlapping-form-cxn cxn-inventory :add-cxn-suffix nil))
               (lex-cxn-1
                (find-cxn-by-form-and-meaning non-overlapping-form-cxn non-overlapping-meaning-cxn cxn-inventory))
               (lex-cxn-2
                (find-cxn-by-form-and-meaning non-overlapping-form-observation non-overlapping-meaning-observation cxn-inventory))
               ;; unit names
               (unit-name-lex-cxn-1
                (second (find 'string non-overlapping-form-cxn :key #'first)))
               (unit-name-lex-cxn-2
                (second (find 'string non-overlapping-form-observation :key #'first)))
               ;; args and syn-cat
               (lex-class-lex-cxn-1
                (if lex-cxn-1
                  (lex-class-cxn lex-cxn-1)
                  (intern (get-base-name unit-name-lex-cxn-1) :grammar-learning)))
               (lex-class-lex-cxn-2
                (if lex-cxn-2
                  (lex-class-cxn lex-cxn-2)
                  (intern (get-base-name unit-name-lex-cxn-2) :grammar-learning)))
               (lex-class-item-based-cxn
                (intern (string-downcase (symbol-name cxn-name-item-based-cxn)) :grammar-learning)) 
               ;; Type hierachy links
               (th-link-1
                (cons lex-class-lex-cxn-1 lex-class-item-based-cxn))
               (th-link-2
                (cons lex-class-lex-cxn-2 lex-class-item-based-cxn))
               ;; Args
               (args-lex-cxn-1
                (extract-args-from-predicates (first non-overlapping-meaning-cxn) meaning-representation-formalism))
                                            
               
               (args-lex-cxn-2
                (extract-args-from-predicates (first non-overlapping-meaning-observation) meaning-representation-formalism)) 
               
               ;; CXNs
               
               (new-lex-cxn-1
                (or lex-cxn-1
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,(make-cxn-name non-overlapping-form-cxn cxn-inventory)
                                                                ((,unit-name-lex-cxn-1
                                                                  (args (,args-lex-cxn-1))
                                                                  (syn-cat (phrase-type lexical)
                                                                           (lex-class ,lex-class-lex-cxn-1)))
                                                                 <-
                                                                 (,unit-name-lex-cxn-1
                                                                  (HASH meaning ,non-overlapping-meaning-cxn)
                                                                  --
                                                                  (HASH form ,non-overlapping-form-cxn)))
                                                                :attributes (:cxn-type lexical
                                                                             :repair holophrase->item-based+lexical+lexical--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-cxn :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form-cxn :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (new-lex-cxn-2
                (or lex-cxn-2
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,(make-cxn-name non-overlapping-form-observation cxn-inventory)
                                                                ((,unit-name-lex-cxn-2
                                                                  (args (,args-lex-cxn-2))
                                                                  (syn-cat (phrase-type lexical)
                                                                           (lex-class ,lex-class-lex-cxn-2)))
                                                                 <-
                                                                 (,unit-name-lex-cxn-2
                                                                  (HASH meaning ,non-overlapping-meaning-observation)
                                                                  --
                                                                  (HASH form ,non-overlapping-form-observation)))
                                                                :attributes (:cxn-type lexical
                                                                             :repair holophrase->item-based+lexical+lexical--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-observation :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form-observation :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (item-based-cxn (second (multiple-value-list (eval
                                                             `(def-fcg-cxn ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                           ((?item-based-unit
                                                                             (syn-cat (phrase-type item-based))
                                                                             (subunits (,unit-name-lex-cxn-1)))
                                                                            (,unit-name-lex-cxn-1
                                                                             (syn-cat (lex-class ,lex-class-item-based-cxn)))
                                                                            <-
                                                                            (?item-based-unit
                                                                             (HASH meaning ,overlapping-meaning-cxn)
                                                                             --
                                                                             (HASH form ,overlapping-form-cxn))
                                                                            (,unit-name-lex-cxn-1
                                                                             (args (,args-lex-cxn-1))
                                                                             --))
                                                                           :attributes (:cxn-type item-based
                                                                                        :repair holophrase->item-based+lexical+lexical--substitution
                                                                                        :meaning ,(loop for predicate in overlapping-meaning-cxn
                                                                                                        unless (or
                                                                                                                (equal (first predicate) 'get-context)
                                                                                                                (equal (first predicate) 'bind))
                                                                                                        return (first predicate))
                                                                                        :string ,(third (find 'string overlapping-form-cxn :key #'first)))
                                                                           
                                                                           :cxn-inventory ,(copy-object cxn-inventory)))))))
          (list new-lex-cxn-1 new-lex-cxn-2 item-based-cxn
                th-link-1 th-link-2 (cons (cdr th-link-1) (car th-link-1))
                (cons (cdr th-link-2) (car th-link-2))))))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair holophrase->item-based+lexical+lexical--substitution) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((lexical-cxn (get-processing-cxn (second (restart-data fix))))
           (item-based-cxn (get-processing-cxn (third (restart-data fix))))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (categorial-network (construction-inventory node)))
           (temp-type-hierarchy (copy-object (categorial-network (construction-inventory node))))
           (th (loop for th-link in (subseq (restart-data fix) 3)
                     do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy)
                         (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5)
                     finally (set-categorial-network (construction-inventory node) temp-type-hierarchy))) 
           ;; apply lexical-cxn and add node
           ;; add new cip (green box) to node with first car-resulting cfs = resulting transient structure after application
           (new-node-lex (fcg::cip-add-child (initial-node node) (first (fcg-apply lexical-cxn (car-source-cfs (cipn-car (initial-node node))) (direction (cip node))
                                                                    :configuration (configuration (construction-inventory node))
                                                                    :cxn-inventory (construction-inventory node)))))
           ;; apply item-based cxn to this new node, and add second new node
           (new-node-item-based (fcg::cip-add-child new-node-lex (first (fcg-apply item-based-cxn (car-resulting-cfs (cipn-car new-node-lex)) (direction (cip node))
                                                                                   :configuration (configuration (construction-inventory node))
                                                                                    :cxn-inventory (construction-inventory node))))))
      ;; ignore
      ;; Reset type hierarchy
      (set-categorial-network (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-cxns (subseq (restart-data fix) 0 3)) ;; add all learned cxns for consolidation
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-th-links (subseq (restart-data fix) 3)) ;; add all th links for consolidation
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node-item-based) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node-lex))
      (push (type-of repair) (statuses new-node-item-based))
      (push 'added-by-repair (statuses new-node-lex))
      (push 'added-by-repair (statuses new-node-item-based))
      ;; enqueue only second new node; never backtrack over the first applied lexical construction, we applied them as a block
      (cip-enqueue new-node-item-based (cip node) (get-configuration node :queue-mode)))))

