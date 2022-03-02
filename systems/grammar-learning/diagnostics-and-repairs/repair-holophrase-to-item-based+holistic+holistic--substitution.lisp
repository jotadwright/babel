(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add item-based construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass holophrase->item-based+holistic+holistic--substitution (repair) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair holophrase->item-based+holistic+holistic--substitution)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (create-item-based-cxn problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defmethod repair ((repair holophrase->item-based+holistic+holistic--substitution)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (create-item-based-cxn problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defun create-item-based-cxn (problem node)
  "Creates item-based construction and holistic constructions
based on existing construction with sufficient overlap."
  (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (random-elt (get-data problem :utterances)))
         (utterance-form-constraints (form-constraints-with-variables utterance (get-configuration cxn-inventory :de-render-mode)))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings)) meaning-representation-formalism))
         ) 

    (multiple-value-bind (non-overlapping-meaning-observation
                          non-overlapping-meaning-cxn
                          non-overlapping-form-observation
                          non-overlapping-form-cxn
                          overlapping-meaning-observation
                          overlapping-meaning-cxn
                          overlapping-form-observation
                          overlapping-form-cxn
                          
                          cxn)
        (select-cxn-for-making-item-based-cxn cxn-inventory utterance-form-constraints meaning)
      
      (when (and cxn overlapping-form-cxn)
        
        (let* ((cxn-name-item-based-cxn
                (make-cxn-name (substitute-slot-meets-constraints non-overlapping-form-observation overlapping-form-observation) cxn-inventory :add-cxn-suffix nil))
               (cxn-name-holistic-cxn-1 (make-cxn-name non-overlapping-form-cxn cxn-inventory))
               (cxn-name-holistic-cxn-2 (make-cxn-name non-overlapping-form-observation cxn-inventory))
               (holistic-cxn-1
                (find-cxn-by-form-and-meaning non-overlapping-form-cxn non-overlapping-meaning-cxn cxn-inventory))
               (holistic-cxn-2
                (find-cxn-by-form-and-meaning non-overlapping-form-observation non-overlapping-meaning-observation cxn-inventory))
               ;; holistic cxn boundaries (leftmost/rightmost)
               (boundaries-cxn-1 (get-boundary-units non-overlapping-form-cxn))
               (leftmost-unit-holistic-cxn-1 (first boundaries-cxn-1))
               (rightmost-unit-holistic-cxn-1 (second boundaries-cxn-1))

               (boundaries-cxn-2 (get-boundary-units non-overlapping-form-observation))
               (leftmost-unit-holistic-cxn-2 (first boundaries-cxn-2))
               (rightmost-unit-holistic-cxn-2 (second boundaries-cxn-2))
               
               ;; unit names
               (unit-name-holistic-cxn-1
               (unit-ify (make-cxn-name non-overlapping-form-cxn cxn-inventory :add-cxn-suffix nil)))
               (unit-name-holistic-cxn-2
               (unit-ify (make-cxn-name non-overlapping-form-observation cxn-inventory :add-cxn-suffix nil)))
               
               ;; args and syn-cat
               (lex-class-holistic-cxn-1
                (if holistic-cxn-1
                  (lex-class-cxn holistic-cxn-1)
                  (intern (string-downcase (symbol-name (make-cxn-name non-overlapping-form-cxn cxn-inventory :add-cxn-suffix nil))) :grammar-learning)))
               (lex-class-holistic-cxn-2
                (if holistic-cxn-2
                  (lex-class-cxn holistic-cxn-2)
                  (intern (string-downcase (symbol-name (make-cxn-name non-overlapping-form-observation cxn-inventory :add-cxn-suffix nil))) :grammar-learning)))
               (lex-class-item-based-cxn
                (intern (string-downcase (symbol-name cxn-name-item-based-cxn)) :grammar-learning)) 
               ;; Type hierachy links
               (categorial-link-1
                (cons lex-class-holistic-cxn-1 lex-class-item-based-cxn))
               (categorial-link-2
                (cons lex-class-holistic-cxn-2 lex-class-item-based-cxn))
               ;; Args
               (args-holistic-cxn-1
                (extract-args-from-irl-network non-overlapping-meaning-cxn))
               
               
               (args-holistic-cxn-2
                (extract-args-from-irl-network non-overlapping-meaning-observation))
               (hash-string (third (find 'string non-overlapping-form-cxn :key #'first)))
               ;; CXNs
               
               (new-holistic-cxn-1
                (or holistic-cxn-1
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-1
                                                                ((,unit-name-holistic-cxn-1
                                                                  (args ,args-holistic-cxn-1)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-1))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-1)
                                                                   (right ,rightmost-unit-holistic-cxn-1)))
                                                                 <-
                                                                 (,unit-name-holistic-cxn-1
                                                                  (HASH meaning ,non-overlapping-meaning-cxn)
                                                                  --
                                                                  (HASH form ,non-overlapping-form-cxn)))
                                                                :attributes (:cxn-type holistic
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-cxn :key #'first))
                                                                             :string ,hash-string)
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (new-holistic-cxn-2
                (or holistic-cxn-2
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,cxn-name-holistic-cxn-2
                                                                ((,unit-name-holistic-cxn-2
                                                                  (args ,args-holistic-cxn-2)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn-2))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn-2)
                                                                   (right ,rightmost-unit-holistic-cxn-2)))
                                                                 <-
                                                                 (,unit-name-holistic-cxn-2
                                                                  (HASH meaning ,non-overlapping-meaning-observation)
                                                                  --
                                                                  (HASH form ,non-overlapping-form-observation)))
                                                                :attributes (:cxn-type holistic
                                                                             :repair holophrase->item-based+holistic+holistic--substitution
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning-observation :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form-observation :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
               (item-based-cxn (second (multiple-value-list (eval
                                                             `(def-fcg-cxn ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                           ((?item-based-unit
                                                                             (syn-cat (phrase-type item-based))
                                                                             (subunits (,unit-name-holistic-cxn-2)))
                                                                            (,unit-name-holistic-cxn-2
                                                                             (syn-cat (lex-class ,lex-class-item-based-cxn)))
                                                                            <-
                                                                            (?item-based-unit
                                                                             (HASH meaning ,overlapping-meaning-observation)
                                                                             --
                                                                             (HASH form ,overlapping-form-observation))
                                                                            (,unit-name-holistic-cxn-2
                                                                             (args ,args-holistic-cxn-2)
                                                                             --
                                                                             (boundaries
                                                                              (left ,leftmost-unit-holistic-cxn-2)
                                                                              (right ,rightmost-unit-holistic-cxn-2))))
                                                                           :attributes (:cxn-type item-based
                                                                                        :repair holophrase->item-based+holistic+holistic--substitution
                                                                                        :meaning ,(loop for predicate in overlapping-meaning-observation
                                                                                                        unless (or
                                                                                                                (equal (first predicate) 'get-context)
                                                                                                                (equal (first predicate) 'bind))
                                                                                                        return (first predicate))
                                                                                        :string ,(third (find 'string overlapping-form-observation :key #'first)))
                                                                           
                                                                           :cxn-inventory ,(copy-object cxn-inventory)))))))
          (list new-holistic-cxn-1 new-holistic-cxn-2 item-based-cxn
                categorial-link-1 categorial-link-2 (cons (cdr categorial-link-1) (car categorial-link-1))
                (cons (cdr categorial-link-2) (car categorial-link-2))))))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair holophrase->item-based+holistic+holistic--substitution) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((holistic-cxn (get-processing-cxn (second (restart-data fix))))
           (item-based-cxn (get-processing-cxn (third (restart-data fix))))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (categorial-network (construction-inventory node)))
           (temp-type-hierarchy (copy-object (categorial-network (construction-inventory node))))
           (th (loop for categorial-link in (subseq (restart-data fix) 3)
                     do (add-categories (list (car categorial-link) (cdr categorial-link)) temp-type-hierarchy :recompute-transitive-closure nil)
                     (add-link (car categorial-link) (cdr categorial-link) temp-type-hierarchy :weight 0.5 :recompute-transitive-closure nil)
                     finally (set-categorial-network (construction-inventory node) temp-type-hierarchy))) 
           ;; apply holistic-cxn and add node
           ;; add new cip (green box) to node with first car-resulting cfs = resulting transient structure after application
           (bla (progn (add-element (make-html holistic-cxn))
                  (add-element (make-html item-based-cxn))))
           (new-node-lex (fcg::cip-add-child (initial-node node) (first (fcg-apply holistic-cxn (car-source-cfs (cipn-car (initial-node node))) (direction (cip node))
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
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-categorial-links (subseq (restart-data fix) 3)) ;; add all th links for consolidation
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node-item-based) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node-lex))
      (push (type-of repair) (statuses new-node-item-based))
      (push 'added-by-repair (statuses new-node-lex))
      (push 'added-by-repair (statuses new-node-item-based))
      ;; enqueue only second new node; never backtrack over the first applied holistic construction, we applied them as a block
      (cip-enqueue new-node-item-based (cip node) (get-configuration node :queue-mode)))))

