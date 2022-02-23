(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Holophrase Single Addition  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass holophrase->item-based+holistic--addition (repair) 
  ((trigger :initform 'fcg::new-node))) ;; it's always fcg::new-node, we created a new node in the search process

(defmethod repair ((repair holophrase->item-based+holistic--addition)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction and holistic cxn."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (create-repair-cxns-holophrase-single-addition problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))

(defmethod repair ((repair holophrase->item-based+holistic--addition)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction and holistic cxn."
  (when (initial-node-p node)
    (let ((constructions-and-categorial-links (create-repair-cxns-holophrase-single-addition problem node)))
      (when constructions-and-categorial-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-categorial-links)))))


(defun create-repair-cxns-holophrase-single-addition (problem node) ;;node = cip node (transient struct, applied cxns, cxn-inventory, ..)
  "Creates item-based construction and a holistic construction
   based on an existing holophrase construction of which the form/meaning are a subset of the observed phrase, and there is a maximum of one differing meaning predicate

   Example:
   - cxn-inventory: contains a holophrase for 'the cube'
   - new observation: 'the red cube'

   Result:
   - holistic-cxn: red-cxn
   - item based-cxn: the-X-cube-cxn
   "
  (let* ((initial-transient-structure (initial-transient-structure node))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (meaning-representation-formalism (get-configuration cxn-inventory :meaning-representation-formalism))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))
                                                                   meaning-representation-formalism))
         (utterance (random-elt (get-data problem :utterances))))
    (multiple-value-bind (subset-holophrase-cxn
                          superset-form
                          non-overlapping-form
                          non-overlapping-meaning)
        (find-subset-holophrase-cxn initial-transient-structure cxn-inventory gold-standard-meaning utterance)

      (when subset-holophrase-cxn
      
      
        ;;(when (and non-overlapping-form non-overlapping-meaning) ;; this check needs to happen while selecting cxns
        (let* ((overlapping-form (set-difference superset-form non-overlapping-form :test #'equal))
               (overlapping-meaning (set-difference gold-standard-meaning non-overlapping-meaning :test #'equal))
               (existing-holistic-cxn (find-cxn-by-form-and-meaning non-overlapping-form non-overlapping-meaning cxn-inventory))
               (boundaries-holistic-cxn (get-boundary-units non-overlapping-form))
               (leftmost-unit-holistic-cxn (first boundaries-holistic-cxn))
               (rightmost-unit-holistic-cxn (second boundaries-holistic-cxn))
               (holistic-cxn-name (make-cxn-name non-overlapping-form cxn-inventory))
               ;
               (cxn-name-item-based-cxn (make-cxn-name
                                         (substitute-slot-meets-constraints non-overlapping-form overlapping-form) cxn-inventory :add-cxn-suffix nil))
               (unit-name-holistic-cxn
                (unit-ify (make-cxn-name non-overlapping-form cxn-inventory :add-cxn-suffix nil))
                )
               ;; lex-class
               (lex-class-holistic-cxn
                (if existing-holistic-cxn
                  (lex-class-cxn existing-holistic-cxn)
                  (intern (string-downcase (symbol-name holistic-cxn-name)) :grammar-learning)))
               (lex-class-item-based-cxn
                (intern (string-downcase (symbol-name cxn-name-item-based-cxn)) :grammar-learning)) 
               ;; type hierachy links
               (categorial-link
                (cons lex-class-item-based-cxn lex-class-holistic-cxn))
               ;; args: 
               (args-holistic-cxn
                (loop for predicate in non-overlapping-meaning
                      collect (extract-args-from-predicate predicate meaning-representation-formalism)))
               
               (holistic-cxn
                (or existing-holistic-cxn
                    (second (multiple-value-list (eval
                                                  `(def-fcg-cxn ,holistic-cxn-name
                                                                ((,unit-name-holistic-cxn
                                                                  (args ,args-holistic-cxn)
                                                                  (syn-cat (phrase-type holistic)
                                                                           (lex-class ,lex-class-holistic-cxn))
                                                                  (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                   (right ,rightmost-unit-holistic-cxn)))
                                                                 <-
                                                                 (,unit-name-holistic-cxn
                                                                  (HASH meaning ,non-overlapping-meaning)
                                                                  --
                                                                  (HASH form ,non-overlapping-form)))
                                                                :attributes (:cxn-type holistic
                                                                             :repair holophrase->item-based+holistic--addition
                                                                             :meaning ,(fourth (find 'bind non-overlapping-meaning :key #'first))
                                                                             :string ,(third (find 'string non-overlapping-form :key #'first)))
                                                                :cxn-inventory ,(copy-object cxn-inventory)))))));; trick to get the cxn without adding it to the cxn-inventory: make a copy of the cxn-inventory, make the cxn, get it, then forget about the copy
               (item-based-cxn
                (second (multiple-value-list (eval
                                              `(def-fcg-cxn ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                            ((?item-based-unit
                                                              (syn-cat (phrase-type item-based))
                                                              (subunits (,unit-name-holistic-cxn)))
                                                             (,unit-name-holistic-cxn
                                                              (syn-cat (lex-class ,lex-class-item-based-cxn)))
                                                             <-
                                                             (?item-based-unit
                                                              (HASH meaning ,overlapping-meaning)
                                                              --
                                                              (HASH form ,overlapping-form))
                                                             (,unit-name-holistic-cxn
                                                              (args ,args-holistic-cxn)
                                                              --
                                                              (boundaries
                                                                   (left ,leftmost-unit-holistic-cxn)
                                                                   (right ,rightmost-unit-holistic-cxn))))
                                                            :attributes (:cxn-type item-based
                                                                         :repair holophrase->item-based+holistic--addition
                                                                         :meaning ,(loop for predicate in overlapping-meaning
                                                                                         unless (or
                                                                                                 (equal (first predicate) 'get-context)
                                                                                                 (equal (first predicate) 'bind))
                                                                                         return (first predicate))
                                                                         :string ,(third (find 'string overlapping-form :key #'first)))
                                                            :cxn-inventory ,(copy-object cxn-inventory)))))))
          (list holistic-cxn item-based-cxn categorial-link))))))
            

(defmethod handle-fix ((fix fcg::cxn-fix) (repair holophrase->item-based+holistic--addition) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((holistic-cxn (get-processing-cxn (first (restart-data fix))))
           (item-based-cxn (get-processing-cxn (second (restart-data fix))))
           (categorial-links (subseq (restart-data fix) 2))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (categorial-network (construction-inventory node)))
           (temp-type-hierarchy (copy-object (categorial-network (construction-inventory node)))))
      ;; add categorial-links
      (loop for categorial-link in categorial-links
                     do (add-categories (list (car categorial-link) (cdr categorial-link)) temp-type-hierarchy :recompute-transitive-closure nil)
                     (add-link (car categorial-link) (cdr categorial-link) temp-type-hierarchy :weight 0.5 :recompute-transitive-closure nil)
                     finally (set-categorial-network (construction-inventory node) temp-type-hierarchy))
           ;; apply holistic-cxn and add node
           ;; add new cip (green box) to node with first car-resulting cfs = resulting transient structure after application
           (let* ((new-node-lex (fcg::cip-add-child (initial-node node) (first (fcg-apply holistic-cxn (car-source-cfs (cipn-car (initial-node node))) (direction (cip node))
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
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-cxns (list (original-cxn holistic-cxn) (original-cxn item-based-cxn)))
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-categorial-links categorial-links)
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node-item-based) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node-lex))
      (push (type-of repair) (statuses new-node-item-based))
      (push 'added-by-repair (statuses new-node-lex))
      (push 'added-by-repair (statuses new-node-item-based))
      ;; enqueue only second new node; never backtrack over the first applied holistic construction, we applied them as a block
      (cip-enqueue new-node-item-based (cip node) (get-configuration node :queue-mode))))))
