(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Holophrase Single Addition  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass holophrase->item-based+lexical--addition (repair) 
  ((trigger :initform 'fcg::new-node))) ;; it's always fcg::new-node, we created a new node in the search process

(defmethod repair ((repair holophrase->item-based+lexical--addition)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction and lexical cxn."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-repair-cxns-holophrase-single-addition problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defmethod repair ((repair holophrase->item-based+lexical--addition)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction and lexical cxn."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-repair-cxns-holophrase-single-addition problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

;; todo: there could also be more than one subset cxn
(defun find-subset-holophrase-cxn (transient-structure cxn-inventory gold-standard-meaning utterance)
  (loop with ts-form-constraints = (transient-structure-form-constraints transient-structure)
        for cxn in (constructions cxn-inventory)
        for cxn-form-constraints = (extract-form-predicates cxn)
        for cxn-meaning-constraints = (extract-meaning-predicates cxn)
        for superset-form = (form-constraints-with-variables utterance (get-configuration (cxn-inventory cxn) :de-render-mode))
        for non-overlapping-form = (diff-subset-superset-form cxn superset-form)
        for non-overlapping-meaning = (set-difference gold-standard-meaning (extract-meaning-predicates cxn) :test #'irl:unify-irl-programs)
        for cxn-type =  (phrase-type cxn)
        when (and (eql cxn-type 'holophrase)
                  (equal 1 (length non-overlapping-meaning))
                  (equal 1 (length non-overlapping-form))
                  ;; check if all the strings in the form constraints are present in the superset
                  ;; todo: include precedes relations
                  (loop for cxn-fc in cxn-form-constraints
                        always (if (equal (first cxn-fc) 'string)
                                 (find (third cxn-fc) ts-form-constraints :key #'third :test #'equalp)
                                 t)) ;; loop returns true if all are true, the third elem is the string
                  (loop for predicate in cxn-meaning-constraints
                        always (if (equal (first predicate) 'bind)
                                 (find (fourth predicate) gold-standard-meaning :key #'fourth)
                                 (find (first predicate) gold-standard-meaning :key #'first))))
        ;; needs to be a holophrase, the form constraints for string and precedes constraints need to be a subset of the cxn, the meaning constraints need to be a subset too (todo: see if this is really the case in IRL)
        return (values cxn superset-form non-overlapping-form non-overlapping-meaning)))


(defun create-repair-cxns-holophrase-single-addition (problem node) ;;node = cip node (transient struct, applied cxns, cxn-inventory, ..)
  "Creates item-based construction and a lexical construction
   based on an existing holophrase construction of which the form/meaning are a subset of the observed phrase, and there is a maximum of one differing meaning predicate

   Example:
   - cxn-inventory: contains a holophrase for 'the cube'
   - new observation: 'the red cube'

   Result:
   - lexical-cxn: red-cxn
   - item based-cxn: the-X-cube-cxn
   "
  (let* ((initial-transient-structure (initial-transient-structure node))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))))
         (utterance (random-elt (get-data problem :utterances))))
    (multiple-value-bind (subset-holophrase-cxn
                          superset-form
                          non-overlapping-form
                          non-overlapping-meaning)
        (find-subset-holophrase-cxn initial-transient-structure cxn-inventory gold-standard-meaning utterance)

    (when subset-holophrase-cxn
      
      
        ;;(when (and non-overlapping-form non-overlapping-meaning) ;; this check needs to happen while selecting cxns
          (let* (
                 (overlapping-form (set-difference superset-form non-overlapping-form :test #'irl:unify-irl-programs))
                 (overlapping-meaning (set-difference gold-standard-meaning non-overlapping-meaning :test #'equal))
                 (existing-lex-cxn (find-cxn-by-form-and-meaning non-overlapping-form non-overlapping-meaning cxn-inventory))
                 (lex-cxn-name (make-cxn-name non-overlapping-form cxn-inventory))
                 (cxn-name-item-based-cxn (make-cxn-name overlapping-form cxn-inventory :add-cxn-suffix nil))
                 (unit-name-lex-cxn (second (find 'string non-overlapping-form :key #'first)))
                 ;; lex-class
                 (lex-class-lex-cxn (if existing-lex-cxn
                                      (lex-class-cxn existing-lex-cxn)
                                      (intern (get-base-name unit-name-lex-cxn) :type-hierarchies)))
                 (lex-class-item-based-cxn (intern (symbol-name cxn-name-item-based-cxn) :type-hierarchies))
                 ;; type hierachy links
                 (th-link-1 (cons lex-class-lex-cxn lex-class-item-based-cxn))
                 (th-link-2 (cons lex-class-item-based-cxn lex-class-lex-cxn))
                 ;; args: 
                 (args-lex-cxn (third (first non-overlapping-meaning))) ;; third if bind
                 ;; unit names
                 (lex-cxn (or existing-lex-cxn
                              (second (multiple-value-list (eval
                                                        `(def-fcg-cxn ,lex-cxn-name
                                                                      ((,unit-name-lex-cxn
                                                                        (args (,args-lex-cxn))
                                                                        (syn-cat (phrase-type lexical)
                                                                                 (lex-class ,lex-class-lex-cxn)))
                                                                       <-
                                                                       (,unit-name-lex-cxn
                                                                        (HASH meaning ,non-overlapping-meaning)
                                                                        --
                                                                        (HASH form ,non-overlapping-form)))
                                                                      :attributes (:cxn-type lexical
                                                                                   :repair holophrase->item-based+lexical--addition
                                                                                   :string ,(third (find 'string non-overlapping-form :key #'first)))
                                                                      :cxn-inventory ,(copy-object cxn-inventory)))))));; trick to get the cxn without adding it to the cxn-inventory: make a copy of the cxn-inventory, make the cxn, get it, then forget about the copy
                 (item-based-cxn (second (multiple-value-list (eval
                                                               `(def-fcg-cxn ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                             ((?item-based-unit
                                                                               (syn-cat (phrase-type item-based))
                                                                               (subunits (,unit-name-lex-cxn)))
                                                                              (,unit-name-lex-cxn
                                                                               (args (,args-lex-cxn)) 
                                                                               (syn-cat (lex-class ,lex-class-item-based-cxn)))
                                                                              <-
                                                                              (?item-based-unit
                                                                               (HASH meaning ,overlapping-meaning)
                                                                               --
                                                                               (HASH form ,overlapping-form)))
                                                                             :attributes (:cxn-type item-based
                                                                                          :repair holophrase->item-based+lexical--addition)
                                                                             :cxn-inventory ,(copy-object cxn-inventory)))))))
            (list lex-cxn item-based-cxn th-link-1 th-link-2)
            
          )))))         ;; if no subset-holophrase is found, when returns nil and the repair is skipped


(defmethod handle-fix ((fix fcg::cxn-fix) (repair holophrase->item-based+lexical--addition) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((lexical-cxn (get-processing-cxn (first (restart-data fix))))
           (item-based-cxn (get-processing-cxn (second (restart-data fix))))
           (th-links (subseq (restart-data fix) 2))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (get-type-hierarchy (construction-inventory node)))
           (temp-type-hierarchy (copy-object (get-type-hierarchy (construction-inventory node))))
           (th (loop for th-link in th-links
                     do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy)
                     (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5)
                     finally (set-type-hierarchy (construction-inventory node) temp-type-hierarchy))) 
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
      (set-type-hierarchy (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-cxns (list (original-cxn lexical-cxn) (original-cxn item-based-cxn)))
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-th-links th-links)
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node-item-based) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node-lex))
      (push (type-of repair) (statuses new-node-item-based))
      (push 'added-by-repair (statuses new-node-lex))
      (push 'added-by-repair (statuses new-node-item-based))
      ;; enqueue only second new node; never backtrack over the first applied lexical construction, we applied them as a block
      (cip-enqueue new-node-item-based (cip node) (get-configuration node :queue-mode)))))
