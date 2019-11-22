(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add item-based construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass add-item-based-cxn (repair) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair add-item-based-cxn)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (let ((constructions-and-th-links (create-item-based-cxn problem node)))
        (when constructions-and-th-links
          (make-instance 'fcg::cxn-fix
                         :repair repair
                         :problem problem
                         :restart-data constructions-and-th-links))))

(defmethod repair ((repair add-item-based-cxn)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (let ((constructions-and-th-links (create-item-based-cxn problem node)))
        (when constructions-and-th-links
          (make-instance 'fcg::cxn-fix
                         :repair repair
                         :problem problem
                         :restart-data constructions-and-th-links))))

(defun create-item-based-cxn (problem node)
  "Creates item-based construction and lexical constructions
based on existing construction with sufficient overlap."
  (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (random-elt (get-data problem :utterances)))
         (meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings)))))

    (multiple-value-bind (non-overlapping-meaning-observation non-overlapping-meaning-cxn
                                                              non-overlapping-form-observation non-overlapping-form-cxn
                                                              overlapping-meaning-cxn overlapping-form-cxn
                                                              cxn)
        (select-cxn-for-making-item-based-cxn cxn-inventory utterance meaning)
      
      (when cxn
        (let* (;; CXN names
               (cxn-name-lex-cxn-1 (make-cxn-name non-overlapping-form-cxn cxn-inventory))
               (cxn-name-lex-cxn-2 (make-cxn-name non-overlapping-form-observation cxn-inventory))
               (cxn-name-item-based-cxn (make-cxn-name overlapping-form-cxn cxn-inventory))
               ;; args and syn-cat
               (lex-class-lex-cxn-1 (intern (symbol-name (make-const "CAT")) :type-hierarchies))
               (lex-class-lex-cxn-2 (intern (symbol-name (make-const "CAT")) :type-hierarchies))
               (lex-class-item-based-cxn (intern (symbol-name (make-const "CAT")) :type-hierarchies))
               ;; Type hierachy links
               (th-link-1 (cons lex-class-lex-cxn-1 lex-class-item-based-cxn))
               (th-link-2 (cons lex-class-lex-cxn-2 lex-class-item-based-cxn))
               ;; Args
               (args-lex-cxn-1 (third (first non-overlapping-meaning-cxn)))
               (args-lex-cxn-2 (third (first non-overlapping-meaning-observation)))
               ;; unit names
               (unit-name-lex-cxn-1 (second (find 'string non-overlapping-form-cxn :key #'first)))
               (unit-name-lex-cxn-2 (second (find 'string non-overlapping-form-observation :key #'first)))
               ;; CXNs
               (lex-cxn-1 (second (multiple-value-list (eval
                                                        `(def-fcg-cxn ,cxn-name-lex-cxn-1
                                                                      ((,unit-name-lex-cxn-1
                                                                        (args (,args-lex-cxn-1))
                                                                        (syn-cat (phrase-type lexical)
                                                                                 (lex-class ,lex-class-lex-cxn-1)))
                                                                       <-
                                                                       (,unit-name-lex-cxn-1
                                                                        (HASH meaning ,non-overlapping-meaning-cxn)
                                                                        --
                                                                        (HASH form ,non-overlapping-form-cxn)))
                                                                      :cxn-inventory ,(copy-object cxn-inventory))))))
               (lex-cxn-2 (second (multiple-value-list (eval
                                                        `(def-fcg-cxn ,cxn-name-lex-cxn-2
                                                                      ((,unit-name-lex-cxn-2
                                                                        (args (,args-lex-cxn-2))
                                                                        (syn-cat (phrase-type lexical)
                                                                                 (lex-class ,lex-class-lex-cxn-2)))
                                                                       <-
                                                                       (,unit-name-lex-cxn-2
                                                                        (HASH meaning ,non-overlapping-meaning-observation)
                                                                        --
                                                                        (HASH form ,non-overlapping-form-observation)))
                                                                      :cxn-inventory ,(copy-object cxn-inventory))))))
               (item-based-cxn (second (multiple-value-list (eval
                                                             `(def-fcg-cxn ,cxn-name-item-based-cxn
                                                                           ((?item-based-unit
                                                                             (syn-cat (phrase-type item-based))
                                                                             (subunits (,unit-name-lex-cxn-1)))
                                                                            (,unit-name-lex-cxn-1
                                                                             (args (,args-lex-cxn-1))
                                                                             (syn-cat (lex-class ,lex-class-item-based-cxn)))
                                                                            <-
                                                                            (?item-based-unit
                                                                             (HASH meaning ,overlapping-meaning-cxn)
                                                                             --
                                                                             (HASH form ,overlapping-form-cxn)))
                                                                           :cxn-inventory ,(copy-object cxn-inventory)))))))
          (list lex-cxn-1 lex-cxn-2 item-based-cxn
                th-link-1 th-link-2 (cons (cdr th-link-1) (car th-link-1))
                (cons (cdr th-link-2) (car th-link-2))))))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair add-item-based-cxn) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((lexical-cxn (get-processing-cxn (second (restart-data fix))))
           (item-based-cxn (get-processing-cxn (third (restart-data fix))))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (get-type-hierarchy (construction-inventory node)))
           (temp-type-hierarchy (copy-object (get-type-hierarchy (construction-inventory node))))
           (th (loop for th-link in (subseq (restart-data fix) 3)
                     do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy)
                         (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5)
                     finally (set-type-hierarchy (construction-inventory node) temp-type-hierarchy))) 
           ;; apply lexical-cxn and add node
           (new-node-lex (fcg::cip-add-child node (first (fcg-apply lexical-cxn (car-resulting-cfs (cipn-car node)) (direction (cip node))
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
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-cxns (subseq (restart-data fix) 0 3))
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-th-links (subseq (restart-data fix) 3))
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node-item-based) (cxn-supplier node))
      ;; set statuses
      (push (type-of repair) (statuses new-node-lex))
      (push (type-of repair) (statuses new-node-item-based))
      (push 'added-by-repair (statuses new-node-lex))
      (push 'added-by-repair (statuses new-node-item-based))
      ;; enqueue only second new node
      (cip-enqueue new-node-item-based (cip node) (get-configuration node :queue-mode)))))

(defun select-cxn-for-making-item-based-cxn (cxn-inventory utterance meaning)
  (loop for cxn in (constructions cxn-inventory)
        for non-overlapping-meaning-observation = (non-overlapping-meaning meaning cxn :nom-observation t)
        for non-overlapping-meaning-cxn = (non-overlapping-meaning meaning cxn :nom-cxn t)
        for overlapping-meaning-cxn = (set-difference (extract-meaning-predicates cxn) non-overlapping-meaning-cxn :test #'equal)
        for non-overlapping-form-observation = (non-overlapping-form utterance cxn :nof-observation t)
        for non-overlapping-form-cxn = (non-overlapping-form utterance cxn :nof-cxn t)
        for overlapping-form-cxn = (set-difference (extract-form-predicates cxn) non-overlapping-form-cxn :test #'equal)
        when (and (eql (phrase-type cxn) 'holophrase)
                  (or (and (= 1 (length non-overlapping-meaning-observation))
                           (= 1 (length non-overlapping-meaning-cxn)))
                      (and (= 1 (length non-overlapping-form-observation))
                           (= 1 (length non-overlapping-form-cxn)))))
        return (values non-overlapping-meaning-observation non-overlapping-meaning-cxn
                       non-overlapping-form-observation non-overlapping-form-cxn
                       overlapping-meaning-cxn overlapping-form-cxn
                       cxn)))