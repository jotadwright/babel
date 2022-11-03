(in-package :intention-reading)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair repair-item-based+item-based->item-based-cxn                                                               ;;
;;                                                                                                                   ;;
;; Example: what is the color of the cube + cube  +  what is the size of the B => what is the X of the Y             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defmethod repair ((repair repair-item-based+item-based->item-based-cxn)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn-from-item-based-cxn problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defmethod repair ((repair repair-item-based+item-based->item-based-cxn)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn-from-item-based-cxn problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))


(defun create-item-based-cxn-from-item-based-cxn (problem node)
  "Creates item-based construction and lexical constructions
based on existing construction with sufficient overlap."
  (let* ((cxn-inventory (original-cxn-set (construction-inventory node)))
         (utterance (random-elt (get-data problem :utterances)))
         (gold-standard-meaning (meaning-predicates-with-variables (random-elt (get-data problem :meanings))))
         (observed-form (extract-forms (left-pole-structure (car-source-cfs (cipn-car (initial-node node))))))
         (matching-lex-cxns (find-matching-lex-cxns cxn-inventory observed-form gold-standard-meaning utterance)))
         
    ;; we need at least one matching lex cxn
    (when (< 0 (length matching-lex-cxns))
      (let* (
             (args-and-non-overlapping-meaning (multiple-value-list (diff-non-overlapping-meaning gold-standard-meaning matching-lex-cxns)))
             (args (first args-and-non-overlapping-meaning))
             (non-overlapping-meaning (second args-and-non-overlapping-meaning))
             (var-form (form-constraints-with-variables utterance (get-configuration (cxn-inventory (first matching-lex-cxns)) :de-render-mode))))
        (multiple-value-bind (subunit-names
                              non-overlapping-form)
            (diff-non-overlapping-form var-form matching-lex-cxns)
          (let ((lex-cxns-lex-classes (map 'list #'lex-class-cxn matching-lex-cxns)))
            (multiple-value-bind (non-overlapping-meaning-observation
                                  non-overlapping-meaning-cxn
                                  non-overlapping-form-observation
                                  non-overlapping-form-cxn
                                  overlapping-meaning-cxn
                                  overlapping-form-cxn
                                  overlapping-meaning-observation
                                  overlapping-form-observation
                                  cxn)
                (select-cxn-for-making-item-based-cxn-from-item-based-cxns non-overlapping-meaning non-overlapping-form lex-cxns-lex-classes cxn-inventory)
              (when cxn
                (let* ((cxn-name-item-based-cxn (make-cxn-name overlapping-form-observation cxn-inventory :add-cxn-suffix nil))
                       (rendered-cxn-name-list (make-cxn-placeholder-name overlapping-form-observation cxn-inventory))
                       (placeholder-list (extract-placeholder-var-list rendered-cxn-name-list))
                       (lex-cxn-1 (find-cxn-by-form-and-meaning non-overlapping-form-observation non-overlapping-meaning-observation cxn-inventory))
                       (lex-cxn-2 (find-cxn-by-form-and-meaning non-overlapping-form-cxn non-overlapping-meaning-cxn cxn-inventory))
                       ;; unit names
                       (unit-name-lex-cxn-1 (second (find 'string non-overlapping-form-observation :key #'first)))
                       (unit-name-lex-cxn-2 (second (find 'string non-overlapping-form-cxn :key #'first)))
                       ;; args and syn-cat
                       (lex-class-lex-cxn-1 (if lex-cxn-1
                                              (lex-class-cxn lex-cxn-1)
                                              (intern (symbol-name (make-const unit-name-lex-cxn-1)) :type-hierarchies)))
                       (lex-class-lex-cxn-2 (if lex-cxn-2
                                              (lex-class-cxn lex-cxn-2)
                                              (intern (symbol-name (make-const unit-name-lex-cxn-2)) :type-hierarchies)))
                      
                       ;; Args
                       (args-lex-cxn-1 (third (first non-overlapping-meaning-observation))) ;; third if bind
                       (args-lex-cxn-2 (third (first non-overlapping-meaning-cxn))) ;; third if bind
               
                       ;; CXNs
               
                       (new-lex-cxn-1 (or lex-cxn-1 (second (multiple-value-list (eval
                                                                                  `(def-fcg-cxn ,(make-cxn-name non-overlapping-form-observation cxn-inventory)
                                                                                                ((,unit-name-lex-cxn-1
                                                                                                  (args (,args-lex-cxn-1))
                                                                                                  (syn-cat (phrase-type lexical)
                                                                                                           (lex-class ,lex-class-lex-cxn-1)))
                                                                                                 <-
                                                                                                 (,unit-name-lex-cxn-1
                                                                                                  (HASH meaning ,non-overlapping-meaning-observation)
                                                                                                  --
                                                                                                  (HASH form ,non-overlapping-form-observation)))
                                                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
                       (new-lex-cxn-2 (or lex-cxn-2 (second (multiple-value-list (eval
                                                                                  `(def-fcg-cxn ,(make-cxn-name non-overlapping-form-cxn cxn-inventory)
                                                                                                ((,unit-name-lex-cxn-2
                                                                                                  (args (,args-lex-cxn-2))
                                                                                                  (syn-cat (phrase-type lexical)
                                                                                                           (lex-class ,lex-class-lex-cxn-2)))
                                                                                                 <-
                                                                                                 (,unit-name-lex-cxn-2
                                                                                                  (HASH meaning ,non-overlapping-meaning-cxn)
                                                                                                  --
                                                                                                  (HASH form ,non-overlapping-form-cxn)))
                                                                                                :cxn-inventory ,(copy-object cxn-inventory)))))))
                       ;; add learned cxn to matching cxns to apply and resort
                       (matching-lex-cxns (sort (append matching-lex-cxns (list new-lex-cxn-1))
                                                #'(lambda (x y)
                                                    (<
                                                     (search (third (first (extract-form-predicates x))) utterance)
                                                     (search (third (first (extract-form-predicates y))) utterance)))))
                       ;; combine args
                       (args (append args (list args-lex-cxn-1)))
                       (non-overlapping-meaning (set-difference non-overlapping-meaning non-overlapping-meaning-observation :test #'equal))
                       (non-overlapping-form (set-difference non-overlapping-form non-overlapping-form-observation :test #'equal))
                       (th-links (create-type-hierarchy-links matching-lex-cxns (format nil "~{~a~^-~}" rendered-cxn-name-list) placeholder-list))
                       (subunit-names (diff-non-overlapping-form var-form matching-lex-cxns))
                       (lex-cxn-subunit-blocks (subunit-block-for-lex-cxns matching-lex-cxns subunit-names args th-links))
                       ;; add the th links for the second learned cxn
                       (lex-class-learned-lex-cxn-slot (loop for link in th-links
                                                             for key = (car (first link))
                                                             for value = (cdr (first link))
                                                             when (eq key lex-class-lex-cxn-1)
                                                             return value))
                       (th-links (append th-links (list (list (cons lex-class-lex-cxn-2 lex-class-learned-lex-cxn-slot) (cons lex-class-learned-lex-cxn-slot lex-class-lex-cxn-2)))))
                       (item-based-cxn (second (multiple-value-list (eval
                                                                     `(def-fcg-cxn ,(add-cxn-suffix cxn-name-item-based-cxn)
                                                                                   ((?item-based-unit
                                                                                     (syn-cat (phrase-type item-based))
                                                                                     (subunits ,subunit-names))
                                                                                    ,@lex-cxn-subunit-blocks
                                                                                    <-
                                                                                    (?item-based-unit
                                                                                     (HASH meaning ,non-overlapping-meaning)
                                                                                     --
                                                                                     (HASH form ,non-overlapping-form)))
                                                                                   :cxn-inventory ,(copy-object cxn-inventory)))))))
                       
                  (list item-based-cxn matching-lex-cxns th-links new-lex-cxn-2))))))))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair repair-item-based+item-based->item-based-cxn) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((item-based-cxn (get-processing-cxn (first (restart-data fix))))
           (lex-cxns (map 'list #'get-processing-cxn (second (restart-data fix))))
           (th-links (third (restart-data fix)))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (categorial-network (construction-inventory node)))
           (temp-type-hierarchy (copy-object (categorial-network (construction-inventory node))))
           (th-flat-list (list))
           (th (loop for th-link in th-links
                     do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy)
                     (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5)
                     (setf th-flat-list (append th-flat-list (list th-link)))
                     finally (set-categorial-network (construction-inventory node) temp-type-hierarchy)))
           (lex-nodes (loop for lex-cxn in lex-cxns
                            with last-node = (initial-node node)
                            do (setf last-node (fcg::cip-add-child last-node (first (fcg-apply lex-cxn (if (initial-node-p last-node)
                                                                                                         (car-source-cfs (cipn-car last-node))
                                                                                                         (car-resulting-cfs (cipn-car last-node)))
                                                                                               (direction (cip node))
                                                                                               :configuration (configuration (construction-inventory node))
                                                                                               :cxn-inventory (construction-inventory node)))))
                            collect last-node))
          
           (new-node-item-based (fcg::cip-add-child (last-elt lex-nodes) (first (fcg-apply item-based-cxn (car-resulting-cfs (cipn-car (last-elt lex-nodes))) (direction (cip node))
                                                                                           :configuration (configuration (construction-inventory node))
                                                                                           :cxn-inventory (construction-inventory node))))))
      ;; ignore
      ;; Reset type hierarchy
      (set-categorial-network (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-cxns (list (original-cxn item-based-cxn) (last-elt (restart-data fix))))
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-th-links th-flat-list)
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node-item-based) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node-item-based))
      (push 'added-by-repair (statuses new-node-item-based))
      ;; enqueue only second new node; never backtrack over the first applied lexical construction, we applied them as a block
      (cip-enqueue new-node-item-based (cip node) (get-configuration node :queue-mode)))))

(defun extract-lex-classes-for-args (cxn)
  (loop for contributing-unit in (contributing-part cxn)
        for unit-structure = (fcg::unit-structure contributing-unit)
        when (equal (first (first unit-structure)) 'grammar-learning::args)
        collect (second (second (second unit-structure)))
  ))

(defun type-hierarchies-connected-p (slot-lex-classes lex-cxns-lex-classes cxn-inventory)
  (loop with th = (categorial-network cxn-inventory)
   for slot-lex-class in slot-lex-classes
        for lex-cxn-lex-class in lex-cxns-lex-classes
        always (type-hierarchies::connected-p lex-cxn-lex-class slot-lex-class th)))

(defun select-cxn-for-making-item-based-cxn-from-item-based-cxns (non-overlapping-meaning-observation non-overlapping-form-observation lex-cxns-lex-classes cxn-inventory)
  (loop for cxn in (constructions cxn-inventory)
        do (when (eql (phrase-type cxn) 'item-based)
             (let ((slot-lex-classes (extract-lex-classes-for-args cxn)))
               (when (type-hierarchies-connected-p slot-lex-classes lex-cxns-lex-classes cxn-inventory)
                 (let* ((item-based-form (extract-form-predicates cxn))
                        (item-based-meaning (extract-meaning-predicates cxn)))
                   (multiple-value-bind (non-overlapping-lex-form-cxn non-overlapping-lex-form-observation) (non-overlapping-predicates item-based-form non-overlapping-form-observation)
                     (multiple-value-bind (non-overlapping-lex-meaning-cxn non-overlapping-lex-meaning-observation) (non-overlapping-predicates item-based-meaning non-overlapping-meaning-observation)
                       ;; these two are wrong: they need to be from the observation, but are not currently!
                       (let ((overlapping-form-cxn (set-difference item-based-form non-overlapping-lex-form-cxn :test #'equal))
                             (overlapping-meaning-cxn (set-difference item-based-meaning non-overlapping-lex-meaning-cxn :test #'equal))
                             (overlapping-form-observation (set-difference non-overlapping-form-observation non-overlapping-lex-form-observation :test #'equal))
                             (overlapping-meaning-observation (set-difference non-overlapping-meaning-observation non-overlapping-lex-meaning-observation :test #'equal)))
                       (when (and
                              (= 1 (length non-overlapping-lex-meaning-observation))
                              (= 1 (length non-overlapping-lex-meaning-cxn))
                              (= 1 (length non-overlapping-lex-form-observation))
                              (= 1 (length non-overlapping-lex-form-cxn))
                              (= (length overlapping-form-cxn) (length overlapping-form-observation))
                              (= (length overlapping-meaning-cxn) (length overlapping-meaning-observation)))
                         (return (values non-overlapping-lex-meaning-observation non-overlapping-lex-meaning-cxn
                                         non-overlapping-lex-form-observation non-overlapping-lex-form-cxn
                                         overlapping-meaning-cxn overlapping-form-cxn
                                         overlapping-meaning-observation overlapping-form-observation
                                         cxn))))))))))))



;; (test-item-based+item-based->item-based-cxn)
;; (defparameter *item-based-cxn* (find-first-item-based-cxn *fcg-constructions*))
;; *item-based-cxn*
;; (select-cxn-for-making-item-based-cxn-from-item-based-cxns *item-based-cxn* *fcg-constructions*)
;; (type-hierarchies::connected-p 'TYPE-HIERARCHIES::THE-?X-CUBE-6 'TYPE-HIERARCHIES::THE-?X-SPHERE-6 (categorial-network *fcg-constructions*))