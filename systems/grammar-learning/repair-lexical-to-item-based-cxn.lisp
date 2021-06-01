(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair from lexical to item-based cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass repair-lexical->item-based-cxn (repair) 
  ((trigger :initform 'fcg::new-node)))

(defmethod repair ((repair repair-lexical->item-based-cxn)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn-from-lex problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))

(defmethod repair ((repair repair-lexical->item-based-cxn)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new item-based construction."
  (when (initial-node-p node)
    (let ((constructions-and-th-links (create-item-based-cxn-from-lex problem node)))
      (when constructions-and-th-links
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data constructions-and-th-links)))))


(defun create-item-based-cxn-from-lex (problem node)
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
             (var-form (form-constraints-with-variables utterance (get-configuration (cxn-inventory (first matching-lex-cxns)) :de-render-mode)))
             (subunit-names-and-non-overlapping-form (multiple-value-list (diff-non-overlapping-form var-form matching-lex-cxns)))
             (subunit-names (first subunit-names-and-non-overlapping-form))
             (non-overlapping-form (second subunit-names-and-non-overlapping-form))
             (args-and-non-overlapping-meaning (multiple-value-list (diff-non-overlapping-meaning gold-standard-meaning matching-lex-cxns)))
             (args (first args-and-non-overlapping-meaning))
             (non-overlapping-meaning (second args-and-non-overlapping-meaning))
             (cxn-name-item-based-cxn (make-cxn-name non-overlapping-form cxn-inventory :add-cxn-suffix nil))
             (rendered-cxn-name-list (make-cxn-placeholder-name non-overlapping-form cxn-inventory))
             (placeholder-list (extract-placeholder-var-list rendered-cxn-name-list))
             (th-links (create-type-hierarchy-links matching-lex-cxns (format nil "~{~a~^-~}" rendered-cxn-name-list) placeholder-list))
             (lex-cxn-subunit-blocks (subunit-block-for-lex-cxns matching-lex-cxns subunit-names args th-links))
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
                                                                         :attributes (:cxn-type item-based
                                                                                      :repair lexical->item-based)
                                                                                      
                                                                         :cxn-inventory ,(copy-object cxn-inventory)))))))
        (list item-based-cxn matching-lex-cxns th-links)))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair repair-lexical->item-based-cxn) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((item-based-cxn (get-processing-cxn (first (restart-data fix))))
           (lex-cxns (map 'list #'get-processing-cxn (second (restart-data fix))))
           (th-links (third (restart-data fix)))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-type-hierarchy (get-type-hierarchy (construction-inventory node)))
           (temp-type-hierarchy (copy-object (get-type-hierarchy (construction-inventory node))))
           (th-flat-list nil)
           (th (loop for th-link in th-links
                     do (add-categories (list (car th-link) (cdr th-link)) temp-type-hierarchy)
                     (add-link (car th-link) (cdr th-link) temp-type-hierarchy :weight 0.5)
                     (setf th-flat-list (append th-flat-list (list th-link)))
                     finally (set-type-hierarchy (construction-inventory node) temp-type-hierarchy)))
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
      (set-type-hierarchy (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-cxns (append (second (restart-data fix)) (list (original-cxn item-based-cxn))))
      (set-data (car-resulting-cfs  (cipn-car new-node-item-based)) :fix-th-links th-flat-list)
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier new-node-item-based) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses new-node-item-based))
      (push 'added-by-repair (statuses new-node-item-based))
      ;; enqueue only second new node; never backtrack over the first applied lexical construction, we applied them as a block
      (cip-enqueue new-node-item-based (cip node) (get-configuration node :queue-mode)))))
