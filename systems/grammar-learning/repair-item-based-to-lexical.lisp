(in-package :grammar-learning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repair Add lexical construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass item-based->lexical (repair) 
  ((trigger :initform 'fcg::new-node)))
  
(defmethod repair ((repair item-based->lexical)
                   (problem non-gold-standard-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new lexical construction."
  (when (initial-node-p node)
    (let ((lex-cxn-and-th-link (create-lexical-cxn problem node)))
      (when lex-cxn-and-th-link
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data lex-cxn-and-th-link)))))
  
(defmethod repair ((repair item-based->lexical)
                   (problem non-gold-standard-utterance)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new lexical construction."
  (when (initial-node-p node)
    (let ((lex-cxn-and-th-link (create-lexical-cxn problem node)))
      (when lex-cxn-and-th-link 
        (make-instance 'fcg::cxn-fix
                       :repair repair
                       :problem problem
                       :restart-data lex-cxn-and-th-link)))))

(defun create-lexical-cxn (problem node)
  "Creates a lexical cxn if a surrounding item-based cxn with one empty slot exists."
  (with-disabled-monitor-notifications
    (let* ((processing-cxn-inventory (construction-inventory node))
           (original-cxn-inventory (original-cxn-set processing-cxn-inventory))
           
           (resulting-cars (loop for cxn in (constructions processing-cxn-inventory)
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
    ;; TODO: rewrite this logic: there can be more than one matching lex cxn without th links and it could still apply, so there can be no new lex cxn and still make the th links --> is this for add-th-links? 
    ;; there is more than one string in root, but there can be a matching lex cxn with missing th links that can be subtracted
    (when (and (> (length string-predicates-in-root) 0)
               item-based-cxn)
      (let* (
             (matching-lex-cxns (find-matching-lex-cxns-in-root original-cxn-inventory string-predicates-in-root)))
        ;; there are one or more lex cxns, and one remaining string in root
        (when (or (and matching-lex-cxns
                       (= 1 (- (length string-predicates-in-root) (length matching-lex-cxns))))
                  (= (length string-predicates-in-root) 1))
          ;; construct the remaining cxn first
          (let* ((utterance (random-elt (get-data problem :utterances)))
                 (type-hierarchy (get-type-hierarchy original-cxn-inventory))
                 (meaning-predicates-gold (meaning-predicates-with-variables (first (get-data problem :meanings))))
                 (meaning-predicates-gold-minus-lex (subtract-lex-cxn-meanings matching-lex-cxns meaning-predicates-gold))
                 (meaning-predicates-observed (extract-meanings observation))
                 (meaning-predicates-lex-cxn (if (= 1 (length string-predicates-in-root))
                                               (set-difference meaning-predicates-gold meaning-predicates-observed :test #'unify)
                                               (set-difference meaning-predicates-gold-minus-lex meaning-predicates-observed :test #'unify)))
                 (form-predicates-lex-cxn (if (= 1 (length string-predicates-in-root))
                                            string-predicates-in-root
                                            (subtract-lex-cxn-forms matching-lex-cxns string-predicates-in-root)))
                 (existing-lex-cxn (find-cxn-by-form-and-meaning form-predicates-lex-cxn meaning-predicates-lex-cxn original-cxn-inventory))
                 (cxn-name (make-cxn-name (third (first form-predicates-lex-cxn)) original-cxn-inventory))
                 (unit-name (second (first form-predicates-lex-cxn)))
                 (lex-class (if existing-lex-cxn
                              (lex-class-cxn existing-lex-cxn)
                              (intern (get-base-name unit-name) :type-hierarchies)))
                 (args (mapcar #'third meaning-predicates-lex-cxn))
                 (new-lex-cxn (or existing-lex-cxn (second (multiple-value-list (eval
                                                                                 `(def-fcg-cxn ,cxn-name
                                                                                               ((,unit-name
                                                                                                 (syn-cat (phrase-type lexical)
                                                                                                          (lex-class ,lex-class))
                                                                                                 (args ,args))
                                                                                                <-
                                                                                                (,unit-name
                                                                                                 (HASH meaning ,meaning-predicates-lex-cxn)
                                                                                                 --
                                                                                                 (HASH form ,form-predicates-lex-cxn)))
                                                                                               :attributes (:cxn-type lexical
                                                                                                            :repair item-based->lexical)
                                                                                               :cxn-inventory ,(copy-object original-cxn-inventory)))))))
                 ;; make a list of all cxns, sort them
                 (applied-lex-cxns (filter-by-phrase-type 'lexical (applied-constructions node)))
                 (lex-cxns (sort-cxns-by-form-string (append
                                                      (list new-lex-cxn)
                                                      matching-lex-cxns
                                                      applied-lex-cxns) utterance))
                 (lex-classes-lex-cxns (when lex-cxns (map 'list #'lex-class-cxn lex-cxns)))
                 (lex-classes-item-based-units (when item-based-cxn (get-all-unit-lex-classes item-based-cxn)))
                 ;; assign all th links
                 (th-links (when (and lex-classes-lex-cxns
                                      lex-classes-item-based-units
                                      (= (length lex-classes-lex-cxns) (length lex-classes-item-based-units)))
                             (create-new-th-links lex-classes-lex-cxns lex-classes-item-based-units type-hierarchy))))
            ;; return
            (when th-links
              (list new-lex-cxn (append (list (get-processing-cxn item-based-cxn))
                                        (list (get-processing-cxn new-lex-cxn))
                                        (unless (= 1 (length string-predicates-in-root))
                                          (map 'list #'get-processing-cxn matching-lex-cxns))
                                        (map 'list #'get-processing-cxn applied-lex-cxns)) th-links)))))))))

(defmethod handle-fix ((fix fcg::cxn-fix) (repair item-based->lexical) (problem problem) (node cip-node) &key &allow-other-keys)
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((new-lex-cxn (first (restart-data fix)))
           (cxns (second (restart-data fix)))
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
           (last-node  (initial-node node))
           (applied-nodes (loop for cxn in cxns
                                do (setf last-node (fcg::cip-add-child last-node (first (fcg-apply cxn (if (initial-node-p last-node)
                                                                                                         (car-source-cfs (cipn-car last-node))
                                                                                                         (car-resulting-cfs (cipn-car last-node)))
                                                                                                   (direction (cip node))
                                                                                                   :configuration (configuration (construction-inventory node))
                                                                                                   :cxn-inventory (construction-inventory node)))))
                                collect last-node)))
      ;; ignore
      ;; Reset type hierarchy
      (set-type-hierarchy (construction-inventory node) orig-type-hierarchy)
      ;; Add cxns to blackboard of last new node
      (set-data (car-resulting-cfs (cipn-car last-node)) :fix-cxns (list new-lex-cxn))
      (set-data (car-resulting-cfs (cipn-car last-node)) :fix-th-links th-flat-list)
      ;; set cxn-supplier to last new node
      (setf (cxn-supplier last-node) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses last-node))
      (push 'added-by-repair (statuses last-node))
      ;; enqueue only last new node; never backtrack over the first applied construction, we applied them as a block
      (cip-enqueue last-node (cip node) (get-configuration node :queue-mode)))))
