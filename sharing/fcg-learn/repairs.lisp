(in-package :fcg)

;;;;;;;;;;;;;
;;         ;;
;; Repairs ;;
;;         ;;
;;;;;;;;;;;;;


(defmethod repair ((repair repair-learn-holophrastic-cxn)
                   (problem gold-standard-not-in-search-space)
                   (cip construction-inventory-processor)
                   &key &allow-other-keys)
  "Learn a holophrastic construction."
  (let* ((speech-act (get-data (blackboard (construction-inventory cip)) :speech-act))
         (cxn-inventory (original-cxn-set (construction-inventory cip)))
         (fix-cxn-inventory (learn-holophrastic-cxn speech-act cxn-inventory))
         (repair-solution-node (fcg-apply (processing-cxn-inventory fix-cxn-inventory) (initial-cfs cip) (direction cip)))
         (fixed-car (cipn-car repair-solution-node)))
    (when (and fixed-car
               (gold-standard-solution-p (car-resulting-cfs fixed-car) speech-act (direction cip) (configuration cxn-inventory)))
      (make-instance 'holophrastic-fix
                     :repair repair
                     :problem problem
                     :fix-constructions (list (original-cxn (car-applied-cxn fixed-car)))
                     :fixed-cars (list fixed-car)
                     :speech-act speech-act))))


(defmethod repair ((repair repair-through-anti-unification)
                   (problem gold-standard-not-in-search-space)
                   (cip construction-inventory-processor)
                   &key &allow-other-keys)
  "Learn constructions through anti-unification."
  (let* ((speech-act (get-data (blackboard (construction-inventory cip)) :speech-act))
         (fix-cxn-inventories (learn-through-anti-unification speech-act cip)))
    (loop with fixes = nil
          for fix-cxn-inventory in fix-cxn-inventories
          do (set-configuration fix-cxn-inventory (if (eql (direction cip) '<-) :parse-goal-tests :production-goal-tests) (list :gold-standard))
             (let* ((repair-solution-node (fcg-apply (processing-cxn-inventory fix-cxn-inventory) (initial-cfs cip) (direction cip)))
                    (fixed-cars (mapcar #'cipn-car (reverse (upward-branch repair-solution-node :include-initial nil)))))
               (when (find 'succeeded (statuses repair-solution-node))
                 (push (make-instance 'anti-unification-fix
                                      :repair repair
                                      :problem problem
                                      :base-cxns (get-data (blackboard fix-cxn-inventory) :base-cxns)
                                      :fix-constructions (constructions-list fix-cxn-inventory)
                                      :fix-categories (categories fix-cxn-inventory)
                                      :fix-categorial-links (links fix-cxn-inventory)
                                      :fixed-cars fixed-cars
                                      :speech-act speech-act)
                       fixes)))
          finally (return fixes))))


(defmethod repair ((repair repair-add-categorial-link)
                   (problem gold-standard-not-in-search-space)
                   (cip construction-inventory-processor)
                   &key &allow-other-keys)
  "Learn by adding a new categorial link."
  (let* ((cxn-inventory (original-cxn-set (construction-inventory cip)))
         (category-linking-mode (get-configuration cxn-inventory :category-linking-mode))
         (cxn-supplier-mode (get-configuration cxn-inventory :cxn-supplier-mode))
         (goal-tests (get-configuration cxn-inventory (if (eql (direction cip) '<-) :parse-goal-tests :production-goal-tests)))
         (nodes-w-empty-root (remove nil (traverse-depth-first cip :collect-fn #'(lambda (node)
                                                                                    (unless (or (find 'duplicate (statuses node))
                                                                                                (not (empty-root-p node)))
                                                                                      node))))))
    
    ;; Change configurations of cip in order to explore continuations where categories are not yet linked
    (set-configuration cxn-inventory :cxn-supplier-mode :linking-cxns-only)
    (set-configuration cxn-inventory :category-linking-mode :categories-exist)
    (set-configuration cxn-inventory (if (eql (direction cip) '<-) :parse-goal-tests :production-goal-tests) (list :gold-standard))

    ;; Created fixes
    (loop for node-w-empty-root in nodes-w-empty-root
          for (repair-node new-cip) = (multiple-value-list (fcg-apply (processing-cxn-inventory cxn-inventory)
                                                                      (car-resulting-cfs (cipn-car node-w-empty-root))
                                                                      (direction cip)))
          when (find 'succeeded (statuses repair-node))
            do (annotate-cip-with-used-categorial-links new-cip)
            and
            collect (make-instance 'categorial-link-fix
                                   :repair repair
                                   :problem problem
                                   :fix-categorial-links (remove-duplicates (loop for (cat-1 cat-2 link-type) in (mappend #'(lambda (node)
                                                                                                                      (get-data node :used-categorial-links))
                                                                                                                  (upward-branch repair-node :include-initial nil))
                                                                                  unless (link-exists-p cat-1 cat-2 cxn-inventory)
                                                                                    collect (list cat-1 cat-2 link-type))
                                                                            :test #'equal)
                                   :fixed-cars (append (mapcar #'cipn-car (reverse (upward-branch node-w-empty-root :include-initial nil)))
                                                       (mapcar #'cipn-car (reverse (upward-branch repair-node :include-initial nil))))
                                   :speech-act (get-data (blackboard (construction-inventory cip)) :speech-act))
          finally (set-configuration cxn-inventory :category-linking-mode category-linking-mode)
                  (set-configuration cxn-inventory :cxn-supplier-mode cxn-supplier-mode)
                  (set-configuration cxn-inventory (if (eql (direction cip) '<-) :parse-goal-tests :production-goal-tests) goal-tests))))
