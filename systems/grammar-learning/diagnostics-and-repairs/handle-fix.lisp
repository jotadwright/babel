(in-package :grammar-learning)

;; Abstract repair class
(defclass add-cxns-and-categorial-links (repair)
  ())
(define-event cxns-learned (cxns list))


;; Generic handle fix for grammar learning
(defmethod handle-fix ((fix fcg::cxn-fix) (repair add-cxns-and-categorial-links) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the constructions provided by the fix to the result of the node and return the construction-application-result"
  ;; generic handle fix, which takes the following positional args in a list:
  ;; cxns-to-apply (list of original cxns) applied in the order they appear in!
  ;; categorial links (list)
  ;; original-cxns-to-consolidate (list) ! exclude existing!

  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (let* ((processing-cxns-to-apply (mapcar #'get-processing-cxn (first (restart-data fix))))
         (categorial-links (second (restart-data fix)))
         (original-cxns-to-consolidate (third (restart-data fix)))
         (categories-to-add (fourth (restart-data fix)))
         (gold-standard-consulted-p (sixth (restart-data fix)))
         ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
         (orig-categorial-network (categorial-network (construction-inventory node)))
         (temp-categorial-network (copy-object (categorial-network (construction-inventory node))))
         (cats (add-categories categories-to-add temp-categorial-network :recompute-transitive-closure nil))
         (cat-links (loop for categorial-link in categorial-links
                          do (add-categories (list (car categorial-link) (cdr categorial-link)) temp-categorial-network :recompute-transitive-closure nil)
                          (add-link (car categorial-link) (cdr categorial-link) temp-categorial-network :recompute-transitive-closure nil)
                          finally (set-categorial-network (construction-inventory node) temp-categorial-network)))
         (learned-cxns (remove-if-not #'(lambda (cxn) (and (eql (attr-val cxn :label) 'fcg::routine)
                                                           (equal 'SINGLE-FLOAT (type-of (cdr (first (attributes cxn)))))))
                                      (append (mapcar #'original-cxn processing-cxns-to-apply) original-cxns-to-consolidate)))
         (sandbox-solution (apply-in-sandbox (initial-node node)
                                          (original-cxn-set (construction-inventory node))
                                          :cxns-to-add (mapcar #'original-cxn processing-cxns-to-apply)
                                          :categories-to-add categories-to-add
                                          :categorial-links-to-add categorial-links))
         (solution-node (loop with current-node = (initial-node node)
                              for node-to-add in (append (ignore-initial-nodes (reverse (all-parents sandbox-solution))) (list sandbox-solution))
                                               do (setf current-node (fcg::cip-add-child current-node (cipn-car node-to-add)))
                                               finally (return current-node))))
    
    ;; ignore
    (declare (ignore cat-links cats))
    
    
    (when learned-cxns
      (loop for cxn in learned-cxns
            do (setf (attr-val cxn :learned-at) (format nil "@~a" (get-data (blackboard (construction-inventory node)) :current-interaction-nr))))
      (notify cxns-learned learned-cxns))
    ;; Reset categorial network
    (set-categorial-network (construction-inventory node) orig-categorial-network)
    ;; Add cxns to blackboard of second new node
    (set-data (car-resulting-cfs  (cipn-car solution-node)) :fix-cxns original-cxns-to-consolidate)
    (set-data (car-resulting-cfs  (cipn-car solution-node)) :fix-categorial-links categorial-links)
    (set-data (car-resulting-cfs  (cipn-car solution-node)) :fix-categories categories-to-add)
    ;; set cxn-supplier to second new node
    (setf (cxn-supplier solution-node) (cxn-supplier node))
    ;; set statuses (colors in web interface)
    (when gold-standard-consulted-p
      (push 'gold-standard-consulted (statuses solution-node)))
    (push (type-of repair) (statuses solution-node))
    (push 'added-by-repair (statuses solution-node))
    ;; enqueue only second new node; never backtrack over the first applied holistic construction, we applied them as a block
    (cip-enqueue solution-node (cip node) (get-configuration node :queue-mode))))
      

