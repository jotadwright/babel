(in-package :grammar-learning)

;; Abstract repair class
(defclass add-cxns-and-categorial-links (repair)
  ())

;; Generic handle fix for grammar learning
(defmethod handle-fix ((fix fcg::cxn-fix) (repair add-cxns-and-categorial-links) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Apply the constructions provided by the fix to the result of the node and return the construction-application-result"
  ;; generic handle fix, which takes the following positional args in a list:
  ;; cxns-to-apply (list) applied in the order they appear in!
  ;; categorial links (list)
  ;; original-cxns-to-consolidate (list) ! exclude existing!

  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (let* ((processing-cxns-to-apply (map 'list #'get-processing-cxn (first (restart-data fix))))
           (original-cxns-to-consolidate (third (restart-data fix)))
           (categorial-links (second (restart-data fix)))
           ;; temporarily store the original type hierarchy, copy it and add the links, and set it to the cxn-inventory
           (orig-categorial-network (categorial-network (construction-inventory node)))
           (temp-categorial-network (copy-object (categorial-network (construction-inventory node))))
           (cat-links (loop for categorial-link in categorial-links
                            do (add-categories (list (car categorial-link) (cdr categorial-link)) temp-categorial-network :recompute-transitive-closure nil)
                            (add-link (car categorial-link) (cdr categorial-link) temp-categorial-network :recompute-transitive-closure nil)
                            finally (set-categorial-network (construction-inventory node) temp-categorial-network)))
           (applied-nodes (loop with last-node = (initial-node node)
                                for cxn in processing-cxns-to-apply
                                do (setf last-node (fcg::cip-add-child last-node (first (fcg-apply cxn (if (initial-node-p last-node)
                                                                                                         (car-source-cfs (cipn-car (initial-node last-node)))
                                                                                                         (car-resulting-cfs (cipn-car last-node)))
                                                                                                   (direction (cip node))
                                                                                                   :configuration (configuration (construction-inventory node))
                                                                                                   :cxn-inventory (construction-inventory node)))))
                                collect last-node))
           (last-applied-node (last-elt applied-nodes)))
      ;; ignore
      (declare (ignore cat-links))
      ;; Reset categorial network
      (set-categorial-network (construction-inventory node) orig-categorial-network)
      ;; Add cxns to blackboard of second new node
      (set-data (car-resulting-cfs  (cipn-car last-applied-node)) :fix-cxns original-cxns-to-consolidate)
      (set-data (car-resulting-cfs  (cipn-car last-applied-node)) :fix-categorial-links categorial-links)
      ;; set cxn-supplier to second new node
      (setf (cxn-supplier last-applied-node) (cxn-supplier node))
      ;; set statuses (colors in web interface)
      (push (type-of repair) (statuses last-applied-node))
      (push 'added-by-repair (statuses last-applied-node))
      ;; enqueue only second new node; never backtrack over the first applied holistic construction, we applied them as a block
      (cip-enqueue last-applied-node (cip node) (get-configuration node :queue-mode)))))