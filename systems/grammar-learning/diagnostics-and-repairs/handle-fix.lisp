(in-package :grammar-learning)

;; Abstract repair class
(defclass add-cxns-and-categorial-links (repair)
  ())
(define-event cxns-learned (cxns list))

(define-event fix-applied (repair-name symbol) (form list) (learned-cxns list))

(defun apply-fix (form
                  cxns-to-apply
                  categorial-links
                  original-cxns-to-consolidate
                  categories-to-add
                  top-level-category
                  gold-standard-consulted-p
                  node
                  repair-name)
  "Apply the learned cxns and links, return the solution node."
  (let ((learned-cxns (remove-if-not #'(lambda (cxn) (and (eql (attr-val cxn :label) 'fcg::routine)
                                                            (not (attr-val cxn :learned-at))))
                                           (append cxns-to-apply original-cxns-to-consolidate))))
  (if node
    (let* ((processing-cxns-to-apply (mapcar #'get-processing-cxn cxns-to-apply))
           (orig-categorial-network (categorial-network (construction-inventory node)))
           (temp-categorial-network (copy-object (categorial-network (construction-inventory node))))
           (cats (add-categories categories-to-add temp-categorial-network :recompute-transitive-closure nil))
           (cat-links (loop for categorial-link in categorial-links
                            do (add-categories (list (car categorial-link) (cdr categorial-link)) temp-categorial-network :recompute-transitive-closure nil)
                            (add-link (car categorial-link) (cdr categorial-link) temp-categorial-network :recompute-transitive-closure nil)
                            finally (set-categorial-network (construction-inventory node) temp-categorial-network)))
         
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
      ;; reset categorial network
      (set-categorial-network (construction-inventory node) orig-categorial-network)
    
      (when (find 'SUCCEEDED (statuses sandbox-solution) :test #'string=)
        (notify fix-applied repair-name form learned-cxns)
        (list
         cxns-to-apply
         categorial-links
         original-cxns-to-consolidate
         categories-to-add
         top-level-category
         gold-standard-consulted-p
         node
         solution-node
         )))
    ;; node is nil (we are in a deeper level of the recursion) just pass through the list of cxns and cats
    ;; todo: apply cxns to form instead
    (progn
      (notify fix-applied repair-name form learned-cxns)
      (list
     cxns-to-apply
     categorial-links
     original-cxns-to-consolidate
     categories-to-add
     top-level-category
     gold-standard-consulted-p
     node
     nil)))))
       
    



;; Generic handle fix for grammar learning
(defmethod handle-fix ((fix fcg::cxn-fix) (repair add-cxns-and-categorial-links) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Enqueue the solution node."
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (destructuring-bind (cxns-to-apply
                       categorial-links
                       original-cxns-to-consolidate
                       categories-to-add
                       top-cat
                       gold-standard-consulted-p
                       original-node
                       solution-node)
      (restart-data fix)
    (declare (ignore top-cat original-node))
    (let ((learned-cxns (remove-if-not #'(lambda (cxn) (and (eql (attr-val cxn :label) 'fcg::routine)
                                                            (not (attr-val cxn :learned-at))))
                                           (append cxns-to-apply original-cxns-to-consolidate))))
      (when learned-cxns
        (loop for cxn in learned-cxns
              for interaction-nr = (find-data (blackboard (construction-inventory node)) :current-interaction-nr)
              ;;when interaction-nr
              do (setf (attr-val cxn :learned-at) (format nil "@~a" interaction-nr)))
        (notify cxns-learned learned-cxns))
   
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
      (cip-enqueue solution-node (cip node) (get-configuration node :queue-mode)))))
      

