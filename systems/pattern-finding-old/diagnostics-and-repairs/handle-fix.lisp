(in-package :pattern-finding-old)

;; Abstract repair class
(defclass add-cxns-and-categorial-links (repair)
  ())

;; Generic function for repair
(defgeneric do-repair (observation-form observation-meaning form-args meaning-args cxn-inventory node repair-type)
  (:documentation "Run a repair of type repair-type."))

;; Events
(define-event cxns-learned (cxns list))
(define-event fix-applied (repair-name symbol) (form list) (learned-cxns list))


;; Apply fix result
(defstruct (apply-fix-result (:conc-name afr-))
  (orig-cxns-to-apply nil)
  (categorial-links nil)
  (orig-cxns-to-consolidate nil)
  (categories-to-add nil)
  (top-lvl-category nil)
  (gold-standard-consulted-p nil)
  (node nil)
  (solution-node nil)
  (repair nil))


(defun apply-fix (form-constraints
                  cxns-to-apply
                  categorial-links
                  original-cxns-to-consolidate
                  categories-to-add
                  top-level-category
                  gold-standard-consulted-p
                  node
                  repair-name)
  "Apply the learned cxns and links, return the solution node."
  (assert (notany #'null categorial-links))
  (assert (notany #'null categories-to-add))
  (let ((learned-cxns
         (remove-if-not #'(lambda (cxn) (and (eql (attr-val cxn :label) 'fcg::routine)
                                             (not (attr-val cxn :learned-at))))
                        (append cxns-to-apply original-cxns-to-consolidate))))
    (if node
      (let* ((orig-categorial-network (categorial-network (construction-inventory node)))
             (temp-categorial-network (copy-object (categorial-network (construction-inventory node)))))
        (add-categories categories-to-add temp-categorial-network :recompute-transitive-closure nil)
        (loop for categorial-link in categorial-links
              do (add-categories (list (car categorial-link) (cdr categorial-link))
                                 temp-categorial-network :recompute-transitive-closure nil)
                 (add-link (car categorial-link) (cdr categorial-link)
                           temp-categorial-network :recompute-transitive-closure nil)
              finally (set-categorial-network (construction-inventory node) temp-categorial-network))
        (let* ((processing-cxns-to-apply (mapcar #'get-processing-cxn cxns-to-apply))
               (sandbox-solution
                (fcg::apply-in-sandbox (fcg::initial-node node)
                                       (original-cxn-set (construction-inventory node))
                                       :cxns-to-add (mapcar #'original-cxn processing-cxns-to-apply)
                                       :categories-to-add categories-to-add
                                       :categorial-links-to-add categorial-links))
               (solution-node
                (loop with current-node = (fcg::initial-node node)
                      with nodes-to-add = (append (ignore-initial-nodes (reverse (all-parents sandbox-solution)))
                                                  (list sandbox-solution))
                      for node in nodes-to-add
                      do (setf current-node (fcg::cip-add-child current-node (cipn-car node)))
                      finally (return current-node))))
          ;; reset categorial network
          (set-categorial-network (construction-inventory node) orig-categorial-network)
          (when (find 'fcg::succeeded (statuses sandbox-solution))
            (notify fix-applied repair-name form-constraints learned-cxns)
            (make-apply-fix-result
             :orig-cxns-to-apply cxns-to-apply
             :categorial-links categorial-links
             :orig-cxns-to-consolidate original-cxns-to-consolidate
             :categories-to-add categories-to-add
             :top-lvl-category top-level-category
             :gold-standard-consulted-p gold-standard-consulted-p
             :node node
             :solution-node solution-node
             :repair repair-name))))
      ;; node is nil (we are in a deeper level of the recursion)
      ;; just pass through the list of cxns and cats
      (progn
        (notify fix-applied repair-name form-constraints learned-cxns)
        (make-apply-fix-result
         :orig-cxns-to-apply cxns-to-apply
         :categorial-links categorial-links
         :orig-cxns-to-consolidate original-cxns-to-consolidate
         :categories-to-add categories-to-add
         :top-lvl-category top-level-category
         :gold-standard-consulted-p gold-standard-consulted-p
         :node node
         :solution-node nil
         :repair repair-name)))))
       
    



;; Generic handle fix for grammar learning
(defmethod handle-fix ((fix fcg::cxn-fix) (repair add-cxns-and-categorial-links) (problem problem) (node cip-node) &key &allow-other-keys) 
  "Enqueue the solution node."
  (push fix (fixes (problem fix)))
  (let* ((cxns-to-apply (afr-orig-cxns-to-apply (restart-data fix)))
         (categorial-links (afr-categorial-links (restart-data fix)))
         (cxns-to-consolidate (afr-orig-cxns-to-consolidate (restart-data fix)))
         (categories-to-add (afr-categories-to-add (restart-data fix)))
         (gold-standard-consulted-p (afr-gold-standard-consulted-p (restart-data fix)))
         (solution-node (afr-solution-node (restart-data fix)))
         (learned-cxns
          (remove-if-not #'(lambda (cxn) (and (eql (attr-val cxn :label) 'fcg::routine)
                                              (not (attr-val cxn :learned-at))))
                         (append cxns-to-apply cxns-to-consolidate))))
    (when learned-cxns
      (loop for cxn in learned-cxns
            for interaction-nr = (find-data (blackboard (construction-inventory node)) :current-interaction-nr)
            do (setf (attr-val cxn :learned-at) (format nil "@~a" interaction-nr)))
      (notify cxns-learned learned-cxns))
   
    ;; Add cxns to blackboard of second new node
    (set-data (car-resulting-cfs (cipn-car solution-node)) :fix-cxns cxns-to-consolidate)
    (set-data (car-resulting-cfs (cipn-car solution-node)) :fix-categorial-links categorial-links)
    (set-data (car-resulting-cfs (cipn-car solution-node)) :fix-categories categories-to-add)
    ;; set cxn-supplier to second new node
    (setf (cxn-supplier solution-node) (cxn-supplier node))
    ;; set statuses (colors in web interface)
    (when gold-standard-consulted-p
      (push 'gold-standard-consulted (statuses solution-node)))
    (push (afr-repair (restart-data fix)) (statuses solution-node))
    (push 'added-by-repair (statuses solution-node))
    ;; enqueue only second new node; never backtrack over the first applied holistic construction, we applied them as a block
    (cip-enqueue solution-node (cip node) (get-configuration node :queue-mode))))
      

