(in-package :fcg)

(defgeneric invent (cip agent topic scene)
  (:documentation "If the speaker could not formulate, new linguistic means need to be invented."))


(defmethod invent ((cip construction-inventory-processor) (agent crs-conventionality::naming-game-agent)
                   (topic crs-conventionality::crs-conventionality-entity-set) (scene crs-conventionality::crs-conventionality-entity-set))
  (let ((cxn-inventory (original-cxn-set (construction-inventory cip))) ;; original cxn set for inventing
        (cipn (top-node cip)))
    
    ;; Add diagnostics, repairs and best-solution to cip
    (loop for diagnostic in (reverse (get-configuration cxn-inventory :diagnostics))
          do (fcg::add-diagnostic cipn diagnostic))
    (loop for repair in (reverse (get-configuration cxn-inventory :repairs))
          do (fcg::add-repair cipn repair))
  
    ;; Notify learning
    (let* ((fix (first (second (multiple-value-list (fcg::notify-learning cipn :trigger 'routine-processing-finished)))))
           (cxn (restart-data fix))
           (best-solution nil)
           (consolidated-cxns nil)
           (current-node (top-node cip))
           (fixed-car (first (get-data fix 'fixed-cars))) 
           (child (cip-add-child current-node fixed-car)))
      
      (setf current-node child)
      (push (type-of (issued-by fix)) (statuses child))
      (setf (fully-expanded? child) t)
      (cip-run-goal-tests child cip) ;; to include succeeded status in node statuses
      (push 'added-by-repair (statuses child))
                                                                   
      (add-cxn cxn cxn-inventory)
      (push cxn consolidated-cxns)
    
      (values cxn fix))))



