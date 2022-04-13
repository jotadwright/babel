(in-package :visual-dialog)

;; ------------------
;; SELECT-ONE primitive ;;
;; ------------------

(defprimitive select-one ((target-world-model world-model)
                          (source-set world-model))
  ;; first case; given source set, compute target object
  ((source-set => target-world-model)
   (let ((object-list (collect-objects-from-world-model source-set)))
     (cond ((length= object-list 0)
            (bind (target-world-model 1.0 (make-instance 'world-model :id (id source-set)))))
     
           ((length= object-list 1)
            (bind (target-world-model 1.0 (make-instance 'world-model
                                                         :id (id source-set)
                                                         :set-items (list (make-instance 'turn
                                                                                         :object-set (make-instance 'object-set
                                                                                                                    :objects (list (first (collect-objects-from-world-model source-set))))))))))
           (t
            (loop for object in object-list
                  do (bind (target-world-model 1.0 (make-instance 'world-model
                                                                  :id (id source-set)
                                                                  :set-items
                                                                  (list (make-instance 'turn
                                                                                       :object-set (make-instance 'object-set
                                                                                                                  :objects (list object))))))))))))
     
  ;; second case; given source set and target object
  ;; check for consistency
  ((source-set target-world-model =>)
   (and (length= (objects (object-set (first (set-items source-set)))) 1)
        (equal-entity target-world-model (first (objects (object-set (first (set-items source-set))))))))
  :primitive-inventory (*symbolic-primitives* *subsymbolic-primitives*))

