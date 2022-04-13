(in-package :visual-dialog)

;; ------------------
;; UNIQUE primitive ;;
;; ------------------

(defprimitive unique ((target-set world-model)
                      (source-set world-model))
  ;; first case; given source set, compute target object
  ((source-set => target-set) 
   (if (length= (objects (object-set (first (set-items source-set)))) 1)
     (bind (target-set 1.0 (make-instance 'world-model
                                                 :set-items (list (make-instance 'turn
                                                                                 :object-set
                                                                                 (make-instance 'object-set :objects (list (first (objects (object-set (first (set-items source-set))))))))))))))

  ;; second case; given source set and target object
  ;; check for consistency
  ((source-set target-set =>)
   (and (length= (objects (object-set (first (set-items source-set)))) 1)
        (equal-entity target-set (first (objects (object-set (first (set-items source-set))))))))
  :primitive-inventory (*symbolic-primitives* *subsymbolic-primitives*))


