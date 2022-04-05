(in-package :visual-dialog)

;; ------------------
;; UNIQUE primtive ;;
;; ------------------

(defprimitive unique ((target-object-set object-set)
                      (source-set world-model))
  ;; first case; given source set, compute target object
  ((source-set => target-object-set) 
   (if (length= (objects (object-set (first (set-items source-set)))) 1)
     (bind (target-object-set 1.0 (make-instance 'object-set :objects (list (first (objects (object-set (first (set-items source-set)))))))))))

  ;; second case; given source set and target object
  ;; check for consistency
  ((source-set target-object-set =>)
   (and (length= (objects (object-set (first (set-items source-set)))) 1)
        (equal-entity target-object-set (first (objects (object-set (first (set-items source-set))))))))
  :primitive-inventory (*symbolic-primitives* *hybrid-primitives*))


