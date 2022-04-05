(in-package :visual-dialog)

;; ------------------
;; SELECT-ONE primtive ;;
;; ------------------

(defprimitive select-one ((target-object-set object-set)
                      (source-set world-model))
  ;; first case; given source set, compute target object
  ((source-set => target-object-set)
   (if (length= (objects (object-set (first (set-items source-set)))) 0)
     (bind (target-object-set 1.0 (make-instance 'object-set :objects '() :id 'empty-set))))
     
   (if (length= (objects (object-set (first (set-items source-set)))) 1)
     (bind (target-object-set 1.0 (make-instance 'object-set :objects (list (first (objects (object-set (first (set-items source-set)))))))))
     (loop for object in (objects (object-set (first (set-items source-set))))
           do (bind (target-object-set 1.0 (make-instance 'object-set :objects (list object)))))))

  ;; second case; given source set and target object
  ;; check for consistency
  ((source-set target-object-set =>)
   (and (length= (objects (object-set (first (set-items source-set)))) 1)
        (equal-entity target-object-set (first (objects (object-set (first (set-items source-set))))))))
  :primitive-inventory (*symbolic-primitives* *hybrid-primitives*))