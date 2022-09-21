(in-package :visual-dialog)

;; ------------------
;; UNIQUE primitive ;;
;; ------------------

(defprimitive unique ((target-set world-model)
                      (source-set world-model))
  ;; first case; given source set, compute target object
  ((source-set => target-set)
   
   (let ((objects (collect-objects-from-world-model source-set)))
     (if (length= objects 1)
       (bind (target-set 1.0 (make-instance 'world-model
                                            :set-items
                                            (list (make-instance 'turn
                                                                 :object-set
                                                                 (make-instance 'object-set
                                                                                :objects
                                                                                (list (first (objects (object-set (first (set-items source-set))))))))))))

       (if (and (equal (get-configuration *subsymbolic-primitives* :search-mode)
                  :best-first)
                objects)
           (loop for object in objects
                ; for score = (average (scores (attention object)))
                 do (bind (target-set ;score
                                      1.0 (make-instance 'world-model
                                                           :set-items
                                                           (list (make-instance 'turn
                                                                                :object-set
                                                                                (make-instance 'object-set
                                                                                               :objects
                                                                                               (list object))))))))))))

  ;; second case; given source set and target object
  ;; check for consistency
  ((source-set target-set =>)
   (and (length= (collect-objects-from-world-model source-set) 1)
        (equal-entity target-set (first (collect-objects-from-world-model source-set)))))
  :primitive-inventory (*symbolic-primitives* *subsymbolic-primitives*))

(average '( 0 0 5))

