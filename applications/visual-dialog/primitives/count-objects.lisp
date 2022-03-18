(in-package :visual-dialog)

;; -----------------
;; COUNT primtive ;;
;; -----------------

(defprimitive count-objects ((target-num digit-category)
                             (source-set world-model))
  ;; first case; given source-set, compute target
  ((source-set => target-num)
   (let ((count (length (objects (object-set (first (reverse (set-items source-set))))))))
     (bind (target-num 1.0 ;(make-instance 'digit-category :id count :digit count)
                       (find-entity-by-id (get-data ontology 'digits) (internal-symb (upcase (format nil "~r" count))))
                       ))))  
  ;; second case; given source and target, check consistency
  ((source-set target-num =>)
   (let ((count (length (objects (object-set (first (reverse (set-items source-set))))))))
     (equal-entity target-num (find-entity-by-id (get-data ontology 'digits)
                                                 (internal-symb (upcase  (format nil "~r" count)))))))
  :primitive-inventory *symbolic-primitives*)
