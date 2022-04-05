(in-package :visual-dialog)

;; -----------------
;; MORE-THAN-ONE primitive 
;; -----------------

(defgeneric get-length (object)
  (:documentation "Gets the length of the thing"))

(defmethod get-length ((object object)) 1)
(defmethod get-length ((set world-model))
  (length (objects (object-set (first (set-items set))))))

(defprimitive more-than-1 ((target-bool boolean-category)
                           (source-set world-model))
  ;; first case; give source-set, compute target-bool
  ((source-set => target-bool)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (> (get-length source-set) 1)
             'yes 'no))))
     ;(print (get-length source-set))
     (bind (target-bool 1.0 boolean-category))))

  ;; second case; given source-set and target-bool, check consistency
  ((source-set target-bool =>)
   (let ((boolean-category
          (find-entity-by-id
           ontology
           (if (> (get-length source-set) 1)
             'yes 'no))))
     (equal-entity target-bool boolean-category)))
  :primitive-inventory (*symbolic-primitives* *hybrid-primitives*))