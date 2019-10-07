(in-package :robot-concept-learning)

;; make html of mwm-object
(defmethod make-html-for-entity-details ((object mwm-object) &key)
  (loop for (attr . val) in (attributes object)
        append `(((div :class "entity-detail")
                  ,(format nil "~a = ~,2f" attr val)))))

;; make html of object set
(defmethod make-html-for-entity-details ((set mwm-object-set) &key)
  `(((div :class "entity-detail")
     ,@(loop for object in (objects set)
             collect (make-html object :expand-initially t)))))

;; make html of mwm-category
(defmethod make-html-for-entity-details ((category min-max-category) &key)
  `(((div :class "entity-detail") ,(format nil "attribute: ~a" (attribute category)))
    ((div :class "entity-detail") ,(format nil "lower-bound: ~,2f" (lower-bound category)))
    ((div :class "entity-detail") ,(format nil "upper-bound: ~,2f" (upper-bound category)))))

(defmethod make-html-for-entity-details ((category prototype-category) &key)
  `(((div :class "entity-detail") ,(format nil "attribute: ~a" (attribute category)))
    ((div :class "entity-detail") ,(format nil "prototype: ~,2f" (prototype category)))
    ((div :class "entity-detail") ,(format nil "variance: ~,2f" (/ (M2 category) (nr-samples category))))))

(defmethod make-html-for-entity-details ((category prototype-min-max-category) &key)
  `(((div :class "entity-detail") ,(format nil "attribute: ~a" (attribute category)))
    ((div :class "entity-detail") ,(format nil "prototype: ~,2f" (prototype category)))
    ((div :class "entity-detail") ,(format nil "lower-bound: ~,2f" (lower-bound category)))
    ((div :class "entity-detail") ,(format nil "upper-bound: ~,2f" (upper-bound category)))))

(defmethod make-html-for-entity-details ((category exponential-category) &key)
  `(((div :class "entity-detail") ,(format nil "attribute: ~a" (attribute category)))
    ((div :class "entity-detail") ,(format nil "prototype: ~,2f" (prototype category)))
    ((div :class "entity-detail") ,(format nil "left-sigma: ~,2f" (left-sigma category)))
    ((div :class "entity-detail") ,(format nil "right-sigma: ~,2f" (right-sigma category)))))
