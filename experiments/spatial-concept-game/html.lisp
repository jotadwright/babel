(in-package :spatial-concepts)

;; make html of spatial-object
(defmethod make-html-for-entity-details ((object spatial-object) &key)
  (loop for (attr . val) in (attributes object)
        append `(((div :class "entity-detail")
                  ,(format nil "~a = ~,2f" attr val)))))

;; make html of object set
(defmethod make-html-for-entity-details ((set spatial-object-set) &key)
  `(((div :class "entity-detail")
     ,@(loop for object in (objects set)
             collect (make-html object :expand-initially t)))))

;; make-html of concept
(defmethod make-html ((concept concept) &key)
  `((div)
    ,(s-dot->svg
      (concept->s-dot concept))))

;; make html of spatial-category
(defmethod make-html-for-entity-details ((prototype prototype) &key)
  `(((div :class "entity-detail")
     ,(format nil "attribute: ~a" (attribute prototype)))
    ((div :class "entity-detail")
     ,(format nil "prototype: ~,2f" (value prototype)))
    ((div :class "entity-detail")
     ,(format nil "variance: ~,2f" (/ (M2 prototype) (nr-samples prototype))))))