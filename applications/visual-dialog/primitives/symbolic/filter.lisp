(in-package :visual-dialog)

;; ------------------
;; FILTER primitive ;;
;; ------------------

(defprimitive filter-by-attribute ((target-set world-model)
                                   (source-set world-model)
                                   (scene pathname-entity)
                                   (category attribute))
  ;; first case: if given source-set and category, compute target-set
  ((source-set category scene => target-set)
   ;symbolic case
     (let ((computed-set (filter-by-category source-set category)))
       (if computed-set
         (bind (target-set 1.0 computed-set))
         (bind (target-set 1.0
                           (make-instance 'world-model
                                          :id (id source-set)
                                          :set-items (list
                                                      (make-instance 'turn
                                                                     :object-set (make-instance 'object-set :id 'empty-set)))))))))
  :primitive-inventory *symbolic-primitives* )

(defgeneric filter-by-category (set category)
  (:documentation "Filter the set by the given category."))

(defmethod filter-by-category ((set world-model)
                               (attribute-category attribute))
  (multiple-value-bind (last-set last-timestamp) (the-biggest #'timestamp (set-items set))
    (if (or (equal (id attribute-category)
                   'thing)
            (equal (id attribute-category)
                   'number))
      ;if thing or number, return new world model with same object-set
      (make-instance 'world-model
                     :set-items (list (make-instance 'turn
                                                     :object-set (copy-object (object-set last-set)))))
      ;else filter objects on attribute-category and return new world model with filtered-objects
      (let ((filtered-objects
             (loop for object in (objects (object-set last-set))
                   for attr-cat = (intern (string-replace (type-of attribute-category) "-category" "") "KEYWORD")
                   if (and (listp (attributes object))
                           (equal (id attribute-category)
                                  (cdr (assoc attr-cat (attributes object)))))
                   collect object)))
      (when filtered-objects
        (make-instance 'world-model
                       :set-items (list (make-instance 'turn
                                                       :object-set (make-instance 'object-set :objects filtered-objects)))))))))
