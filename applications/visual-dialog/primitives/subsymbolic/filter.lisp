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
   (if (not (equal (id source-set) 'context))
     (let ((computed-set (filter-by-category source-set category)))
       (if computed-set
         (bind (target-set 1.0 computed-set))
         (bind (target-set 1.0
                           (make-instance 'world-model
                                          :set-items (list
                                                      (make-instance 'turn
                                                                     :id (id (first (set-items source-set)))
                                                                     :object-set (make-instance 'object-set :id 'empty-set))))))))
     (let ((source-object-id-list (collect-objects-id-from-world-model source-set))
           (source-object-list (collect-objects-from-world-model source-set)))
       (cond ((= (length source-object-id-list) 0)
              (bind (target-set 1.0 (make-instance 'world-model :id 'context :path scene :set-items (list (make-instance 'turn ))))))
             ((or (eql (category-value category) 'thing)
                  (eql (category-value category) 'number))
              (bind (target-set 1.0 (copy-object source-set))))
             ((eql (id category) 'none))
             (t 
              (multiple-value-bind (bind-scores bind-values)
                  (evaluate-neural-primitive
                   "filter"
                   (get-data ontology 'server-address)
                   (get-data ontology 'cookie-jar)
                   `(:target nil
                     :source ,source-object-id-list
                     :scene ,(namestring (path scene))
                     :concept ,(category-value category)))
                (loop for scores in bind-scores
                      for values in bind-values
                      do (let ((objects-with-attn
                                (loop for attn in (getf values 'target)
                                     ; for score in (getf scores 'target)
                                      for score = 1.0

                                      for attn-id = (intern attn :visual-dialog)
                                      for object = (copy-object (find attn-id source-object-list :key #'id))
                                        do (setf (scores (attention object)) score)
                                      collect object)))
                           (bind (target-set ; (getf scores 'target)
                                  1.0
                                             (make-instance 'world-model
                                                            :id 'context
                                                            :path scene
                                                            :set-items
                                                            (list
                                                             (make-instance 'turn
                                                                            :id (id (first (set-items source-set)))
                                                                            :timestamp 'permanent
                                                                            :object-set
                                                                            (make-instance 'object-set
                                                                                           :objects objects-with-attn))))))))))))))
     :primitive-inventory *subsymbolic-primitives*)
   
(defgeneric filter-by-category (set category)
  (:documentation "Filter the set by the given category."))

(defmethod filter-by-category ((set world-model)
                               (attribute-category attribute))
  (multiple-value-bind (last-set last-timestamp)
      (the-biggest #'timestamp (set-items set))
    (if (or (equal (id attribute-category)
                   'thing)
            (equal (id attribute-category)
                   'number))
      ;if thing or number, return new world model with same object-set
      (make-instance 'world-model
                     :set-items (list (make-instance 'turn
                                                     :id (id last-set)
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
                                                       :id (id last-set)
                                                       :object-set (make-instance 'object-set :objects filtered-objects)))))))))
