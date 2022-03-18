(in-package :visual-dialog)

(defprimitive get-last-attribute-category ((attribute attribute-category)
                                           (source-set world-model))
  ;; first case
  ((source-set => attribute)
   (multiple-value-bind (last-set last-timestamp) (the-biggest #'timestamp (set-items source-set))
     (let* ((last-set-objects (objects (object-set last-set)))
            (second-last-set-objects (objects
                                      (object-set
                                       (find (- last-timestamp 1) (set-items source-set)
                                             :key #'timestamp :test #'=))))
            (last-topic-attribute nil))
       (if (length= last-set-objects second-last-set-objects)        
         (loop for object in last-set-objects
               for object2 = (find (id object) second-last-set-objects :key #'id)
               do (if (and (listp (attributes object))
                           (listp (attributes object2)))
                    (loop for attr in (attributes object)
                          do (let* ((first-attr (cdr attr))
                                    (second-attr (cdr (assoc (car attr) (attributes object2)))))
                               (if (not (eq first-attr second-attr))
                                 (setf last-topic-attribute (car attr)))))
                    (if (not (and (symbolp (attributes object))
                             (symbolp (attributes object2))))
                      
                      (setf last-topic-attribute (car (car (attributes object)))))))
         (progn
           (loop for object in last-set-objects
                 unless (member (id object) second-last-set-objects :key #'id)
                 do (setf last-topic-attribute (car (first (attributes object)))))))
       (bind (attribute 1.0 (make-instance 'attribute-category
                                             :attribute  (intern (symbol-name last-topic-attribute))))))))
  ;; second case: given attribute, compute source-set
  ((attribute source-set => )
   (equal-entity-last-attr attribute source-set))
  :primitive-inventory *symbolic-primitives*)


(defun equal-entity-last-attr (attribute source-set)
  (multiple-value-bind (last-set last-timestamp) (the-biggest #'timestamp (set-items source-set))
   (let* ((last-set-objects (objects (object-set last-set)))
            (second-last-set-objects (objects
                                      (object-set
                                       (find (- last-timestamp 1) (set-items source-set)
                                             :key #'timestamp :test #'=))))
            (last-topic-attribute nil))
       (if (length= last-set-objects second-last-set-objects)        
         (loop for object in last-set-objects
               for object2 = (find (id object) second-last-set-objects :key #'id)
               do (if (and (listp (attributes object))
                           (listp (attributes object2)))
                    (loop for attr in (attributes object)
                          do (let* ((first-attr (cdr attr))
                                    (second-attr (cdr (assoc (car attr) (attributes object2)))))
                               (if (not (eq first-attr second-attr))
                                 (setf last-topic-attribute (car attr)))))
                    (if (not (and (symbolp (attributes object))
                             (symbolp (attributes object2))))
                      
                      (setf last-topic-attribute (car (car (attributes object)))))))
         (progn
           (loop for object in last-set-objects
                 unless (member (id object) second-last-set-objects :key #'id)
                 do (setf last-topic-attribute (car (first (attributes object)))))))
       (if (equal  (intern (symbol-name last-topic-attribute)) (id attribute) )
         t)
       )))
  