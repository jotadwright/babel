(in-package :visual-dialog)

;; ------------------
;; RELATE primitive ;;
;; ------------------
    
(defprimitive immediate-relate ((target world-model)
                                (source world-model)
                                (segmented-scene world-model)
                                (scene pathname-entity)
                                (spatial-relation category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source segmented-scene scene spatial-relation => target)
   (let ((rel (cond ((eq (type-of spatial-relation) '2D-relation-category)
                     (convert-relation spatial-relation))
                    ((eq (type-of spatial-relation) 'spatial-relation-category)
                     (spatial-relation spatial-relation))))
         (scene-objects-id-list (collect-objects-id-from-world-model segmented-scene))
         (scene-objects-list (collect-objects-from-world-model segmented-scene))
         (source-object-list  (collect-objects-id-from-world-model source)))
     (if source-object-list 
       (multiple-value-bind (bind-scores bind-values)
           (evaluate-neural-primitive
            "immediate-relate"
            (get-data ontology 'server-address)
            (get-data ontology 'cookie-jar)
            `(:target nil
              :source ,source-object-list
              :segmented-scene ,scene-objects-id-list
              :scene ,(namestring (path scene))
              :spatial-relation ,rel))
         (loop for scores in bind-scores
               for values in bind-values
               do (if (getf values 'target)
                    (let ((objects-with-attn
                           (loop for attn in (getf values 'target)
                                ; for score in (getf scores 'target)
                                   for score = 1.0
                                 for attn-id = (intern attn :visual-dialog)
                                 for object = (copy-object (find attn-id scene-objects-list :key #'id))
                                   do (setf (scores (attention object)) score)
                                 collect object)))
                      (bind (target ; (getf scores 'target)
                             1.0
                                    (make-instance 'world-model
                                                   :id (id source)
                                                   :set-items (list (make-instance 'turn
                                                                                   :object-set (make-instance 'object-set :objects objects-with-attn)))))))
                    (bind (target 1.0 (make-instance 'world-model
                                                     :id (id source)
                                                     :set-items (list (make-instance 'turn
                                                                                     :object-set (make-instance 'object-set :id 'empty-set))))))))))))
   
   :primitive-inventory *subsymbolic-primitives*)

(defmethod convert-relation ((spatial-relation 2D-relation-category))
  (let* ((relation (2D-relation spatial-relation))
        (rel (cond ((eq relation 'below) 'below)
                  ((eq relation 'above) 'above)
                  ((eq relation '2D-right) 'right)
                  ((eq relation '2D-left) 'left))))
    rel))

