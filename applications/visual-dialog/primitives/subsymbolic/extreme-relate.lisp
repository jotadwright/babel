(in-package :visual-dialog)

;; ------------------
;; RELATE primitive ;;
;; ------------------

(defprimitive extreme-relate ((target world-model)
                              (source world-model)
                              (scene pathname-entity)
                              (spatial-relation spatial-relation-category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source spatial-relation scene => target)
   (let ((source-object-id-list (collect-objects-id-from-world-model source))
         (source-object-list (collect-objects-from-world-model source)))
     (multiple-value-bind (bind-scores bind-values)
         (evaluate-neural-primitive
          "extreme-relate"
          (get-data ontology 'server-address)
          (get-data ontology 'cookie-jar)
        `(:target nil
          :source ,source-object-id-list
          :scene ,(namestring (path scene))
          :spatial-relation ,(spatial-relation spatial-relation)))
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
                  (bind (target ;(getf scores 'target)
                         1.0
                         (make-instance 'world-model
                                        :id 'context
                                        :path scene
                                        :set-items
                                        (list
                                         (make-instance 'turn
                                                        :timestamp 'permanent
                                                        :object-set
                                                        (make-instance 'object-set
                                                                       :objects objects-with-attn)))))))))))
  
  ;; second case; given source-object and target set, compute the spatial relation
  ((source target scene => spatial-relation)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        (get-data ontology 'cookie-jar)
        `(:primitive extreme-relate
          :slots (:source-attn ,(id source-attn)
                  :spatial-relation nil
                  :target-attn ,(id target-attn))))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (spatial-relation
                     (getf scores 'spatial-relation)
                     (find (intern (getf values 'spatial-relation)
                                   :hybrid-dialog)
                           (get-data ontology 'spatial-relation)
                           :key #'spatial-relation))))))

  ;; third case; given source-object, compute pairs of target-set and spatial-relation
  ((source scene => target spatial-relation)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        (get-data ontology 'cookie-jar)
        `(:primitive extreme-relate
          :slots (:source-attn ,(id source-attn)
                  :spatial-relation nil
                  :target-attn nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target
                     (getf scores 'target-attn)
                     (make-instance 'attention
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-dialog)))
                    (spatial-relation
                     (getf scores 'spatial-relation)
                     (find (intern (getf values 'spatial-relation)
                                   :hybrid-dialog)
                           (get-data ontology 'spatial-relation)
                           :key #'spatial-relation))))))

  ;; fourth case; given source-object, target-set and spatial-relation
  ;; check for consistency
  ((source target spatial-relation scene =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           (get-data ontology 'cookie-jar)
           `(:primitive extreme-relate
             :slots (:source-attn ,(id source-attn)
                     :spatial-relation ,(spatial-relation spatial-relation)
                     :target-attn ,(id target-attn))))))
     consistentp))
  :primitive-inventory *subsymbolic-primitives*)
