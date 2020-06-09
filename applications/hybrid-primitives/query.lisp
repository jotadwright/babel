;;;; query.lisp

(in-package :hybrid-primitives)

;; -----------------
;; QUERY primtive ;;
;; -----------------

(defprimitive query ((target-category attribute)
                     (source-attn attention)
                     (attribute attribute-category))
  ;; first case; given attribute and source-object, compute the target category
  ((source-attn attribute => target-category)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . query)
             (:slots ((:source-attn . ,(id source-attn))
                      (:attribute . ,(attribute attribute))
                      (:target-category . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (find-entity-by-id
                                            ontology
                                            (internal-symb (upcase value)))))))))

  ;; second case; given source-object and target-category, compute the attribute
  ((source-attn target-category => attribute)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . query)
             (:slots ((:source-attn . ,(id source-attn))
                      (:target-category . ,(category-value target-category))
                      (:attribute . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (find (internal-symb (upcase value))
                                                 (get-data ontology 'attributes)
                                                 :key #'id)))))))

  ;; third case; given source-object, compute pairs of attribute and target-category
  ((source-attn => attribute target-category)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . query)
             (:slots ((:source-attn . ,(id source-attn))
                      (:target-category . nil)
                      (:attribute . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (case variable
                                             (target-category
                                              (find-entity-by-id
                                               ontology
                                               (internal-symb (upcase value))))
                                             (attribute 
                                              (find (internal-symb (upcase value))
                                                    (get-data ontology 'attributes)
                                                    :key #'attribute)))))))))
  
  ;; fourth case; if given source-object, attribute and target-category, check
  ;; for consistency
  ((source-attn attribute target-category =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . query)
             (:slots ((:source-attn . ,(id source-attn))
                      (:target-category . ,(category-value target-category))
                      (:attribute . ,(attribute attribute))))))))
     consistentp)))

