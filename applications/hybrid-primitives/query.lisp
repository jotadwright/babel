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
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive query
          :slots (:source-attn ,(id source-attn)
                  :attribute ,(attribute attribute)
                  :target-category nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-category
                     (getf scores 'target-category)
                     (find-entity-by-id
                      ontology
                      (intern (getf values 'target-category)
                              :hybrid-primitives)))))))

  ;; second case; given source-object and target-category, compute the attribute
  ((source-attn target-category => attribute)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive query
          :slots (:source-attn ,(id source-attn)
                  :target-category ,(category-value target-category)
                  :attribute nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (attribute
                     (getf scores 'attribute)
                     (find (intern (getf values 'attribute)
                                   :hybrid-primitives)
                           (get-data ontology 'attributes)
                           :key #'id))))))

  ;; third case; given source-object, compute pairs of attribute and target-category
  ((source-attn => attribute target-category)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive query
          :slots (:source-attn ,(id source-attn)
                  :target-category nil
                  :attribute nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (attribute
                     (getf scores 'attribute)
                     (find (intern (getf values 'attribute)
                                   :hybrid-primitives)
                           (get-data ontology 'attributes)
                           :key #'id))
                    (target-category
                     (getf scores 'target-category)
                     (find-entity-by-id
                      ontology
                      (intern (getf values 'target-category)
                              :hybrid-primitives)))))))
  
  ;; fourth case; if given source-object, attribute and target-category, check
  ;; for consistency
  ((source-attn attribute target-category =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive query
             :slots (:source-attn ,(id source-attn)
                     :target-category ,(category-value target-category)
                     :attribute ,(attribute attribute))))))
     consistentp))
  :primitive-inventory *hybrid-primitives*)

