(in-package :visual-dialog)

;; ------------------
;; RELATE primitive ;;
;; ------------------

(defprimitive extreme-relate ((target-attn attention)
                              (source-attn attention)
                              (spatial-relation spatial-relation-category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source-attn spatial-relation => target-attn)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        (get-data ontology 'cookie-jar)
        `(:primitive extreme-relate
          :slots (:source-attn ,(id source-attn)
                  :spatial-relation ,(spatial-relation spatial-relation)
                  :target-attn . nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-attn
                     (getf scores 'target-attn)
                     (make-instance 'attention
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-dialog)))))))
  
  ;; second case; given source-object and target set, compute the spatial relation
  ((source-attn target-attn => spatial-relation)
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
  ((source-attn => target-attn spatial-relation)
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
           do (bind (target-attn
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
  ((source-attn target-attn spatial-relation =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           (get-data ontology 'cookie-jar)
           `(:primitive extreme-relate
             :slots (:source-attn ,(id source-attn)
                     :spatial-relation ,(spatial-relation spatial-relation)
                     :target-attn ,(id target-attn))))))
     consistentp))
  :primitive-inventory *hybrid-primitives*)
