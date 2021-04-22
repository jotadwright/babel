;;;; relate.lisp

(in-package :hybrid-primitives)

;; ------------------
;; RELATE primtive ;;
;; ------------------

(defprimitive relate ((target-attn attention-set)
                      (source-attn attention-object)
                      (spatial-relation spatial-relation-category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source-attn spatial-relation => target-attn)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive relate
          :slots (:source-attn ,(id source-attn)
                  :spatial-relation ,(spatial-relation spatial-relation)
                  :target-attn . nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-attn
                     (getf scores 'target-attn)
                     (make-instance 'attention-set
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-primitives)))))))
  
  ;; second case; given source-object and target set, compute the spatial relation
  ((source-attn target-attn => spatial-relation)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive relate
          :slots (:source-attn ,(id source-attn)
                  :spatial-relation nil
                  :target-attn ,(id target-attn))))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (spatial-relation
                     (getf scores 'spatial-relation)
                     (find (intern (getf values 'spatial-relation)
                                   :hybrid-primitives)
                           (get-data ontology 'spatial-relations)
                           :key #'spatial-relation))))))

  ;; third case; given source-object, compute pairs of target-set and spatial-relation
  ((source-attn => target-attn spatial-relation)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive relate
          :slots (:source-attn ,(id source-attn)
                  :spatial-relation nil
                  :target-attn nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-attn
                     (getf scores 'target-attn)
                     (make-instance 'attention-set
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-primitives)))
                    (spatial-relation
                     (getf scores 'spatial-relation)
                     (find (intern (getf values 'spatial-relation)
                                   :hybrid-primitives)
                           (get-data ontology 'spatial-relations)
                           :key #'spatial-relation))))))

  ;; fourth case; given source-object, target-set and spatial-relation
  ;; check for consistency
  ((source-attn target-attn spatial-relation =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive relate
             :slots (:source-attn ,(id source-attn)
                     :spatial-relation ,(spatial-relation spatial-relation)
                     :target-attn ,(id target-attn))))))
     consistentp))
  :primitive-inventory *hybrid-primitives*)
