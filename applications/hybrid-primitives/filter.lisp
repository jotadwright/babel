;;;; filter.lisp

(in-package :hybrid-primitives)

;; ------------------
;; FILTER primtive ;;
;; ------------------

(defprimitive filter ((target-attn attention)
                      (source-attn attention)
                      (category attribute))
  ;; first case: if given source-set and category, compute target-set
  ((source-attn category => target-attn)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive filter
          :slots (:source-attn ,(id source-attn)
                  :category  ,(category-value category)
                  :target-attn nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-attn
                     (getf scores 'target-attn)
                     (make-instance 'attention
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-primitives)))))))

  ;; second case: if given source-set and target-set, compute category
  ((source-attn target-attn => category)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive filter
          :slots (:source-attn ,(id source-attn)
                  :category  nil
                  :target-attn ,(id target-attn))))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (category
                     (getf scores 'category)
                     (find-entity-by-id
                      ontology
                      (intern (getf values 'category)
                              :hybrid-primitives)))))))

  ;; third case: if given source-set, compute pairs of target-set and category
  ((source-attn => target-attn category)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive filter
          :slots (:source-attn ,(id source-attn)
                  :category nil
                  :target-attn nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-attn
                     (getf scores 'target-attn)
                     (make-instance 'attention
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-primitives)))
                    (category
                     (getf scores 'category)
                     (find-entity-by-id
                      ontology
                      (intern (getf values 'category)
                              :hybrid-primitives)))))))

  ;; fourth case: if given source-set, target-set and category, check for consistency
  ((source-attn target-attn category =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitve filter
             :slots (:source-attn ,(id source-attn)
                     :category ,(category-value category)
                     :target-attn ,(id target-attn))))))
     consistentp))
  :primitive-inventory *hybrid-primitives*)

