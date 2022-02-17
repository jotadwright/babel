;;;; same.lisp

(in-package :hybrid-primitives)

;; ----------------
;; SAME primtive ;;
;; ----------------

(defprimitive same ((target-attn attention-set)
                    (source-attn attention-object)
                    (attribute attribute-category))
  ;; first case; given source-object and attribute, compute the target-set
  ((source-attn attribute => target-attn)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        (get-data ontology 'cookie-jar)
        `(:primitive same
          :slots (:source-attn ,(id source-attn)
                  :attribute ,(attribute attribute)
                  :target-attn nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-attn
                     (getf scores 'target-attn)
                     (make-instance 'attention-set
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-primitives)))))))
  
  ;; second case; given source-object and target-set, compute the attribute
  ((source-attn target-attn => attribute)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        (get-data ontology 'cookie-jar)
        `(:primitive same
          :slots (:source-attn ,(id source-attn)
                  :attribute nil
                  :target-attn ,(id target-attn))))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (attribute
                     (getf scores 'attribute)
                     (find (intern (getf values 'attribute)
                                   :hybrid-primitives)
                           (get-data ontology 'attributes)
                           :key #'attribute))))))

  ;; third case; given source-object, compute pairs of attribute and target-set
  ((source-attn => target-attn attribute)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        (get-data ontology 'cookie-jar)
        `(:primitive same
          :slots (:source-attn ,(id source-attn)
                  :attribute nil
                  :target-attn nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (attribute
                     (getf scores 'attribute)
                     (find (intern (getf values 'attribute)
                                   :hybrid-primitives)
                           (get-data ontology 'attributes)
                           :key #'attribute))
                    (target-attn
                     (getf scores 'target-attn)
                     (make-instance 'attention-set
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-primitives)))))))

  ;; fourth case; given source-object, attribute and target set,
  ;; check for consistency
  ((source-attn attribute target-attn =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           (get-data ontology 'cookie-jar)
           `(:primitive same
             :slots (:source-attn ,(id source-attn)
                     :attribute ,(attribute attribute)
                     :target-attn ,(id target-attn))))))
     consistentp))
  :primitive-inventory *hybrid-primitives*)
