;;;; relate.lisp

(in-package :hybrid-primitives)

;; ------------------
;; RELATE primtive ;;
;; ------------------

(defprimitive relate ((target-attn attention)
                      (source-attn attention)
                      (spatial-relation spatial-relation-category))
  ;; first case; given source-object and spatial relation, compute the target set
  ((source-attn spatial-relation => target-attn)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive relate
             :slots (:source-attn ,(id source-attn)
                     :spatial-relation ,(spatial-relation spatial-relation)
                     :target-attn . nil)))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (make-instance 'attention
                                                          :id (internal-symb value))))))))
  ;; second case; given source-object and target set, compute the spatial relation
  ((source-attn target-attn => spatial-relation)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive relate
             :slots (:source-attn ,(id source-attn)
                     :spatial-relation nil
                     :target-attn ,(id target-attn))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (find (internal-symb value)
                                                 (get-data ontology 'spatial-relation)
                                                 :key #'spatial-relation)))))))

  ;; third case; given source-object, compute pairs of target-set and spatial-relation
  ((source-attn => target-attn spatial-relation)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive relate
             :slots (:source-attn ,(id source-attn)
                     :spatial-relation nil
                     :target-attn nil)))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (case variable
                                             (spatial-relation
                                              (find (internal-symb value)
                                                    (get-data ontology 'spatial-relation)
                                                    :key #'spatial-relation))
                                             (target-attn
                                              (make-instance 'attention
                                                             :id (internal-symb value))))))))))

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
     consistentp)))
