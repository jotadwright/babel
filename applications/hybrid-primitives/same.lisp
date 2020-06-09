;;;; same.lisp

(in-package :hybrid-primitives)

;; ----------------
;; SAME primtive ;;
;; ----------------

(defprimitive same ((target-attn attention)
                    (source-attn attention)
                    (attribute attribute-category))
  ;; first case; given source-object and attribute, compute the target-set
  ((source-attn attribute => target-attn)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . same)
             (:slots ((:source-attn . ,(id source-attn))
                      (:attribute . ,(attribute attribute))
                      (:target-attn . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (make-instance 'attention
                                                          :id (internal-symb (upcase value)))))))))
  
  ;; second case; given source-object and target-set, compute the attribute
  ((source-attn target-attn => attribute)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . same)
             (:slots ((:source-attn . ,(id source-attn))
                      (:attribute . nil)
                      (:target-attn . ,(id target-attn))))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (find (internal-symb (upcase value))
                                                 (get-data ontology 'attributes)
                                                 :key #'attribute)))))))

  ;; third case; given source-object, compute pairs of attribute and target-set
  ((source-attn => target-attn attribute)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . same)
             (:slots ((:source-attn . ,(id source-attn))
                      (:attribute . nil)
                      (:target-attn . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (case variable
                                             (attribute
                                              (find (internal-symb (upcase value))
                                                    (get-data ontology 'attributes)
                                                    :key #'attribute))
                                             (target-attn
                                              (make-instance 'attention
                                                             :id (internal-symb (upcase value)))))))))))

  ;; fourth case; given source-object, attribute and target set,
  ;; check for consistency
  ((source-attn attribute target-attn =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . same)
             (:slots ((:source-attn . ,(id source-attn))
                      (:attribute . ,(attribute attribute))
                      (:target-attn . ,(id target-attn))))))))
     consistentp)))
