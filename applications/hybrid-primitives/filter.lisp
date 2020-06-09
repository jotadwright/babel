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
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . filter)
             (:slots ((:source-attn . ,(id source-attn))
                      (:category . ,(category-value category))
                      (:target-attn . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (make-instance 'attention
                                                          :id (internal-symb (upcase value)))))))))

  ;; second case: if given source-set and target-set, compute category
  ((source-attn target-attn => category)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . filter)
             (:slots ((:source-attn . ,(id source-attn))
                      (:category . nil)
                      (:target-attn . ,(id target-attn))))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (find-entity-by-id
                                            ontology
                                            (internal-symb (upcase value)))))))))

  ;; third case: if given source-set, compute pairs of target-set and category
  ((source-attn => target-attn category)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . filter)
             (:slots ((:source-attn . ,(id source-attn))
                      (:category . nil)
                      (:target-attn . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (case variable
                                             (category (find-entity-by-id
                                                        ontology
                                                        (internal-symb (upcase value))))
                                             (target-attn (make-instance 'attention
                                                                         :id (internal-symb (upcase value)))))))))))

  ;; fourth case: if given source-set, target-set and category, check for consistency
  ((source-attn target-attn category =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitve . filter)
             (:slots ((:source-attn . ,(id source-attn))
                      (:category . ,(category-value category))
                      (:target-attn . ,(id target-attn))))))))
     consistentp)))

