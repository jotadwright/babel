;;;; unique.lisp

(in-package :hybrid-primitives)

;; ------------------
;; UNIQUE primtive ;;
;; ------------------

(defprimitive unique ((target-attn attention)
                      (source-attn attention))
  ;; first case; given source set, compute target object
  ((source-attn => target-attn)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . unique)
             (:slots ((:source-attn . ,(id source-attn))
                      (:target-attn . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (make-instance 'attention
                                                          :id (internal-symb (upcase value)))))))))

  ;; second case; given source set and target object
  ;; check for consistency
  ((source-attn target-attn =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . unique)
             (:slots ((:source-attn . ,(id source-attn))
                      (:target-attn . ,(id target-attn))))))))
     consistentp)))

