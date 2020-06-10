;;;; intersect.lisp

(in-package :hybrid-primitives)

;; ---------------------
;; INTERSECT primtive ;;
;; ---------------------

(defprimitive intersect ((target-attn attention)
                         (source-attn-1 attention)
                         (source-attn-2 attention))
  ;; first case; given both source sets, compute the target set
  ((source-attn-1 source-attn-2 => target-attn)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . intersect)
             (:slots ((:target-attn . nil)
                      (:source-attn-1 . ,(id source-attn-1))
                      (:source-attn-2 . ,(id source-attn-2))))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (make-instance 'attention
                                                          :id (internal-symb (upcase value)))))))))

  ;; fourth case; given both source sets and target set
  ;; check for consistency
  ((source-attn-1 source-attn-2 target-attn =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . intersect)
             (:slots ((:target-attn . ,(id target-attn))
                      (:source-attn-1 . ,(id source-attn-1))
                      (:source-attn-2 . ,(id source-attn-2))))))))
     consistentp)))