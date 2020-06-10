;;;; count.lisp

(in-package :hybrid-primitives)

;; -----------------
;; COUNT primtive ;;
;; -----------------

(defprimitive count! ((target-num number)
                      (source-attn attention))
  ;; first case; given source-set, compute target
  ((source-attn => target-num)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . count)
             (:slots ((:source-attn . ,(id source-attn))
                      (:target-num . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (parse-integer value)))))))

  ;; second case; given source and target, check consistency
  ((source-attn target-num =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitve . count)
             (:slots ((:source-attn . ,(id source-attn))
                      (:target-num . ,target-num)))))))
     consistentp)))

