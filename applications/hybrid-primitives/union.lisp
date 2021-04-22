;;;; union.lisp

(in-package :hybrid-primitives)

;; -----------------
;; UNION primtive ;;
;; -----------------

(defprimitive union! ((target-attn attention-set)
                      (source-attn-1 attention-set)
                      (source-attn-2 attention-set))
  ;; first case; given both source sets, compute the target set
  ((source-attn-1 source-attn-2 => target-attn)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive union
          :slots (:target-attn nil
                  :source-attn-1 ,(id source-attn-1)
                  :source-attn-2 ,(id source-attn-2))))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-attn
                     (getf scores 'target-attn)
                     (make-instance 'attention-set
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-primitives)))))))

  ;; fourth case; given both source sets and target set
  ;; check for consistency
  ((source-attn-1 source-attn-2 target-attn =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive union
             :slots (:target-attn ,(id target-attn)
                     :source-attn-1 ,(id source-attn-1)
                     :source-attn-2 ,(id source-attn-2))))))
     consistentp))
  :primitive-inventory *hybrid-primitives*)