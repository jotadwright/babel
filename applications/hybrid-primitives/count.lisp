;;;; count.lisp

(in-package :hybrid-primitives)

;; -----------------
;; COUNT primtive ;;
;; -----------------

(defprimitive count! ((target-num number)
                      (source-attn attention-set))
  ;; first case; given source-set, compute target
  ((source-attn => target-num)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive count
          :slots (:source-attn ,(id source-attn)
                  :target-num nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-num
                     (getf scores 'target-num)
                     (getf values 'target-num))))))

  ;; second case; given source and target, check consistency
  ((source-attn target-num =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive count
             :slots (:source-attn ,(id source-attn)
                     :target-num  ,target-num)))))
     consistentp))
  :primitive-inventory *hybrid-primitives*)

