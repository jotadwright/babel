;;;; unique.lisp

(in-package :hybrid-primitives)

;; ------------------
;; UNIQUE primtive ;;
;; ------------------

(defprimitive unique ((target-attn attention-object)
                      (source-attn attention-set))
  ;; first case; given source set, compute target object
  ((source-attn => target-attn)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive unique
          :slots (:source-attn ,(id source-attn)
                  :target-attn nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-attn
                     (getf scores 'target-attn)
                     (make-instance 'attention-object
                                    :id (intern (getf values 'target-attn)
                                                :hybrid-primitives)))))))

  ;; second case; given source set and target object
  ;; check for consistency
  ((source-attn target-attn =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive unique
             :slots (:source-attn ,(id source-attn)
                     :target-attn ,(id target-attn))))))
     consistentp))
  :primitive-inventory *hybrid-primitives*)

