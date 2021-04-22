;;;; get-context.lisp

(in-package :hybrid-primitives)

;; -----------------------
;; GET-CONTEXT primtive ;;
;; -----------------------

(defprimitive get-context ((context attention-set))
  ;; first case; consistency check
  ((context =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive get-context
             :slots (:context ,(id context))))))
     consistentp))

  ;; second case; bind the context from the ontology
  ((=> context)
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive get-context
          :slots (:context nil)))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (context
                     (getf scores 'context)
                     (make-instance 'attention-set
                                    :id (intern (getf values 'context)
                                                :hybrid-primitives)))))))
  :primitive-inventory *hybrid-primitives*)

