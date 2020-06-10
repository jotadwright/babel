;;;; get-context.lisp

(in-package :hybrid-primitives)

;; -----------------------
;; GET-CONTEXT primtive ;;
;; -----------------------

(defprimitive get-context ((context attention))
  ;; first case; consistency check
  ((context =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . get-context)
             (:slots . ((:context . ,(id context))))))))
     consistentp))

  ;; second case; bind the context from the ontology
  ((=> context)
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . get-context)
             (:slots . ((:context . nil)))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             collect (list variable score
                                           (make-instance 'attention
                                                          :id (internal-symb (upcase value))))))))))

