;;;; exist.lisp

(in-package :hybrid-primitives)

;; -----------------
;; EXIST primtive ;;
;; -----------------

(defprimitive exist ((target-bool boolean-category)
                     (source-attn attention))
  ((source-attn => target-bool)
   ;; first case; give source-set, compute target-bool
   (let ((new-bindings
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . exist)
             (:slots ((:target-bool . nil)
                      (:source-attn . ,(id source-attn))))))))
     (loop for bind-set in new-bindings
           do `(bind ,@(loop for (variable score value) in bind-set
                             for yes/no = (internal-symb (upcase value))
                             collect (list variable score
                                           (find-entity-by-id
                                            ontology yes/no)))))))
  
  ;; second case; given source-set and target-bool, check consistency
  ((source-attn target-bool =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'endpoint)
           `((:primitive . exist)
             (:slots ((:target-bool . ,(bool target-bool))
                      (:source-attn . ,(id source-attn))))))))
     consistentp)))