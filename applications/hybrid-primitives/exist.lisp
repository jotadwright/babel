;;;; exist.lisp

(in-package :hybrid-primitives)

;; -----------------
;; EXIST primtive ;;
;; -----------------

(defprimitive exist ((target-bool boolean-category)
                     (source-attn attention))
  ((source-attn => target-bool)
   ;; first case; give source-set, compute target-bool
   (multiple-value-bind (bind-scores bind-values)
       (evaluate-neural-primitive
        (get-data ontology 'server-address)
        `(:primitive exist
          :slots (:target-bool nil
                  :source-attn ,(id source-attn))))
     (loop for scores in bind-scores
           for values in bind-values
           do (bind (target-bool
                     (getf scores 'target-bool)
                     (find-entity-by-id
                      ontology
                      (intern (getf values 'target-bool)
                              :hybrid-primitives)))))))
  
  ;; second case; given source-set and target-bool, check consistency
  ((source-attn target-bool =>)
   (let ((consistentp
          (evaluate-neural-primitive
           (get-data ontology 'server-address)
           `(:primitive exist
             :slots (:target-bool ,(bool target-bool)
                     :source-attn ,(id source-attn))))))
     consistentp)))