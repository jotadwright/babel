(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                             ;;
;; Functionality for handling vectors during unification       ;;
;;                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod fcg-expand ((type (eql :compare-distributional-vectors))
                       &key value source bindings merge? cxn-inventory)
  "Use cosine similarity metric to match via token embeddings."
  (if merge?
    ;; in the merging phase, we keep the original embedding pointer
    (values source bindings)
    ;; in the matching phase...
    (cond (;; if both embedding pointers are eq, we just continue like in unification 
           (eq value source) 
           (values value bindings))
          (;; if they are not eq, but non-nil, we compute their cosine and return true above a given threshold
           ;; we also add a binding between both pointers (variablified versions to make merge succeed)
           (and value source)
           (let* ((token-embedding-cxn (cdr (assoc value (get-data (blackboard cxn-inventory) :cxn-token-embeddings))))
                  (token-embedding-ts (cdr (assoc source (get-data (blackboard cxn-inventory) :ts-token-embeddings))))
                  (cosine-similarity (cosine-similarity token-embedding-cxn token-embedding-ts)))
             (if (> cosine-similarity 0.6)
               (values source (mapcar #'(lambda (bindings-list)
                                          (extend-bindings (variablify value) cosine-similarity bindings-list))
                                      bindings))
               (values nil +fail+))))
          (t
           (values nil +fail+)))))