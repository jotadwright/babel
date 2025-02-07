(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                             ;;
;; Heuristics for dealing with embedding-based unification     ;;
;;                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod apply-heuristic ((node cip-node) (mode (eql :embedding-similarity)))
  "Discounts embedding bindings based on cosine."
  (let* ((cxn-inventory (construction-inventory node))
         (embedding-bindings (loop for binding in (car-second-merge-bindings (cipn-car node))
                                   for symbol-name = (symbol-name (car binding))
                                   when (and (>= (length symbol-name) 3)
                                             (equal "?->" (subseq symbol-name 0 3)))
                                     collect binding)))
    (if embedding-bindings
      (loop for embedding-binding in embedding-bindings
            for cosine-similarity = (cdr embedding-binding)
            sum cosine-similarity into total-similarity
            finally (return (- (- 1 (/ total-similarity (length embedding-bindings))))))
      0)))