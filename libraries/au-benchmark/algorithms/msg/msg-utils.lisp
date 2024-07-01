(in-package :au-benchmark.msg)

(export '(matching-predicates
          msg-alignment-state
          pattern-predicates
          source-predicates
          pattern-delta
          source-delta
          pattern-remaining
          source-remaining))


(defun matching-predicates (predicate-1 predicate-2 &key allow-generalisation-over-constants)
  "Returns t if predicate-1 and predicate-2 can be aligned: same feature name and same arity.
   By default, returns nil if a constant occurs in one predicate and the same constant does
   not appear at the same position in the other predicate."
  (and
   ;; predicate names are equal
   (equalp (first predicate-1) (first predicate-2))
   ;; arity is equal
   (= (length predicate-1) (length predicate-2))
   ;; allow or disallow generalisation over constants
   (if allow-generalisation-over-constants
     t
     (loop for arg-1 in (rest predicate-1)
           for arg-2 in (rest predicate-2)
           always (or (equalp arg-1 arg-2)
                      (and (variable-p arg-1)
                           (variable-p arg-2)))))))