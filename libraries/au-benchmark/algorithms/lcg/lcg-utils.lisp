(in-package :au-benchmark.lcg)

(export '(matching-predicates-p
          gen
          compatible-pairs-p
          compatible-with-set-of-pairs
          lcg-alignment-state
          candidates
          phi))


(defun matching-predicates-p (predicate-1 predicate-2)
  "Check if p1 matches p2 and return the bindings.
   Predicates match when their arguments do not introduce
   non-injective bindings."
  (if (= (length predicate-1) (length predicate-2))
    (loop named predicate-elements
          with bindings = +no-bindings+
          for el-1 in predicate-1
          for el-2 in predicate-2
          do (cond (;; when two equal constants, continue
                    (and (not (variable-p el-1)) (not (variable-p el-2)) (equalp el-1 el-2)))
                   (;; when two different constant, fail
                    (and (not (variable-p el-1)) (not (variable-p el-2)) (not (equalp el-1 el-2)))
                    (return-from predicate-elements 'fail))
                   (;; when el-1 is bound to X, and el-2 = X, continue
                    (and (get-binding el-1 bindings)
                         (equalp (get-binding el-1 bindings) el-2)))
                   (;; when el-1 is bound to X, and el-2 != X, fail
                    (and (get-binding el-1 bindings)
                         (not (equalp (get-binding el-1 bindings) el-2)))
                    (return-from predicate-elements 'fail))
                   (;; when el-2 is bound to Y, and el-1 = Y, continue
                    (and (get-binding el-2 (reverse-bindings bindings))
                         (equalp (get-binding el-1 (reverse-bindings bindings)) el-1)))
                   (;; when el-2 is bound to Y, and el-1 != Y, fail
                    (and (get-binding el-2 (reverse-bindings bindings))
                         (not (equalp (get-binding el-2 (reverse-bindings bindings)) el-1)))
                    (return-from predicate-elements 'fail))
                   (;; otherwise, make a new binding (el-1 . el-2)
                    t (setf bindings (extend-bindings el-1 el-2 bindings))))
          finally (return-from predicate-elements bindings))
    'fail))


(defun gen (G1 G2)
  "Returns all pairs of predicates in G1 and G2 that match.
   Format: '((predicate-g1 predicate-g2 bindings) ...)"
  (loop for predicate-g1 in G1
        append (loop for predicate-g2 in G2
                     for bindings = (matching-predicates-p predicate-g1 predicate-g2)
                     unless (eql bindings 'fail)
                     collect (list predicate-g1 predicate-g2 bindings))))


(defun compatible-pairs-p (pair-1 pair-2)
  "Returns t if pair-1 and pair-2 are compatible in terms of bindings."
  (and (not (equal (first pair-1) (first pair-2)))
       (not (equal (second pair-1) (second pair-2)))
       (loop with pair-1-bindings = (third pair-1)
             with pair-2-bindings = (third pair-2)
             with pair-2-bindings-reversed = (reverse-bindings (third pair-2))
             for binding in pair-1-bindings
             never (or
                    (and (get-binding (first binding) pair-2-bindings)
                         (not (equalp (cdr binding)
                                      (cdr (get-binding (first binding) pair-2-bindings)))))
                    (and (get-binding (cdr binding) pair-2-bindings-reversed)
                         (not (equalp (car binding)
                                      (cdr (get-binding (cdr binding) pair-2-bindings-reversed)))))))))


(defun compatible-with-set-of-pairs (set-of-pairs pair-to-check)
  "Return t if pair-to-check is compatible with all pairs in set-of-pairs"
  (loop for pair in set-of-pairs
        always (compatible-pairs-p pair pair-to-check)))


(defclass lcg-alignment-state ()
  ((candidates :accessor candidates :initarg :candidates :initform nil)
   (phi :accessor phi :initarg :phi :initform nil)))