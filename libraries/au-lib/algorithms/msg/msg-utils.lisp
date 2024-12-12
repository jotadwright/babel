(in-package :au-lib)

(export '(matching-predicates
          msg-alignment-state
          pattern-predicates
          source-predicates
          pattern-delta
          source-delta
          pattern-remaining
          source-remaining
          gen
          delta-intern
          singleton-bindings
          delta-extern
          pre-compute-delta-i
          get-delta-i
          set-delta-i
          init-gains
          set-gain
          get-gain
          recompute-gains
          compatible-pairs-p
          compatible-with-set-of-pairs
          comp
          do-replacement))


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

;;;;
;;;; Utils for all k-swap implementations
;;;;

;;;; gen
(defun gen (G1 G2 &key allow-generalisation-over-constants)
  "Returns all pairs of predicates in G1 and G2 that match.
   Format: '((predicate-g1 predicate-g2 bindings) ...)"
  (loop for predicate-g1 in G1
        append (loop for predicate-g2 in G2
                     when (matching-predicates predicate-g1 predicate-g2
                                               :allow-generalisation-over-constants
                                               allow-generalisation-over-constants)
                     collect (list predicate-g1 predicate-g2
                                   (pairlis (rest predicate-g1) (rest predicate-g2))))))

;;;; Definitions of Delta_I and Delta_E1
(defun delta-intern (set-of-pairs)
  "Count all occurrences of bindings with multiplicity > 1 in set-of-pairs"
  (let ((all-bindings (mappend #'third set-of-pairs))
        (counts (make-hash-table :test #'equal)))
    (dolist (binding all-bindings)
      (incf (gethash binding counts 0)))
    (loop for count being the hash-value of counts
          when (> count 1)
          sum count)))

(defun singleton-bindings (set-of-bindings)
  "Given a set of bindings, return all bindings with multiplicity = 1"
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (binding set-of-bindings)
      (incf (gethash binding counts 0)))
    (loop for binding being the hash-key of counts
          using (hash-value count)
          when (= count 1)
          collect binding)))

(defun delta-extern (source-set-of-pairs target-set-of-pairs)
  "Count the single occurrence of all bindings with multiplicity 1
   in source-set-of-pairs that have at least an occurrence in
   target-set-of-pairs."
  (let* ((all-source-bindings (mappend #'third source-set-of-pairs))
         (source-singletons (singleton-bindings all-source-bindings))
         (all-target-bindings (mappend #'third target-set-of-pairs)))
    (loop for binding in source-singletons
          when (member binding all-target-bindings :test #'equal)
          count binding)))

#|
;;;; Pre-compute Delta_I of all pairs in gen
;;;; + accessor functions
(defun pre-compute-delta-i (gen)
  "Pre-compute the Delta_I values for all pairs in gen.
   Store only the cardinality of the set, not the set itself."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair gen)
      (setf (gethash pair table)
            (delta-intern (list pair))))
    table))

(defun get-delta-i (delta-i pair)
  (gethash pair delta-i))

(defun set-delta-i (delta-i pair value)
  (setf (gethash pair delta-i) value))
|#


;;;; Gain
;;;; For each pair in gen, the increase of Delta_I
;;;; of that pair w.r.t. the current generalisation
(defun init-gains (gen)
  "Initialize the gain for all pairs in gen.
   The initial gain for a pair is its Delta_I value.
   The gain table contains at any point the increase
   in quality that a pair can have w.r.t. to the
   current generalisation."
  (let ((gain (make-hash-table :test #'equal)))
    (dolist (pair gen)
      (setf (gethash pair gain)
            (delta-intern (list pair))))
    gain))

(defun get-gain (gain pair)
  (gethash pair gain))

(defun set-gain (gain pair value)
  (setf (gethash pair gain) value))

(defun recompute-gains (gains phi gen delta-is)
  "Recompute the gains of all pairs in gen"
  ;; TO DO; how can this be updated incrementally?
  (dolist (pair gen)
    (let ((phi-without-pair (remove pair phi :test #'equal)))
      (set-gain gains pair
                (+ (get-delta-i delta-is pair)
                   (delta-extern (list pair) phi-without-pair)
                   (delta-extern phi-without-pair (list pair))))))
  gains)

#|
(defun update-gains (gains gen A-pair &optional B-pair)
  "Update the gain of each pair in gen when
   (i) A-pair was added to phi, or
   (ii) B-pair was swapped with A-pair"
  (if (and A-pair B-pair)
    (dolist (pair gen)
      (set-gain gains pair
                (cond ((equal pair A-pair)
                       (- (get-gain gains pair)
                          (delta-extern (list pair) (list B-pair))
                          (delta-extern (list B-pair) (list pair))))
                      ((equal pair B-pair)
                       (+ (get-gain gains pair)
                          (delta-extern (list pair) (list A-pair))
                          (delta-extern (list A-pair) (list pair))))
                      (t (+ (get-gain gains pair)
                            (- (delta-extern (list pair) (list B-pair)))
                            (- (delta-extern (list B-pair) (list pair)))
                            (delta-extern (list pair) (list A-pair))
                            (delta-extern (list A-pair) (list pair)))))))
    (dolist (pair (set-difference gen (list A-pair) :test #'equal))
      (set-gain gains pair
                (+ (get-gain gains pair)
                   (delta-extern (list pair) (list A-pair))
                   (delta-extern (list A-pair) (list pair))))))
  gains)
|#
                  


;;;; comp
(defun compatible-pairs-p (pair-1 pair-2)
  "A pairing is a bijective mapping, i.e. every predicate from
   G1 and every predicate from G2 can be used only once."
  (and (not (equal (first pair-1) (first pair-2)))
       (not (equal (second pair-1) (second pair-2)))))

(defun compatible-with-set-of-pairs (set-of-pairs pair-to-check)
  "Return t if pair-to-check is compatible with all pairs in set-of-pairs."
  (loop for pair in set-of-pairs
        always (compatible-pairs-p pair pair-to-check)))

(defun comp (set-of-pairs set-of-pairs-to-check)
  "Returns part of set-of-pairs-to-check that is readily compatible with set-of-pairs."
  (loop for pair in set-of-pairs-to-check
        if (compatible-with-set-of-pairs set-of-pairs pair)
        collect pair))


;;;; do replacement
(defun do-replacement (phi phi-s phi-c)
  "replace phi-s with phi-c in phi"
  (union (set-difference phi phi-s :test #'equal) phi-c :test #'equal))

