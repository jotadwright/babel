(in-package :au-benchmark.msg.kswap-omega)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ;;
;; K-swap stability abstraction w.r.t. ;;
;; a quality function Omega            ;;                            
;;                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;; multiset operations
(defun munion (mset-1 mset-2 &key (test #'eql))
  "The union of two multisets A and B is a multiset such that the
   multiplicity of an element is equal to the maximum of the
   multiplicity of an element in A and B"
  (let ((unique-union (union (remove-duplicates mset-1 :test test)
                             (remove-duplicates mset-2 :test test)
                             :test test)))
    (loop for elem in unique-union
          for count-1 = (count elem mset-1 :test test)
          for count-2 = (count elem mset-2 :test test)
          for max = (max count-1 count-2)
          append (loop repeat max collect elem))))

(defun mset-difference (mset-1 mset-2 &key (test #'eql))
  "The difference of two multisets A and B, is a multiset such that the
   multiplicity of an element is equal to the multiplicity of the element
   in A minus the multiplicity of the element in B if the difference is positive,
   and is equal to 0 if the difference is 0 or negative"
  (let ((mset-1-unique-elements (remove-duplicates mset-1 :test test)))
    (loop for elem in mset-1-unique-elements
          for count-1 = (count elem mset-1 :test test)
          for count-2 = (count elem mset-2 :test test)
          for diff = (- count-1 count-2)
          when (plusp diff)
          append (loop repeat diff collect elem))))

(defun msum (mset-1 mset-2 &key (test #'eql))
  "The sum of two multisets A and B, is a multiset such that the multiplicity
   of an element is equal to the sum of the multiplicity of an element in A and B"
  (declare (ignore test))
  (append mset-1 mset-2))



(defun delta (source-set-of-pairs target-set-of-pairs)
  "Characterise the number of shared bindings gained/lost when
   adding/removing source set of pairs to/from target set of pairs."
  (msum (delta-intern source-set-of-pairs)
        (delta-extern source-set-of-pairs target-set-of-pairs)))

(defun delta-intern (set-of-pairs)
  "Count the internal shared bindings"
  (let ((all-bindings (mappend #'third set-of-pairs)))
    (loop for binding in all-bindings
          when (> (count binding all-bindings :test #'equal) 1)
          collect binding)))

(defun delta-extern (source-set-of-pairs target-set-of-pairs)
  "Count the external shared bindings"
  (let ((all-source-bindings (mappend #'third source-set-of-pairs))
        (all-target-bindings (mappend #'third target-set-of-pairs)))
    (loop for binding in all-source-bindings
          when (find binding all-target-bindings :test #'equal)
          collect binding)))
  


;;;; delta-e functions
(defun init-delta-e (gen)
  "Pre-compute the delta-e values for all pairs
   of pairs in gen. Store only the cardinality
   of the set, not the set itself."
  (let ((delta-e-table (make-hash-table :test #'equal)))
    (dolist (pair-1 gen)
      (let ((pair-1-table (make-hash-table :test #'equal)))
        (dolist (pair-2 gen)
          (setf (gethash pair-2 pair-1-table)
                (length (delta-extern (list pair-1) (list pair-2)))))
        (setf (gethash pair-1 delta-e-table)
              pair-1-table)))
    delta-e-table))

(defun get-delta-e (delta-e pair-1 pair-2)
  (gethash pair-2 (gethash pair-1 delta-e)))

(defun set-delta-e (delta-e pair-1 pair-2 value)
  (setf (gethash pair-2 (gethash pair-1 delta-e)) value))


;;;; omega functions
(defun init-omega (gen)
  "Initialize the omega values for all pairs in gen.
   The initial omega value for a pair is its delta-i
   value, i.e. the amount of shared variables within
   the pair itself, e.g. (f ?a ?a)."
  (let ((omega (make-hash-table :test #'equal)))
    (dolist (pair gen)
      (setf (gethash pair omega)
            (length (delta-intern (list pair)))))
    omega))

(defun get-omega (omega pair)
  (gethash pair omega))

(defun set-omega (omega pair value)
  (setf (gethash pair omega) value))

(defun update-omega (omega delta-e gen added-pair &optional removed-pair)
  (let ((pairs-to-update gen))
    (dolist (A-pair pairs-to-update)
      (set-omega omega A-pair
                 (- (+ (get-omega omega A-pair)
                       (get-delta-e delta-e A-pair added-pair))
                    (if removed-pair (get-delta-e delta-e A-pair removed-pair) 0)))))
  omega)
        

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
(defun do-replacement (phi B-pair C-pair)
  "Replace B-pair with C-pair in phi"
  (adjoin C-pair (remove B-pair phi :test #'equal) :test #'equal))
  ;(union (set-difference phi (list B-pair) :test #'equal) (list C-pair) :test #'equal))


;;;; select swappable pairs
(defun improves-quality-p (B-pair C-pair omega delta-e)
  "Return t if swapping B-pair with C-pair
   will yield a generalisation of better quality"
  (> (- (get-omega omega C-pair)
        (get-delta-e delta-e C-pair B-pair))
     (- (get-omega omega B-pair)
        (get-delta-e delta-e B-pair B-pair))))

(defun select-B-pair-C-pair (phi gen omega delta-e)
  "Select a B-pair from phi and a C-pair from comp
   such that swapping B-pair with C-pair yields a generalisation
   of higher quality."
  ;; omega-value of pair is the number of shared variables/bindings it would contribute when adding
  ;; pair to the current phi. however, B-pair is already in phi, so we subtract the
  ;; delta-e value of the pair with itself from the current omega-value of B-pair

  ;; using the set comp to select C-pair is not correct here. For every selected B-pair, we have
  ;; to recompute the comp set with the assumption that B-pair will be removed from phi!
  (loop for B-pair in (sort phi #'< :key #'(lambda (pair) (- (get-omega omega pair) (get-delta-e delta-e pair pair))))
        for comp-set = (comp (remove B-pair phi :test #'equal) gen)
        do (loop for C-pair in (sort comp-set #'> :key #'(lambda (pair) (get-omega omega pair)))
                 when (improves-quality-p B-pair C-pair omega delta-e)
                 do (return-from select-B-pair-C-pair (values B-pair C-pair)))
        finally (return (values 'fail 'fail))))


;;;; k-swap generalise
(defun k-swap-generalise (G1 G2 &key allow-generalisation-over-constants)
  (loop with phi = nil  ;; start with an empty generalisation
        with gen = (gen G1 G2
                        :allow-generalisation-over-constants
                        allow-generalisation-over-constants)
        ;; initialize omega values for all pairs in gen
        ;; initial omega value is Delta_I(pair)
        with omega = (init-omega gen)
        ;; pre-compute delta_E values for all pairs of pairs in gen
        with delta-e = (init-delta-e gen)
        ;; select pairs from gen that are compatible with current phi
        with comp = (comp phi gen)
        ;; select the best A-pair from comp according to quality
        for A-pair = (the-biggest #'(lambda (pair) (get-omega omega pair)) comp)
        ;; repeat until there is no more A-pair to add
        ;; i.e. we have reached the generalisation of maximum length
        while A-pair
        ;; Add A-pair to phi and update values accordingly
        do (setf phi (adjoin A-pair phi :test #'equal)
                 omega (update-omega omega delta-e gen A-pair)
                 comp (comp phi gen))
        ;; DEBUG: print the Delta_I value of phi
        do (progn
             (format t "~%(Add) Current phi has length ~a" (length phi))
             (format t "~%(Add) Current phi has Delta_I value of ~a" (length (delta-intern phi)))
             (format t "~%(Add) Current phi is ~a~%" (mkstr phi)))
        ;; after adding A-pair, perform 1-swap(s) that improve the quality
        do (loop for (B-pair C-pair) = (multiple-value-list (select-B-pair-C-pair phi gen omega delta-e))
                 unless (and (eql B-pair 'fail) (eql C-pair 'fail))
                 do (setf phi (do-replacement phi B-pair C-pair)
                          omega (update-omega omega delta-e gen C-pair B-pair)
                          comp (comp phi gen))
                    ;; DEBUG: print the Delta_I value of phi
                    (progn
                      (format t "~%(Swap) Current phi has length ~a" (length phi))
                      (format t "~%(Swap) Current phi has Delta_I value of ~a" (length (delta-intern phi)))
                      (format t "~%(Swap) Current phi is ~a~%" (mkstr phi)))
                 until (and (eql B-pair 'fail) (eql C-pair 'fail)))
        ;; finally return the generalisation phi
        ;; it's Delta_I value (number of shared variables/bindings)
        finally
          (return (values phi (length (delta-intern phi))))))



(defun identify-k-swap-alignments (pattern source &key allow-generalisation-over-constants)
  (multiple-value-bind (k-swap-stable-phi delta-i) (k-swap-generalise pattern source
                                                                      :allow-generalisation-over-constants
                                                                      allow-generalisation-over-constants)
    (values
     (list
      (make-instance 'predicate-alignment-state
                     :pattern-predicates (mapcar #'first k-swap-stable-phi)
                     :source-predicates (mapcar #'second k-swap-stable-phi)
                     :pattern-delta (set-difference pattern (mapcar #'first k-swap-stable-phi) :test #'equal)
                     :source-delta (set-difference source (mapcar #'second k-swap-stable-phi) :test #'equal)))
     delta-i)))


(defun anti-unify-predicate-networks (pattern source &key allow-generalisation-over-constants)
  "Anti-unify pattern and source. Returns all solutions of the lowest cost,
   their cost value, and the number of alignments that were found."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with (possible-alignments delta-i)
        = (multiple-value-list
           (identify-k-swap-alignments pattern source
                                       :allow-generalisation-over-constants
                                       allow-generalisation-over-constants))
        for alignment in possible-alignments
        for pattern-in-alignment = (pattern-predicates alignment)
        for source-in-alignment = (source-predicates alignment)
        for pattern-delta = (pattern-delta alignment)
        for source-delta = (source-delta alignment)
        collect (multiple-value-bind (resulting-generalisation
                                      resulting-pattern-bindings
                                      resulting-source-bindings
                                      resulting-pattern-delta
                                      resulting-source-delta)
                    (anti-unify-predicate-sequence pattern-in-alignment source-in-alignment nil nil nil pattern-delta source-delta)
                  (make-instance 'anti-unification-result
                                 :pattern pattern
                                 :source source
                                 :generalisation resulting-generalisation
                                 :pattern-bindings resulting-pattern-bindings
                                 :source-bindings resulting-source-bindings
                                 :pattern-delta resulting-pattern-delta
                                 :source-delta resulting-source-delta
                                 :cost (anti-unification-cost pattern source
                                                              resulting-generalisation
                                                              resulting-pattern-delta
                                                              resulting-source-delta
                                                              resulting-pattern-bindings
                                                              resulting-source-bindings
                                                              :msg)))
          into results
        ;; Sort results based on increasing cost.
        finally (return (values (sort results #'< :key #'cost)
                                (cost (first results))
                                delta-i))))



