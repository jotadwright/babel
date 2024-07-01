(in-package :au-benchmark.msg.1swap-omega)

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


;;;; Definition of Delta_I and Delta_E1
(defun delta-intern (set-of-pairs)
  "Count all occurrences of bindings with multiplicity > 1 in set-of-pairs"
  (let ((all-bindings (mappend #'third set-of-pairs)))
    (loop for binding in all-bindings
          when (> (count binding all-bindings :test #'equal) 1)
          count binding)))

(defun singleton-bindings (set-of-bindings)
  "Given a set of bindings, return all bindings with multiplicity = 1"
  (loop for binding in set-of-bindings
        when (= (count binding set-of-bindings :test #'equal) 1)
        collect binding))

(defun delta-extern-1 (source-set-of-pairs target-set-of-pairs)
  "Count the single occurrence of all bindings with multiplicity 1
   in source-set-of-pairs that have at least an occurrence in
   target-set-of-pairs."
  (let* ((all-source-bindings (mappend #'third source-set-of-pairs))
         (source-singletons (singleton-bindings all-source-bindings))
         (all-target-bindings (mappend #'third target-set-of-pairs)))
    (loop for binding in source-singletons
          when (member binding all-target-bindings :test #'equal)
          count binding)))



;;;; delta-i helper functions
(defun init-delta-is (gen)
  "Pre-compute the delta-i values for all pairs in gen.
   Store only the cardinality of the set, not the set itself."
  (let ((delta-i-table (make-hash-table :test #'equal)))
    (dolist (pair gen)
      (setf (gethash pair delta-i-table)
             (delta-intern (list pair))))
    delta-i-table))

(defun get-delta-i (delta-i pair)
  (gethash pair delta-i))

(defun set-delta-i (delta-i pair value)
  (setf (gethash pair delta-i) value))


;;;; omega functions
(defun init-omega (gen)
  "Initialize the omega values for all pairs in gen.
   The initial omega value for a pair is its delta-i
   value. The omega vector contains at any point the
   quality of a pair w.r.t. the current generalisation."
  (let ((omega (make-hash-table :test #'equal)))
    (dolist (pair gen)
      (setf (gethash pair omega)
            (delta-intern (list pair))))
    omega))

(defun get-omega (omega pair)
  (gethash pair omega))

(defun set-omega (omega pair value)
  (setf (gethash pair omega) value))

(defun update-omega (omega phi phi-delta-i delta-is gen)
  "Update the omega value of all pairs w.r.t.
   the new phi and its Delta_I value."
  (let ((pairs-to-update gen))
    (dolist (pair pairs-to-update)
      (set-omega omega pair
                 (+ phi-delta-i
                    (get-delta-i delta-is pair)
                    (delta-extern-1 phi (list pair))
                    (delta-extern-1 (list pair) phi)))))
  omega)


(defun update-phi-delta-i (phi phi-delta-i delta-is A-pair &optional B-pair)
  "Compute the new Delta_I value of phi when
   (i) A-pair was added to phi, or
   (ii) B-pair was swapped with A-pair"
  (let ((new-delta-i phi-delta-i)
        (phi-minus-B-pair (remove B-pair phi :test #'equal)))
    (setf new-delta-i
          (if (and A-pair B-pair)
            (+ phi-delta-i
               (- (get-delta-i delta-is B-pair))
               (- (delta-extern-1 phi-minus-B-pair (list B-pair)))
               (- (delta-extern-1 (list B-pair) phi-minus-B-pair))
               (get-delta-i delta-is A-pair)
               (delta-extern-1 phi-minus-B-pair (list A-pair))
               (delta-extern-1 (list A-pair) phi-minus-B-pair))
            (+ phi-delta-i
               (get-delta-i delta-is A-pair)
               (delta-extern-1 phi (list A-pair))
               (delta-extern-1 (list A-pair) phi))))
    new-delta-i))


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
    


;;;; select swappable pairs
(defun improves-quality-p (B-pair C-pair phi-minus-B-pair phi-delta-i delta-is)
  "Return t if swapping B-pair with C-pair
   will yield a generalisation of better quality"
  (> (+ phi-delta-i
        (- (get-delta-i delta-is B-pair))
        (- (delta-extern-1 phi-minus-B-pair (list B-pair)))
        (- (delta-extern-1 (list B-pair) phi-minus-B-pair))
        (get-delta-i delta-is C-pair)
        (delta-extern-1 phi-minus-B-pair (list C-pair))
        (delta-extern-1 (list C-pair) phi-minus-B-pair))
     phi-delta-i))

(defun select-B-pair-C-pair (phi phi-delta-i gen omega delta-is)
  "Select a B-pair from phi and a C-pair from comp
   such that swapping B-pair with C-pair yields a generalisation
   of higher quality."
  (loop for B-pair in (sort phi #'<
                            :key #'(lambda (pair)
                                     (- (get-omega omega pair)
                                        (+ (get-delta-i delta-is pair)
                                           (delta-extern-1 (remove pair phi :test #'equal) (list pair))
                                           (delta-extern-1 (list pair) (remove pair phi :test #'equal))))))
        for comp-set = (comp (remove B-pair phi :test #'equal) gen)
        for phi-minus-B-pair = (remove B-pair phi :test #'equal)
        do (loop for C-pair in (sort comp-set #'> :key #'(lambda (pair) (get-omega omega pair)))
                 when (improves-quality-p B-pair C-pair phi-minus-B-pair phi-delta-i delta-is)
                 do (return-from select-B-pair-C-pair (values B-pair C-pair)))
        finally (return (values 'fail 'fail))))


;;;; 1-swap generalise
(defun 1-swap-generalise (G1 G2 &key allow-generalisation-over-constants)
  (loop with phi = nil  ;; start with an empty generalisation
        with phi-delta-i = 0  ;; quality of current generalisation
        with gen = (gen G1 G2
                        :allow-generalisation-over-constants
                        allow-generalisation-over-constants)
        ;; initialize omega values for all pairs in gen
        ;; initial omega value is Delta_I(pair)
        with omega = (init-omega gen)
        ;; pre-compute Delta_I values for all pairs in gen
        with delta-is = (init-delta-is gen)
        ;; select pairs from gen that are compatible with current phi
        with comp = (comp phi gen)
        ;; select the best A-pair from comp according to quality
        for A-pair = (the-biggest #'(lambda (pair) (get-omega omega pair)) comp)
        ;; repeat until there is no more A-pair to add
        ;; i.e. we have reached the generalisation of maximum length
        while A-pair
        ;; Add A-pair to phi and update values accordingly
        do (setf phi-delta-i (update-phi-delta-i phi phi-delta-i delta-is A-pair)
                 phi (adjoin A-pair phi :test #'equal)
                 omega (update-omega omega phi phi-delta-i delta-is gen)
                 comp (comp phi gen))
        ;; DEBUG: print the Delta_I value of phi
        ;do (progn
        ;     (assert (= phi-delta-i (delta-intern phi)))
        ;     (format t "~%(Add) Current phi has length ~a" (length phi))
        ;     (format t "~%(Add) Current phi has Delta_I value of ~a" (delta-intern phi))
        ;     (format t "~%(Add) Should be the same as ~a" phi-delta-i)
        ;     (format t "~%(Add) Current phi is ~a~%" (mkstr phi)))
        ;; after adding A-pair, perform 1-swap(s) that improve the quality
        do (loop for (B-pair C-pair) = (multiple-value-list (select-B-pair-C-pair phi phi-delta-i gen omega delta-is))
                 unless (and (eql B-pair 'fail) (eql C-pair 'fail))
                 do (setf phi-delta-i (update-phi-delta-i phi phi-delta-i delta-is C-pair B-pair)
                          phi (do-replacement phi (list B-pair) (list C-pair))
                          omega (update-omega omega phi phi-delta-i delta-is gen)
                          comp (comp phi gen))
                    ;; DEBUG: print the Delta_I value of phi
                    ;(progn
                    ;  (assert (= phi-delta-i (delta-intern phi)))
                    ;  (format t "~%(Swap) Current phi has length ~a" (length phi))
                    ;  (format t "~%(Swap) Current phi has Delta_I value of ~a" (delta-intern phi))
                    ;  (format t "~%(Swap) Should be the same as ~a" phi-delta-i)
                    ;  (format t "~%(Swap) Current phi is ~a~%" (mkstr phi)))
                 until (and (eql B-pair 'fail) (eql C-pair 'fail)))
        ;; finally return the generalisation phi
        ;; it's Delta_I value (number of shared variables/bindings)
        finally
          (return (values phi phi-delta-i))))


#|
(defparameter *G1* '((add ?x ?y ?z) (even ?x) (odd ?z) (p ?z)))
(defparameter *G2* '((add ?a ?b ?c) (add ?c ?b ?a) (even ?c) (odd ?a) (p ?c)))

(1-swap-generalise (shuffle *G1*) (shuffle *G2*))
(k-swap-generalise (shuffle *G1*) (shuffle *G2*) :k 2 :W 1)
|#


#|
(defparameter *class-1-G1*
  '((F ?E-3)
    (G ?G-3 ?F-3) (G ?C-3 ?B-3) (G ?G-3 ?E-3)
    (H ?D-3 ?G-3 ?C-3) (H ?E-3 ?G-3 ?A-3) (H ?A-3 ?C-3 ?E-3) (H ?B-3 ?F-3 ?G-3)))

(defparameter *class-1-G2*
  '((F ?ETA-3) (F ?BETA-3) (F ?EPSILON-3)
    (G ?BETA-3 ?ALPHA-3) (G ?ALPHA-3 ?ALPHA-3) (G ?DELTA-3 ?GAMMA-3) (G ?BETA-3 ?GAMMA-3)
    (G ?EPSILON-3 ?GAMMA-3) (G ?ZETA-3 ?EPSILON-3) (G ?GAMMA-3 ?ALPHA-3)
    (H ?ALPHA-3 ?GAMMA-3 ?BETA-3)))

(loop repeat 5000 do (1-swap-generalise (shuffle *class-1-G1*) (shuffle *class-1-G2*)))
(loop repeat 10 do (time (k-swap-generalise (shuffle *class-1-G1*) (shuffle *class-1-G2*) :k 4 :W 4)))
|#


(defun identify-k-swap-alignments (pattern source &key k W allow-generalisation-over-constants)
  (declare (ignore k W))
  (multiple-value-bind (k-swap-stable-phi gen-size phi-delta-i)
      (1-swap-generalise pattern source :allow-generalisation-over-constants allow-generalisation-over-constants)
    (declare (ignore phi-delta-i))
    (values
     (list
      (make-instance 'predicate-alignment-state
                     :pattern-predicates (mapcar #'first k-swap-stable-phi)
                     :source-predicates (mapcar #'second k-swap-stable-phi)
                     :pattern-delta (set-difference pattern (mapcar #'first k-swap-stable-phi) :test #'equal)
                     :source-delta (set-difference source (mapcar #'second k-swap-stable-phi) :test #'equal)))
     gen-size)))


(defun anti-unify-predicate-networks (pattern source &key k W allow-generalisation-over-constants)
  "Anti-unify pattern and source. Returns all solutions of the lowest cost,
   their cost value, and the number of alignments that were found."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with (possible-alignments gen-size)
        = (multiple-value-list
           (identify-k-swap-alignments pattern source :k k :W W
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
                                gen-size))))



