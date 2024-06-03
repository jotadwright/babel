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
(k-swap-generalise (shuffle *class-1-G1*) (shuffle *class-1-G2*) :k 2 :W 2 :x nil :y nil :z nil)
|#

(defun valid-generalisation-p (phi)
  (loop for subset on phi
        always (compatible-with-set-of-pairs (rest subset) (first subset))))


(defun k-swap-stable-p (phi phi-delta-i gen k W)
  (loop with queue = nil
        for state = (pop queue)
        for phi-s = (first state)
        for phi-c = (second state)
        for phi-prime = (or (third state) phi)
        for all-swap-candidates
          = (loop for B-pair in (set-difference phi phi-c :test #'equal)
                  for phi-minus-B-pair = (remove B-pair phi-prime :test #'equal)
                  for comp-set = (comp phi-minus-B-pair gen)
                  append (loop for C-pair in (set-difference comp-set phi-s :test #'equal)
                               for new-phi-prime = (do-replacement (copy-list phi-prime) (list B-pair) (list C-pair))
                               for new-delta-i = (delta-intern new-phi-prime)
                               when (and (< (length phi-s) k)
                                         (< (length phi-c) k))
                               collect (list (cons B-pair phi-s)
                                             (cons C-pair phi-c)
                                             new-phi-prime
                                             new-delta-i)))
        for W-swap-candidates = (take-bins all-swap-candidates W #'fourth)
        do (loop for candidate in W-swap-candidates
                 if (> (fourth candidate) phi-delta-i)
                 do (return-from k-swap-stable-p nil)
                 else do (push candidate queue))
        until (null queue)
        finally (return t)))
        


(defun take-bins (set-of-pairs number-of-bins quality-fn &key (best t))
  "Group the pairs in set-of-pairs based on their omega values
   and take a number of groups with the best/worst omega values. 
   When number-of-bins is nil, take all pairs."
  (cond ((null set-of-pairs) nil)
        ((= (length set-of-pairs) 1) set-of-pairs)
        (t
         (let ((pairs-grouped-by-quality-and-sorted
                (sort
                 (copy-list
                  (group-by set-of-pairs quality-fn :test #'=))
                 (if best #'> #'<)
                 :key #'car)))
           (mappend #'cdr
                    (cond ((null number-of-bins) pairs-grouped-by-quality-and-sorted)
                          ((> number-of-bins (length pairs-grouped-by-quality-and-sorted))
                           pairs-grouped-by-quality-and-sorted)
                          (t (subseq pairs-grouped-by-quality-and-sorted 0 number-of-bins))))))))


(defun select-phi-s-phi-c (phi phi-delta-i gen delta-Is k W x y z)
  "Select phi-s (a subset of phi) and phi-c (a subset of gen) to swap"
  (let ((initial-state (list nil nil phi phi-delta-i)))
    (loop with best-swap = initial-state
          with queue = (list initial-state)
          with improvement-found? = nil
          ;; take the next possible swap from the queue
          for swap-state = (pop queue)
          for phi-s = (first swap-state)
          for phi-c = (second swap-state)
          for phi-prime = (third swap-state)
          for phi-prime-delta-i = (fourth swap-state)
          ;; check if a new best state is found
          do (when (> phi-prime-delta-i (fourth best-swap))
               (setf improvement-found? t)
               (setf best-swap swap-state))
          ;; expand the current swap state (taking into account k and W)
          do (when (and (< (length phi-s) k) (< (length phi-c) k))
               (let* ((all-swap-candidates
                       (loop for B-pair in (set-difference phi-prime phi-c :test #'equal) ;; don't try to remove things that are just added
                             for phi-minus-B-pair = (remove B-pair phi-prime :test #'equal)
                             for comp-set = (comp phi-minus-B-pair gen)
                             append (loop for C-pair in (set-difference comp-set phi-s :test #'equal) ;; dont try to add things that are just removed
                                          for new-delta-i = (update-phi-delta-i phi-prime phi-prime-delta-i delta-Is C-pair B-pair)
                                          if (null x)
                                            collect (list B-pair C-pair new-delta-i)
                                          else if (and x (>= new-delta-i phi-prime-delta-i))
                                            collect (list B-pair C-pair new-delta-i))))
                      ;; select the W best swaps based on Delta_I value
                      (W-swap-candidates (take-bins all-swap-candidates W #'third)))
                 (loop for (B-pair C-pair new-delta-i) in W-swap-candidates
                       do (push (list (cons B-pair phi-s)
                                      (cons C-pair phi-c)
                                      (do-replacement (copy-list phi-prime) (list B-pair) (list C-pair))
                                      new-delta-i)
                                queue))))
          ;; sort the queue
          when z do (setf queue (sort queue #'> :key #'fourth))  ;; use a priority queue instead of regular queue?
          ;; repeat until the queue is empty
          ;until (null queue)
          until (if y (or improvement-found? (null queue)) (null queue))  ;; return the very first swap that improves the quality?
          ;; finally, return the best possible swap (if any)
          finally
            (return
             (if (equal best-swap initial-state)
               (values 'fail 'fail 'fail 'fail)
               (values (first best-swap) (second best-swap) (third best-swap) (fourth best-swap)))))))


(defun k-swap-generalise (G1 G2 &key k W allow-generalisation-over-constants x y z)
  (loop with phi = nil
        with phi-delta-i = 0
        with gen = (gen G1 G2 :allow-generalisation-over-constants
                        allow-generalisation-over-constants)
        with omega = (init-omega gen)
        with delta-Is = (init-delta-is gen)
        for comp = (comp phi gen)
        for A-pair = (the-biggest #'(lambda (pair) (get-omega omega pair)) comp)
        while A-pair
        do (setf phi-delta-i (update-phi-delta-i phi phi-delta-i delta-is A-pair)
                 phi (adjoin A-pair phi :test #'equal)
                 omega (update-omega omega phi phi-delta-i delta-Is gen))
        ;do (format t "~%Add - Delta_I = ~a" phi-delta-i)
        do (loop for (phi-s phi-c new-phi new-delta-i)
                 = (multiple-value-list
                    (select-phi-s-phi-c phi phi-delta-i gen delta-Is k W x y z))
                 unless (and (eql phi-s 'fail) (eql phi-c 'fail))
                 do ;(format t "~%Swap (size ~a) - Delta_I = ~a" (length phi-s) new-delta-i)
                    (setf phi-delta-i new-delta-i
                          phi new-phi
                          omega (update-omega omega phi phi-delta-i delta-Is gen))
                 until (and (eql phi-s 'fail) (eql phi-c 'fail)))
        ;do (assert (= phi-delta-i (delta-intern phi)))
        ;do (assert (k-swap-stable-p phi phi-delta-i gen k))
        finally
          (progn
            ;(assert (k-swap-stable-p phi phi-delta-i gen k W))
            (return (values phi (length gen) phi-delta-i)))))


(defun identify-k-swap-alignments (pattern source &key k W allow-generalisation-over-constants x y z)
  (multiple-value-bind (k-swap-stable-phi gen-size phi-delta-i)
      (k-swap-generalise pattern source :k k :W W
                        :allow-generalisation-over-constants
                         allow-generalisation-over-constants
                         :x x :y y :z z)
    (declare (ignore phi-delta-i))
    (values
     (list
      (make-instance 'predicate-alignment-state
                     :pattern-predicates (mapcar #'first k-swap-stable-phi)
                     :source-predicates (mapcar #'second k-swap-stable-phi)
                     :pattern-delta (set-difference pattern (mapcar #'first k-swap-stable-phi) :test #'equal)
                     :source-delta (set-difference source (mapcar #'second k-swap-stable-phi) :test #'equal)))
     gen-size)))


(defun anti-unify-predicate-networks (pattern source &key k W allow-generalisation-over-constants x y z)
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
                                       allow-generalisation-over-constants
                                       :x x :y y :z z))
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



