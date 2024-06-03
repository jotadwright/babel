(in-package :cl-user)

(defpackage :k-swap-variable-decoupling
  (:use :common-lisp :cl-user :utils :fcg)
  (:export :k-swap-generalise
           :anti-unify-predicate-network-k-swap))

(in-package :k-swap-variable-decoupling)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;; Experimental implementation of the k-swap optimization  ;;
;; for anti-unifying over sets (Yernaux and Vanhoof 2022)  ;;
;; but with variable decoupling, using bitmaps as          ;;
;; representation (for efficient union, set-diff, and      ;;
;; permutation checking)                                   ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (ql:quickload :fcg)

(defun all-positions (elem array &key (key #'identity) (test #'eql))
  (loop for element across array 
        and position from 0
        when (funcall test (funcall key element) elem)
        collect position))

(defun position-of-ones (bitmap)
  (when bitmap
    (all-positions 1 bitmap)))

(defun number-of-ones (bitmap)
  (count 1 bitmap))

(defun make-bit-array (size &key fill)
  (make-array size :element-type 'bit :initial-element (if fill 1 0)))



(defun gen (G1 G2)
  "Returns all pairs of predicates in G1 and G2 that unify."
  (let ((all-mappings
         (loop for predicate-g1 in G1
               append (loop for predicate-g2 in G2
                            for bindings = (unify-simple predicate-g1 predicate-g2)
                            when bindings collect (list predicate-g1 predicate-g2 bindings)))))
    (make-array (length all-mappings) :initial-contents all-mappings)))


;; (gen '((p a) (p b) (f x)) '((p ?z) (f x)))
;; (length (gen *G1* *G2*))


(defun omega (pair set-of-pairs)
  "Quality estimator of a pair.
   The quality of a pair is determined by the number of
   bindings in set-of-pairs that are in conflict with the
   bindings in pair, divided by the occurrence count of
   the bindngs in pair.
   When more bindings are in conflict proportionally,
   the quality of the pair decreases."
  (let ((occurrence-count 1)
        (conflict-count 0))
    (loop with pair-bindings = (third pair)
          for (nil nil bindings) across set-of-pairs
          do (loop for (x . y) in pair-bindings
                   for pair = (find x bindings :key #'car)
                   if (and pair (not (eql (cdr pair) y)))
                   do (incf occurrence-count)
                      (incf conflict-count)
                   else if pair
                   do (incf occurrence-count)))
    (- 1 (/ conflict-count occurrence-count))))


(defun all-omegas (gen)
  "Compute omega values for all pairs of gen and store
   them in an array."
  (loop with omegas = (make-array (length gen))
        for pair across gen
        for fill-idx from 0
        do (setf (aref omegas fill-idx)
                 (omega pair gen))
        finally (return omegas)))


(defun take-W (bitmap-of-pairs omega-values W &key (best t))
  "Group the pairs in bitmap-of-pairs based on their omega values
   and take pairs with the W best/worst omega values. When W is
   nil, take all pairs."
  (cond (; when there are no pairs, return nil
         (= (count 1 bitmap-of-pairs) 0)
         bitmap-of-pairs)
        (; when there is only one pair, return it
         (= (count 1 bitmap-of-pairs) 1)
         bitmap-of-pairs)
        (t
         (let* ((pair-positions (position-of-ones bitmap-of-pairs))
                (pair-positions-grouped-by-omega-and-sorted
                 (sort
                  (group-by pair-positions
                            #'(lambda (position)
                                (aref omega-values position))
                            :test #'=)
                  (if best #'> #'<)
                  :key #'car))
                (W-best-positions
                 (mappend #'cdr
                          (cond ((null W) pair-positions-grouped-by-omega-and-sorted)
                                ((> W (length pair-positions-grouped-by-omega-and-sorted))
                                 pair-positions-grouped-by-omega-and-sorted)
                                (t (subseq pair-positions-grouped-by-omega-and-sorted 0 W))))))
           (loop with resulting-bitmap = (make-bit-array (length bitmap-of-pairs))
                 for position in W-best-positions
                 do (setf (bit resulting-bitmap position) 1)
                 finally (return resulting-bitmap))))))             


(defun do-replacement (phi phi-s phi-c A-pair)
  "In phi, replace phi-s with the union of phi-c and A-pair."
  (let ((replacement-bitmap (bit-ior (bit-andc2 phi phi-s) phi-c)))
    (setf (bit replacement-bitmap A-pair) 1)
    replacement-bitmap))
  ;(union (union (set-difference phi phi-s :test #'equal) phi-c :test #'equal) (list A-pair) :test #'equal))


(defun compatible-pairs-p (pair-1-position pair-2-position gen)
  "Returns t if pair-1 and pair-2 are compatible.
   'gen' generates multiple mappings between predicates from pattern and source.
   However, each predicate from the input can be used only once in the generalisation.
   So this is checked here. We can ignore the bindings because we don't want the
   injectivity of the original k-swap implementation."
  (let ((pair-1 (aref gen pair-1-position))
        (pair-2 (aref gen pair-2-position)))
    (and (not (equal (first pair-1) (first pair-2)))
         (not (equal (second pair-1) (second pair-2))))))


(defun enforce (bitmap-of-pairs position-to-enforce gen)
  "Adds pair-to-enforce to set-of-pairs, removing any pairs that would become incompatible."
  (loop with resulting-bitmap = (make-bit-array (length gen))
        for pair-position in (position-of-ones bitmap-of-pairs)
        if (compatible-pairs-p pair-position position-to-enforce gen)
        do (setf (bit resulting-bitmap pair-position) 1)
        finally (progn (setf (bit resulting-bitmap position-to-enforce) 1)
                  (return resulting-bitmap))))

;; (enforce '(((p ?x ?y) (p ?a ?b) ((?x . ?a) (?y . ?b))) ((q ?x) (q ?a) ((?x . ?a)))) '((r ?y) (r ?c) ((?y . ?c))))

(defun compatible-with-set-of-pairs (bitmap-of-pairs position-to-check gen)
  "Return t if pair-to-check is compatible with all pairs in set-of-pairs."
  (loop for pair-position in (position-of-ones bitmap-of-pairs)
        always (compatible-pairs-p pair-position position-to-check gen)))

(defun comp (bitmap-of-pairs bitmap-of-pairs-to-check gen)
  "Returns part of set-of-pairs-to-check that is readily compatible with set-of-pairs."
  (loop with resulting-bitmap = (make-bit-array (length gen))
        for pair-position in (position-of-ones bitmap-of-pairs-to-check)
        when (compatible-with-set-of-pairs bitmap-of-pairs pair-position gen)
        do (setf (bit resulting-bitmap pair-position) 1)
        finally (return resulting-bitmap)))

(defun select-phi-s-phi-c (gen gen-bitmap omegas phi A-pair k W)
  "Adds A-pair to phi, if necessary (i.e. if there are incompatibilities) swapping up to k other pairs."
  ;; phi-s: pairs from phi to remove (k = upperbound on length of phi-s)
  ;; phi-c: pairs to replace phi-s in phi: always includes A-pair and has the same length as phi-s
  (let* ((GS nil) ;; stack
         (BS (make-instance 'queue)) ;; queue
         (phi-enforced-A (enforce phi A-pair gen))
         (phi-c (make-bit-array (length gen)))
         (phi-s (bit-andc2 phi phi-enforced-A))
         (S (bit-andc2 gen-bitmap phi-enforced-A)))

    (loop while (and (> (number-of-ones S) 0)
                     (< (number-of-ones phi-c) (number-of-ones phi-s))
                     (<= (number-of-ones phi-s) k))
          do (loop with GS-states = nil
                   for comp-set = (take-W (comp (do-replacement phi phi-s phi-c A-pair) S gen) omegas W)
                   while (and (< (number-of-ones phi-c) (number-of-ones phi-s))
                              (or (> (number-of-ones comp-set) 0) GS))
                   ;; select p to extend phi-c (the subset to replace phi-s)
                   ;; sorted by omega
                   do (loop for p in (position-of-ones comp-set)
                            for enlarged-phi-c = (let ((arr (alexandria:copy-array phi-c))) (setf (bit arr p) 1) arr)
                            for new-S = (let ((arr (alexandria:copy-array S))) (setf (bit arr p) 0) arr)
                            for dup = (find enlarged-phi-c GS-states :key #'car :test #'equal)
                            unless dup do (push (cons enlarged-phi-c new-S) GS))
                      (let* ((stack-pair (pop GS)))
                        (when stack-pair (push stack-pair GS-states))
                        (setf phi-c (if stack-pair (car stack-pair) (make-bit-array (length gen))))
                        (setf S (if stack-pair (cdr stack-pair) (make-bit-array (length gen))))))
             (when (< (number-of-ones phi-c) (number-of-ones phi-s))
               ;; select p to extend phi-s (the subset to remove from phi)
               ;; sorted by omega (inverse)
               (loop with options = (take-W (bit-andc2 phi phi-s) omegas W :best nil)
                     for p in (position-of-ones options)
                     for enlarged-phi-s = (let ((arr (alexandria:copy-array phi-s))) (setf (bit arr p) 1) arr)
                     for dup = (find enlarged-phi-s (elements BS) :test #'equal)
                     unless dup do (enqueue-at-end BS (list enlarged-phi-s)))
               (if (elements BS)
                 (progn
                   (setf phi-s (remove-front BS))  ; (pop BS))
                   (setf phi-c (make-bit-array (length gen)))
                   (setf S (bit-andc2 gen-bitmap phi-enforced-A)))
                 (progn
                   (return-from select-phi-s-phi-c (values 'fail 'fail)))))) ;; unable to find phi-c 

    (if (= (number-of-ones phi-c) (number-of-ones phi-s))
      (values phi-s phi-c)
      (values 'fail 'fail))))


(defun k-swap-generalise (G1 G2 &key (k 5) (W 1))
  (loop with potentially-extensible = t
        with gen = (gen G1 G2)
        with gen-bitmap = (make-bit-array (length gen) :fill t)
        with omegas = (all-omegas gen)
        with phi = (make-bit-array (length gen))
        until (not potentially-extensible)
        ;; in each loop, try to extend phi with A-pair
        ;; consider A-pairs in the order that has the
        ;; least conflicting bindings with all other pairs (as in the paper)
        ;; (alternatively, least conflicts with phi so far)
        do (loop for A-pair in (sort (position-of-ones (bit-andc2 gen-bitmap phi)) #'>
                                     :key (lambda (pos) (aref omegas pos)))
                 for (phi-s phi-c) = (multiple-value-list (select-phi-s-phi-c gen gen-bitmap omegas phi A-pair k W))
                 unless (and (eql phi-s 'fail) (eql phi-c 'fail))
                 do (setf phi (do-replacement phi phi-s phi-c A-pair))
                    (return)
                 finally (setf potentially-extensible nil))
        finally
        (return
         (loop for p in (position-of-ones phi)
               collect (aref gen p)))))


#|
(defparameter *G1* '((add ?x ?y ?z) (even ?x) (odd ?z) (p ?z)))
(defparameter *G2* '((add ?a ?b ?c) (add ?c ?b ?a) (even ?c) (odd ?a) (p ?c)))

(fcg::cost (first
 (anti-unify-predicate-network
  (shuffle *G1*) (shuffle *G2*))))
=> best cost is 11

(time
(anti-unify-predicate-network-k-swap
 (shuffle *G1*) (shuffle *G2*)
 :k 5 :W nil))

|#

(defun identify-k-swap-alignments (pattern source &key (k 5) (W 1) allow-generalisation-over-constants)
  (declare (ignore allow-generalisation-over-constants))
  (let ((k-swap-stable-phi (k-swap-generalise pattern source :k k :W W)))
    (list
     (make-instance 'fcg::predicate-alignment-state
                    :pattern-predicates (mapcar #'first k-swap-stable-phi)
                    :source-predicates (mapcar #'second k-swap-stable-phi)
                    :pattern-delta (set-difference pattern (mapcar #'first k-swap-stable-phi) :test #'equal)
                    :source-delta (set-difference source (mapcar #'second k-swap-stable-phi) :test #'equal)))))


(defun anti-unify-predicate-network-k-swap (pattern source &key (k 5) (W 1) allow-generalisation-over-constants)
  "Anti-unifies pattern with source. Returns 5 values:
   generalisation, pattern-bindings, source-bindings, pattern-delta and source-delta."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with possible-alignments = (identify-k-swap-alignments pattern source :k k :W W
                                                               :allow-generalisation-over-constants
                                                               allow-generalisation-over-constants)
        for alignment in possible-alignments
        for pattern-in-alignment = (fcg::pattern-predicates alignment)
        for source-in-alignment = (fcg::source-predicates alignment)
        for pattern-delta = (pattern-delta alignment)
        for source-delta = (source-delta alignment)
        collect (multiple-value-bind (resulting-generalisation
                                      resulting-pattern-bindings
                                      resulting-source-bindings
                                      resulting-pattern-delta
                                      resulting-source-delta)
                    (fcg::anti-unify-predicate-sequence pattern-in-alignment source-in-alignment nil nil nil pattern-delta source-delta)
                  (make-instance 'fcg::anti-unification-result
                                 :pattern pattern
                                 :source source
                                 :generalisation resulting-generalisation
                                 :pattern-bindings (sort resulting-pattern-bindings #'string< :key (compose #'mkstr #'car))
                                 :source-bindings (sort resulting-source-bindings #'string< :key (compose #'mkstr #'car))
                                 :pattern-delta resulting-pattern-delta
                                 :source-delta resulting-source-delta
                                 :cost (fcg::anti-unification-cost pattern source
                                                                   resulting-generalisation
                                                                   resulting-pattern-delta
                                                                   resulting-source-delta)))
          into results
        ;; Sort results based on increasing cost.
        finally (return (sort results #'< :key #'cost))))






