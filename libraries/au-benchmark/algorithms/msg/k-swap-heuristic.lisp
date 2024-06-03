(in-package :au-benchmark.msg.kswap-heuristic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;; Experimental implementation of the k-swap optimization  ;;
;; for anti-unifying over sets (Yernaux and Vanhoof 2022)  ;;
;; but with variable decoupling!                           ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gen (G1 G2 &key allow-generalisation-over-constants)
  "Returns all pairs of predicates in G1 and G2 that unify.
   Format: '((predicate-g1 predicate-g2 bindings) ...)"
  (loop for predicate-g1 in G1
        append (loop for predicate-g2 in G2
                     when (matching-predicates predicate-g1 predicate-g2
                                               :allow-generalisation-over-constants
                                               allow-generalisation-over-constants)
                     collect (list predicate-g1 predicate-g2
                                   (pairlis (rest predicate-g1) (rest predicate-g2))))))


(defun normalised-conflicting-bindings (binding other-bindings)
  (let ((counts 0)
        (conflicts 0))
    (loop for (x . y) in other-bindings
          if (and (eql x (car binding))
                  (eql y (cdr binding)))
          do (incf counts)
          else if (eql x (car binding))
          do (incf counts)
             (incf conflicts))
    (list conflicts counts)))
          
(defun omega (pair set-of-pairs)
  (let* ((cc-1 (apply #'mapcar #'+
                       (loop for binding in (third pair)
                             collect (normalised-conflicting-bindings
                                      binding (mappend #'third set-of-pairs)))))
         (cc-2 (apply #'mapcar #'+
                       (loop for binding in (third pair)
                             collect (normalised-conflicting-bindings
                                      (cons (cdr binding) (car binding))
                                      (reverse-bindings
                                       (mappend #'third set-of-pairs))))))
         (cc (mapcar #'+ cc-1 cc-2)))
    (- 1 (/ (first cc) (+ (second cc) 1)))))


#|
(defun omega (pair set-of-pairs)
  "Quality estimator of a pair.
   The quality of a pair is the multiplicative inverse of
   the number of bindings in set-of-pairs that are in conflict 
   with the bindings in pair, divided by the occurrence count of
   the bindngs in pair. When more bindings are in conflict 
   proportionally, the quality of the pair decreases."
  (let ((occurrence-count 1)
        (conflict-count 0))
    (loop with pair-bindings = (third pair)
          for (nil nil bindings) in set-of-pairs
          do (loop for (x . y) in pair-bindings
                   for pair = (find x bindings :key #'car)
                   if (and pair (not (eql (cdr pair) y)))
                   do (incf occurrence-count)
                      (incf conflict-count)
                   else if pair
                   do (incf occurrence-count)))
    (- 1 (/ conflict-count occurrence-count))))
|#


(defun take-bins (set-of-pairs omega-values number-of-bins &key (best t))
  "Group the pairs in set-of-pairs based on their omega values
   and take a number of groups with the best/worst omega values. 
   When number-of-bins is nil, take all pairs."
  (cond ((null set-of-pairs) nil)
        ((= (length set-of-pairs) 1) set-of-pairs)
        (t
         (let ((pairs-grouped-by-omega-value-and-sorted
                (sort
                 (copy-list
                  (group-by set-of-pairs
                            #'(lambda (pair)
                                (rest (assoc pair omega-values :test #'equal)))
                            :test #'=))
                 (if best #'> #'<)
                 :key #'car)))
           (mappend #'cdr
                    (cond ((null number-of-bins) pairs-grouped-by-omega-value-and-sorted)
                          ((> number-of-bins (length pairs-grouped-by-omega-value-and-sorted))
                           pairs-grouped-by-omega-value-and-sorted)
                          (t (subseq pairs-grouped-by-omega-value-and-sorted 0 number-of-bins))))))))


(defun do-replacement (phi phi-s phi-c A-pair)
  "In phi, replace phi-s with phi-c and add A-pair."
  (adjoin A-pair (union (set-difference phi phi-s :test #'equal) phi-c :test #'equal) :test #'equal))


(defun compatible-pairs-p (pair-1 pair-2)
  "Returns t if pair-1 and pair-2 are compatible.
   'gen' generates multiple mappings between predicates from pattern and source.
   However, each predicate from the input can be used only once in the generalisation.
   So this is checked here. We can ignore the bindings because we don't want the
   injectivity of the original k-swap implementation."
  (and (not (equal (first pair-1) (first pair-2)))
       (not (equal (second pair-1) (second pair-2)))))


(defun enforce (set-of-pairs pair-to-enforce)
  "Adds pair-to-enforce to set-of-pairs, removing any pairs that would become incompatible."
  (loop for pair in set-of-pairs
        if (compatible-pairs-p pair pair-to-enforce)
        collect pair into comptatible-pairs
        finally (return (cons pair-to-enforce comptatible-pairs))))

;; (enforce '(((p ?x ?y) (p ?a ?b) ((?x . ?a) (?y . ?b))) ((q ?x) (q ?a) ((?x . ?a)))) '((r ?y) (r ?c) ((?y . ?c))))


(defun compatible-with-set-of-pairs (set-of-pairs pair-to-check)
  "Return t if pair-to-check is compatible with all pairs in set-of-pairs."
  (loop for pair in set-of-pairs
        always (compatible-pairs-p pair pair-to-check)))


(defun comp (set-of-pairs set-of-pairs-to-check)
  "Returns part of set-of-pairs-to-check that is readily compatible with set-of-pairs."
  (loop for pair in set-of-pairs-to-check
        if (compatible-with-set-of-pairs set-of-pairs pair)
        collect pair))


(defun select-phi-s-phi-c (gen omegas phi A-pair k W V)
  "Adds A-pair to phi, if necessary (i.e. if there are incompatibilities) swapping up to k other pairs."
  ;; phi-s: pairs from phi to remove (k = upperbound on length of phi-s)
  ;; phi-c: pairs to replace phi-s in phi: always includes A-pair and has the same length as phi-s
  (let* ((GS nil) ;; stack
         (BS (make-instance 'queue)) ;; queue
         (phi-enforced-A (enforce phi A-pair))
         (phi-c nil)
         (phi-s (set-difference phi phi-enforced-A :test #'equal))
         (S (set-difference gen phi-enforced-A :test #'equal)))

    (loop while (and S
                     (< (length phi-c) (length phi-s))
                     (if k (<= (length phi-s) k) t))
          do (loop with GS-states = nil
                   for comp-set = (take-bins (comp (do-replacement phi phi-s phi-c A-pair) S) omegas W)
                   while (and (< (length phi-c) (length phi-s))
                              (or comp-set GS))
                   ;; select p to extend phi-c (the subset to replace phi-s)
                   ;; take the W best bins according to omega
                   do (loop for p in comp-set
                            for enlarged-phi-c = (adjoin p phi-c :test #'equal)
                            for new-S = (remove p S :test #'equal)
                            for dup = (find enlarged-phi-c GS-states
                                            :test #'(lambda (x y)
                                                      (permutation-of? x y :test #'equal)))
                            unless dup do (push (cons enlarged-phi-c new-S) GS))
                      (let* ((stack-pair (pop GS)))
                        (push (car stack-pair) GS-states)
                        (setf phi-c (car stack-pair))
                        (setf S (cdr stack-pair))))
             (when (< (length phi-c) (length phi-s))
               ;; select p to extend phi-s (the subset to remove from phi)
               ;; take the V worst bins according to omega
               (loop with options = (take-bins (set-difference phi phi-s :test #'equal) omegas V :best nil)
                     for p in options
                     for enlarged-phi-s = (adjoin p phi-s :test #'equal)
                     for dup = (find enlarged-phi-s (elements BS)
                                     :test #'(lambda (x y)
                                               (permutation-of? x y :test #'equal)))
                     unless dup do (enqueue-at-end BS (list enlarged-phi-s)))
               (if (elements BS)
                 (progn
                   (setf phi-s (remove-front BS))
                   (setf phi-c nil)
                   (setf S (set-difference gen phi-enforced-A :test #'equal)))
                 (progn
                   (return-from select-phi-s-phi-c (values 'fail 'fail)))))) ;; unable to find phi-c 

    (if (= (length phi-c) (length phi-s))
      (values phi-s phi-c)
      (values 'fail 'fail))))


(defun k-swap-generalise (G1 G2 &key (k 5) (W 1) (V 1) (omega-scope :global) allow-generalisation-over-constants)
  (loop with phi = nil
        with potentially-extensible = t
        with gen = (gen G1 G2
                        :allow-generalisation-over-constants
                        allow-generalisation-over-constants)
        ;; global omegas: omega value of pair w.r.t. gen
        ;; compute once at the start
        with omegas-global = (mapcar #'(lambda (pair) (cons pair (omega pair gen))) gen)
        until (not potentially-extensible)
        ;; local omegas: omega value of pair w.r.t. phi
        ;; recompute every loop
        for omegas-local = (mapcar #'(lambda (pair) (cons pair (omega pair phi))) gen)
        ;; in each loop, try to extend phi with A-pair
        ;; consider A-pairs in the order that has the
        ;; least conflicting bindings with all other pairs
        do (loop for A-pair in (sort (copy-list (set-difference gen phi :test #'equal))
                                     #'> :key #'(lambda (pair)
                                                  (case omega-scope
                                                    (:global (rest (assoc pair omegas-global :test #'equal)))
                                                    (:local (rest (assoc pair omegas-local :test #'equal))))))
                 for (phi-s phi-c)
                   = (multiple-value-list
                      (select-phi-s-phi-c gen
                                          (case omega-scope
                                            (:global omegas-global)
                                            (:local omegas-local))
                                          phi A-pair k W V))
                 unless (and (eql phi-s 'fail) (eql phi-c 'fail))
                 do (setf phi (do-replacement phi phi-s phi-c A-pair))
                    (return)
                 finally (setf potentially-extensible nil))
        finally (return (values phi (length gen)))))


#|
(defparameter *G1* '((add ?x ?y ?z) (even ?x) (odd ?z) (p ?z)))
(defparameter *G2* '((add ?a ?b ?c) (add ?c ?b ?a) (even ?c) (odd ?a) (p ?c)))

(k-swap-generalise *G1* *G2*) 

(fcg::cost (first
 (anti-unify-predicate-networks
  (shuffle *G1*) (shuffle *G2*))))
=> best cost is 11

(fcg::cost (first
 (anti-unify-predicate-networks-k-swap
  (shuffle *G1*) (shuffle *G2*)
  :k 5 :W nil :V nil)))

|#

(defun identify-k-swap-alignments (pattern source &key (k 5) (W 1) (V 1) (omega-scope :global) allow-generalisation-over-constants)
  (multiple-value-bind (k-swap-stable-phi size-of-gen) (k-swap-generalise pattern source :k k :W W :V V
                                                                          :omega-scope omega-scope
                                                                          :allow-generalisation-over-constants
                                                                          allow-generalisation-over-constants)
    (values
     (list
      (make-instance 'predicate-alignment-state
                     :pattern-predicates (mapcar #'first k-swap-stable-phi)
                     :source-predicates (mapcar #'second k-swap-stable-phi)
                     :pattern-delta (set-difference pattern (mapcar #'first k-swap-stable-phi) :test #'equal)
                     :source-delta (set-difference source (mapcar #'second k-swap-stable-phi) :test #'equal)))
     size-of-gen)))


(defun anti-unify-predicate-networks (pattern source &key (k 5) (W 1) (V 1) (omega-scope :local) allow-generalisation-over-constants)
  "Anti-unify pattern and source. Returns all solutions of the lowest cost,
   their cost value, and the number of alignments that were found."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with (possible-alignments size-of-gen)
        = (multiple-value-list
           (identify-k-swap-alignments pattern source :k k :W W :V V
                                       :omega-scope omega-scope
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
                                 :pattern-bindings (sort resulting-pattern-bindings #'string<
                                                         :key #'(lambda (b) (mkstr (car b))))
                                 :source-bindings (sort resulting-source-bindings #'string<
                                                        :key #'(lambda (b) (mkstr (car b))))
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
                                size-of-gen))))
