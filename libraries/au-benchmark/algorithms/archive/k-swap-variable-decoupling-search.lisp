(in-package :cl-user)

(defpackage :k-swap-variable-decoupling-search
  (:use :common-lisp :cl-user :utils :fcg)
  (:export :k-swap-generalise
           :anti-unify-predicate-network-k-swap))

(in-package :k-swap-variable-decoupling-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;; Experimental implementation of the k-swap optimization  ;;
;; for anti-unifying over sets (Yernaux and Vanhoof 2022)  ;;
;; but with variable decoupling and search!                ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen (G1 G2)
  "Returns all pairs of predicates in G1 and G2 that unify.
   Format: '((predicate-g1 predicate-g2 bindings) ...)"
  (loop for predicate-g1 in G1
        append (loop for predicate-g2 in G2
                     for bindings = (unify-simple predicate-g1 predicate-g2)
                     when bindings collect (list predicate-g1 predicate-g2 bindings))))


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
          for (nil nil bindings) in set-of-pairs
          do (loop for (x . y) in pair-bindings
                   for pair = (find x bindings :key #'car)
                   if (and pair (not (eql (cdr pair) y)))
                   do (incf occurrence-count)
                      (incf conflict-count)
                   else if pair
                   do (incf occurrence-count)))
    (- 1 (/ conflict-count occurrence-count))))
                   
;; (gen '((p a) (p b) (f x)) '((p ?z) (f x)))
;; (length (gen *G1* *G2*))


(defun take-bins (set-of-pairs omega-values number-of-bins &key (best t))
  "Group the pairs in set-of-pairs based on their omega values
   and take pairs with the W best/worst omega values. When W is
   nil, take all pairs."
  (cond ((null set-of-pairs) nil)
        ((length= set-of-pairs 1) set-of-pairs)
        (t
         (let ((pairs-grouped-by-omega-value-and-sorted
                (sort
                 (group-by set-of-pairs
                           #'(lambda (pair)
                               (rest (assoc pair omega-values :test #'equal)))
                           :test #'=)
                 (if best #'> #'<)
                 :key #'car)))
           (mappend #'cdr
                    (cond ((null number-of-bins) pairs-grouped-by-omega-value-and-sorted)
                          ((> number-of-bins (length pairs-grouped-by-omega-value-and-sorted))
                           pairs-grouped-by-omega-value-and-sorted)
                          (t (subseq pairs-grouped-by-omega-value-and-sorted 0 number-of-bins))))))))


(defun do-replacement (phi phi-s phi-c A-pair)
  "In phi, replace phi-s with the union of phi-c and A-pair."
  (union (union (set-difference phi phi-s :test #'equal) phi-c :test #'equal) (list A-pair) :test #'equal))


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
                     (<= (length phi-s) k))
          do (loop with GS-states = nil
                   for comp-set = (take-bins (comp (do-replacement phi phi-s phi-c A-pair) S) omegas W)
                   while (and (< (length phi-c) (length phi-s))
                              (or comp-set GS))
                   ;; select p to extend phi-c (the subset to replace phi-s)
                   ;; sorted by omega
                   do (loop for p in comp-set
                            for enlarged-phi-c = (union phi-c (list p) :test #'equal)
                            for new-S = (set-difference S (list p) :test #'equal)
                            for dup = (find enlarged-phi-c GS-states
                                            :key #'car
                                            :test #'(lambda (x y)
                                                      (permutation-of? x y :test #'equal)))
                            unless dup do (push (cons enlarged-phi-c new-S) GS))
                      (let* ((stack-pair (pop GS)))
                        (when stack-pair (push stack-pair GS-states))
                        (setf phi-c (car stack-pair))
                        (setf S (cdr stack-pair))))
             (when (< (length phi-c) (length phi-s))
               ;; select p to extend phi-s (the subset to remove from phi)
               ;; sorted by omega (inverse)
               (loop with options = (take-bins (set-difference phi phi-s :test #'equal) omegas V :best nil)
                     for p in options
                     for enlarged-phi-s = (union phi-s (list p) :test #'equal)
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

(defclass k-swap-state ()
  ((phi
    :documentation "The current mapping phi"
    :accessor phi :initarg :phi :initform nil :type list)
   (A-pairs
    :documentation "Potential pairs that can be added to phi"
    :accessor A-pairs :initarg :A-pairs
    :initform nil :type list)))

(defun make-initial-k-swap-state (gen)
  (make-instance 'k-swap-state :phi nil :A-pairs gen))

(defun k-swap-generalise (G1 G2 &key (k 5) (W 1) (V 1) (n 3))
  (loop with solutions = nil
        with gen = (gen G1 G2)
        with omegas = (mapcar #'(lambda (pair) (cons pair (omega pair gen))) gen)
        with queue = (list (make-initial-k-swap-state gen))
        while queue
        for state = (pop queue)
        do (cond ((and n (= (length solutions) n))
                  (return solutions))
                 ((null (A-pairs state))
                  (when (loop for solution in solutions
                              never (permutation-of? (phi solution) (phi state) :test #'equal))
                    (push state solutions)))
                 (t
                  (loop for A-pair in (sort (A-pairs state) #'> :key #'(lambda (pair) (rest (assoc pair omegas :test #'equal))))
                        for (phi-s phi-c) = (multiple-value-list (select-phi-s-phi-c gen omegas (phi state) A-pair k W V))
                        if (and (eql phi-s 'fail) (eql phi-c 'fail))
                        do (push (make-instance 'k-swap-state :phi (phi state) :A-pairs nil) queue)
                        else
                        do (let* ((new-phi (do-replacement (phi state) phi-s phi-c A-pair))
                                  (new-A-pairs (set-difference (A-pairs state) new-phi :test #'equal)))
                             (push (make-instance 'k-swap-state :phi new-phi :A-pairs new-A-pairs) queue)))))
        finally (return solutions)))
                                    


(defun identify-k-swap-alignments (pattern source &key (k 5) (W 1) (V 1) (n 3) allow-generalisation-over-constants)
  (declare (ignore allow-generalisation-over-constants))
  (let ((k-swap-stable-phis (k-swap-generalise pattern source :k k :W W :V V :n n)))
    (loop for state in k-swap-stable-phis
          collect 
            (make-instance 'fcg::predicate-alignment-state
                           :pattern-predicates (mapcar #'first (phi state))
                           :source-predicates (mapcar #'second (phi state))
                           :pattern-delta (set-difference pattern (mapcar #'first (phi state)) :test #'equal)
                           :source-delta (set-difference source (mapcar #'second (phi state)) :test #'equal)))))


(defun anti-unify-predicate-network-k-swap (pattern source &key (k 5) (W 1) (V 1) (n 3) allow-generalisation-over-constants)
  "Anti-unifies pattern with source. Returns 5 values:
   generalisation, pattern-bindings, source-bindings, pattern-delta and source-delta."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with possible-alignments = (identify-k-swap-alignments pattern source :k k :W W :V V :n n
                                                               :allow-generalisation-over-constants
                                                               allow-generalisation-over-constants)
        for alignment in possible-alignments
        for pattern-in-alignment = (fcg::pattern-predicates alignment)
        for source-in-alignment = (fcg::source-predicates alignment)
        for pattern-delta = (fcg::pattern-delta alignment)
        for source-delta = (fcg::source-delta alignment)
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
                                 :cost (fcg::anti-unification-cost pattern
                                                                   source
                                                                   resulting-generalisation
                                                                   resulting-pattern-delta
                                                                   resulting-source-delta)))
          into results
        ;; Sort results based on increasing cost.
        finally (return (sort results #'< :key #'cost))))

