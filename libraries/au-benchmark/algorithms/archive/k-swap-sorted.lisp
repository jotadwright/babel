(in-package :au-benchmark.lcg.kswap-sorted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;; Experimental implementation of the k-swap optimization  ;;
;; for anti-unifying over sets (Yernaux and Vanhoof 2022)  ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; represent 'gen' as a list of tuples (predicate-name pos-in-G2 pos-in-G2 bindings)
;;;; keep every set sorted the whole time (requires sorted-union and sorted-difference)
;;;; permutation check becomes equality check
;;;; WIP!!!

;;;; mapping ADT
(deftype mapping ()
  `(list))

(defun make-mapping (predicate-name n m bindings)
  (list predicate-name n m bindings))

(defun mapping-predicate (mapping)
  (declare (type mapping mapping))
  (first mapping))

(defun mapping-n (mapping)
  (declare (type mapping mapping))
  (second mapping))

(defun mapping-m (mapping)
  (declare (type mapping mapping))
  (third mapping))

(defun mapping-bindings (mapping)
  (declare (type mapping mapping))
  (fourth mapping))

(defun mapping-lessp (mapping-1 mapping-2)
  (declare (type mapping mapping-1)
           (type mapping mapping-2))
  (if (string-equal (mapping-predicate mapping-1) (mapping-predicate mapping-2))
    (if (= (mapping-n mapping-1) (mapping-n mapping-2))
      (< (mapping-m mapping-1) (mapping-m mapping-2))
      (< (mapping-n mapping-1) (mapping-n mapping-2)))
    (string-lessp (mapping-predicate mapping-1) (mapping-predicate mapping-2))))

(defun mapping-greaterp (mapping-1 mapping-2)
  (declare (type mapping mapping-1)
           (type mapping mapping-2))
  (if (string-equal (mapping-predicate mapping-1) (mapping-predicate mapping-2))
    (if (= (mapping-n mapping-1) (mapping-n mapping-2))
      (> (mapping-m mapping-1) (mapping-m mapping-2))
      (> (mapping-n mapping-1) (mapping-n mapping-2)))
    (string-greaterp (mapping-predicate mapping-1) (mapping-predicate mapping-2))))

(defun mapping-equalp (mapping-1 mapping-2)
  (declare (type mapping mapping-1)
           (type mapping mapping-2))
  (and (eql (mapping-predicate mapping-1) (mapping-predicate mapping-2))
       (= (mapping-n mapping-1) (mapping-n mapping-2))
       (= (mapping-m mapping-1) (mapping-m mapping-2))))


;;;; union and difference of sorted lists, both O(|list-1| + |list-2|) 
(defun sorted-union (list-1 list-2 &key (key #'identity) (test #'eql) predicate)
  (cond ((null list-1) list-2)
        ((null list-2) list-1)
        ((funcall test (funcall key (car list-1)) (funcall key (car list-2)))
         (cons (car list-1) (sorted-union (cdr list-1) (cdr list-2) :key key :test test :predicate predicate)))
        ((funcall predicate (funcall key (car list-1)) (funcall key (car list-2)))
         (cons (car list-1) (sorted-union (cdr list-1) list-2 :key key :test test :predicate predicate)))
        (t
         (cons (car list-2) (sorted-union list-1 (cdr list-2) :key key :test test :predicate predicate)))))

; (sorted-union '(1 2 3 4 5) '(2 4 6 8) :test #'= :predicate #'<)
; (sorted-union '(5 4 3 2 1) '(8 6 4 2) :test #'= :predicate #'>)


(defun sorted-difference (list-1 list-2 &key (key #'identity) (test #'eql) predicate)
  (cond ((null list-2) list-1)
        ((null list-1) nil)
        ((funcall test (funcall key (car list-1)) (funcall key (car list-2)))
         (sorted-difference (cdr list-1) (cdr list-2) :key key :test test :predicate predicate))
        ((funcall predicate (funcall key (car list-1)) (funcall key (car list-2)))
         (cons (car list-1) (sorted-difference (cdr list-1) list-2 :key key :test test :predicate predicate)))
        (t
         (sorted-difference list-1 (cdr list-2) :key key :test test :predicate predicate))))

; (sorted-difference '(1 2 3 4 5) '(2 4 6 8) :test #'= :predicate #'<)
; (sorted-difference '(5 4 3 2 1) '(8 6 4 2) :test #'= :predicate #'>)


;;;; k-swap abstraction
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
  (let ((matrix (loop for predicate-g1 in G1
                      for n from 1
                      append (loop for predicate-g2 in G2
                                   for m from 1
                                   for bindings = (matching-predicates-p predicate-g1 predicate-g2)
                                   unless (eql bindings 'fail)
                                   collect (make-mapping (first predicate-g1) n m bindings)))))
    (sort matrix #'mapping-lessp)))


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


(defun omega (pair set-of-pairs)
  "Quality estimator based on the number of conflicting pairs,
   as described in Yernaux and Vanhoof (2019)"
  (let ((conflicting-pairs
         (loop for other-pair in set-of-pairs
               unless (compatible-pairs-p pair set-of-pairs)
               count other-pair)))
    (/ 1 (+ conflicting-pairs 1))))


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
  "In phi, replace phi-s with the union of phi-c and A-pair."
  (adjoin A-pair (union (set-difference phi phi-s :test #'equal) phi-c :test #'equal) :test #'equal))


(defun enforce (set-of-pairs pair-to-enforce)
  "Adds pair-to-enforce to set-of-pairs, removing any pairs that would become incompatible."
  (loop for pair in set-of-pairs
        if (compatible-pairs-p pair pair-to-enforce)
        collect pair into comptatible-pairs
        finally (return (cons pair-to-enforce comptatible-pairs))))


(defun comp (set-of-pairs set-of-pairs-to-check)
  "Returns part of set-of-pairs-to-check that is readily compatible with set-of-pairs."
  (loop for pair in set-of-pairs-to-check
        if (compatible-with-set-of-pairs set-of-pairs pair)
        collect pair))


(defun select-phi-s-phi-c (gen phi A-pair omega-values k W V)
  "Adds A-pair to phi, if necessary (i.e. if there are incompatibilities) swapping up to k other pairs."
  ;; phi-s: pairs from phi to remove (k = upperbound on length of phi-s)
  ;; phi-c: pairs to replace phi-s in phi: always includes A-pair and has the same length as phi-s
  (let* ((GS nil) ;; stack
         (BS (make-instance 'queue)) ;; queue
         (A-pair-enforced (enforce phi A-pair))
         (phi-c nil)
         ;; initialize phi-s with the pairs of phi that are incompatible when enforcing A-pair in phi
         (phi-s (set-difference phi A-pair-enforced :test #'equal))
         ;; S are the candidate pairs used for swapping
         ;; i.e. all possible pairs minus the result of enforcing A-pair in phi
         (S (set-difference gen A-pair-enforced :test #'equal)))

    (loop while (and (< (length phi-c) (length phi-s))  ;; replace phi-s with something of the same size
                     (if k (<= (length phi-s) k) t))    ;; and keep if manageable
          do (loop with GS-states = nil
                   for comp-set = (take-bins (comp (do-replacement phi phi-s phi-c A-pair) S) omega-values W)
                   while (and (< (length phi-c) (length phi-s))  ;; when you have not reached the size of phi-s
                              (or comp-set                       ;; either there are new compatible pairs to enlarge phi-c
                                  GS))                           ;; or you take an alternative from the stack
                   ;; comp(pi, S) is the subset of S that can be added to pi
                   ;; such that the result is a valid generalisation (no conflicts)
                   ;; So here we take the subset of S that is compatible with
                   ;; the mapping obtained by replacing phi-s with phi-c + A-pair
                   do (loop for p in comp-set
                            ;; create entries on the stack for each compatible pair
                            for enlarged-phi-c = (adjoin p phi-c :test #'equal)
                            for new-S = (remove p S :test #'equal)
                            for dup = (find enlarged-phi-c GS-states
                                            :test #'(lambda (x y)
                                                      (permutation-of? x y :test #'equal)))
                            unless dup do (push (cons enlarged-phi-c new-S) GS))
                      ;; take the first entry from the stack and loop!
                      (let* ((stack-pair (pop GS)))
                        (push (car stack-pair) GS-states)
                        (setf phi-c (car stack-pair))
                        (setf S (cdr stack-pair))))
             ;; when not possible to find phi-c of size phi-s
             ;; we remove one more element from phi-s
             ;; and re-initialise phi-c and S
             ;; This is tried as long as phi-s <= k
             (when (< (length phi-c) (length phi-s))
               (loop for p in (take-bins (set-difference phi phi-s :test #'equal) omega-values V :best nil)
                     for enlarged-phi-s = (adjoin p phi-s :test #'equal)
                     for dup = (find enlarged-phi-s
                                     (elements BS)
                                     :test #'(lambda (x y)
                                               (permutation-of? x y :test #'equal)))
                     unless dup do (enqueue-at-end BS (list enlarged-phi-s)))
               (if (elements BS)
                 (progn
                   (setf phi-s (remove-front BS))
                   (setf phi-c nil)
                   (setf S (set-difference gen A-pair-enforced :test #'equal)))
                 (return-from select-phi-s-phi-c (values 'fail 'fail))))) ;; unable to find pi-c 

    (if (= (length phi-c) (length phi-s))
      (values phi-s phi-c)
      (values 'fail 'fail))))


(defun k-swap-generalise (G1 G2 &key (k 5) (W 1) (V 1) (omega-scope :global))
  "Compute a k-swap stable generalisation of G1 and G2.
   A generalisation is k-swap stable when there is no obvious way
   (i.e. by replacing k or less elements) in which a larger
   generalization could be obtained."
  (loop with phi = nil  ;; the generalisation thus far
        with potentially-extensible = t
        with gen = (gen G1 G2)
        with omegas-global = (mapcar #'(lambda (m) (cons m (omega m gen))) gen)
        until (not potentially-extensible)
        for omegas-local = (mapcar #'(lambda (m) (cons m (omega m phi))) gen)
        for omega-values = (case omega-scope (:global omegas-global) (:local omegas-local))
        ;; in every loop, we try to add A-pair to phi
        do (loop for A-pair in (sorted-difference gen phi :test #'mapping-equalp :predicate #'mapping-lessp)
                 for (phi-s phi-c) = (multiple-value-list (select-phi-s-phi-c gen phi A-pair omega-values k W V))
                 unless (and (eql phi-s 'fail) (eql phi-c 'fail))
                 do (setf phi (do-replacement phi phi-s phi-c A-pair))
                    (return)
                 finally (setf potentially-extensible nil))
        finally (return (values phi (length gen)))))


;;;; high-level API
(defun identify-k-swap-alignments (pattern source &key (k 5) (W 1) (V 1) (omega-scope :local) allow-generalisation-over-constants)
  (declare (ignore allow-generalisation-over-constants))
  (multiple-value-bind (k-swap-stable-phi size-of-gen)
      (k-swap-generalise pattern source :k k :W W :V V :omega-scope omega-scope)
    (let ((pattern-predicates (mapcar #'first k-swap-stable-phi))
          (source-predicates (mapcar #'second k-swap-stable-phi)))
      (values
       (list
        (make-instance 'predicate-alignment-state
                       :pattern-predicates pattern-predicates
                       :source-predicates source-predicates
                       :pattern-delta (set-difference pattern pattern-predicates :test #'equal)
                       :source-delta (set-difference source source-predicates :test #'equal)))
       size-of-gen))))


(defun anti-unify-predicate-networks (pattern source &key (k 5) (W 1) (V 1) (omega-scope :local) allow-generalisation-over-constants)
  "Anti-unify pattern and source. Returns all solutions of the lowest cost,
   their cost value, and the number of alignments that were found."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with (possible-alignments size-of-gen)
          = (multiple-value-list
             (identify-k-swap-alignments pattern source :k k :W W :V V :omega-scope omega-scope
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
                                                              :lcg)))
          into results
        ;; Sort results based on increasing cost.
        finally (return (values (sort results #'< :key #'cost)
                                (cost (first results))
                                size-of-gen))))

