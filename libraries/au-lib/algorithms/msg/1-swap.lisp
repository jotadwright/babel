(in-package :au-lib)

;; 1-swap stability w.r.t. a quality
;; function omega which counts
;; variable couplings


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
               (- (delta-extern phi-minus-B-pair (list B-pair)))
               (- (delta-extern (list B-pair) phi-minus-B-pair))
               (get-delta-i delta-is A-pair)
               (delta-extern phi-minus-B-pair (list A-pair))
               (delta-extern (list A-pair) phi-minus-B-pair))
            (+ phi-delta-i
               (get-delta-i delta-is A-pair)
               (delta-extern phi (list A-pair))
               (delta-extern (list A-pair) phi))))
    new-delta-i))


;;;; select swappable pairs
(defun improves-quality-p (B-pair C-pair phi-minus-B-pair phi-delta-i delta-is)
  "Return t if swapping B-pair with C-pair
   will yield a generalisation of better quality"
  (> (+ phi-delta-i
        (- (get-delta-i delta-is B-pair))
        (- (delta-extern phi-minus-B-pair (list B-pair)))
        (- (delta-extern (list B-pair) phi-minus-B-pair))
        (get-delta-i delta-is C-pair)
        (delta-extern phi-minus-B-pair (list C-pair))
        (delta-extern (list C-pair) phi-minus-B-pair))
     phi-delta-i))


(defun select-first-B-pair-C-pair (phi phi-delta-i gen gains delta-is)
  "Select a B-pair from phi and a C-pair from comp
   such that swapping B-pair with C-pair yields a generalisation
   of higher quality."
  (loop for B-pair in (sort (copy-list phi) #'< :key #'(lambda (pair) (get-gain gains pair)))
        for comp-set = (comp (remove B-pair phi :test #'equal) gen)
        for phi-minus-B-pair = (remove B-pair phi :test #'equal)
        do (loop for C-pair in (sort comp-set #'>
                                     :key #'(lambda (pair)
                                              (- (get-gain gains pair)
                                                 (delta-extern (list pair) (list B-pair))
                                                 (delta-extern (list B-pair) (list pair)))))
                 when (improves-quality-p B-pair C-pair phi-minus-B-pair phi-delta-i delta-is)
                 do (return-from select-first-B-pair-C-pair (values B-pair C-pair)))
        finally (return (values 'fail 'fail))))


(defun select-best-B-pair-C-pair (phi phi-delta-i gen gains delta-is)
  (let ((best-delta-i phi-delta-i)
        best-B-pair
        best-C-pair)
    (loop for B-pair in (sort (copy-list phi) #'< :key #'(lambda (pair) (get-gain gains pair)))
          for comp-set = (comp (remove B-pair phi :test #'equal) gen)
          for phi-minus-B-pair = (remove B-pair phi :test #'equal)
          do (loop for C-pair in (sort comp-set #'>
                                       :key #'(lambda (pair)
                                                (- (get-gain gains pair)
                                                   (delta-extern (list pair) (list B-pair))
                                                   (delta-extern (list B-pair) (list pair)))))
                   for new-delta-i = (+ phi-delta-i
                                        (- (get-delta-i delta-is B-pair))
                                        (- (delta-extern phi-minus-B-pair (list B-pair)))
                                        (- (delta-extern (list B-pair) phi-minus-B-pair))
                                        (get-delta-i delta-is C-pair)
                                        (delta-extern phi-minus-B-pair (list C-pair))
                                        (delta-extern (list C-pair) phi-minus-B-pair))
                   when (and (> new-delta-i phi-delta-i) (> new-delta-i best-delta-i))
                   do (setf best-delta-i new-delta-i
                            best-B-pair B-pair
                            best-C-pair C-pair))
          finally
            (if (and best-B-pair best-C-pair)
              (return (values best-B-pair best-C-pair))
              (return (values 'fail 'fail))))))


;;;; 1-swap generalise
(defun 1-swap-generalise (G1 G2 &key allow-generalisation-over-constants)
  (loop with phi = nil  ;; start with an empty generalisation
        with phi-delta-i = 0  ;; quality of current generalisation
        with gen = (gen G1 G2
                        :allow-generalisation-over-constants
                        allow-generalisation-over-constants)
        ;; initialize omega values for all pairs in gen
        ;; initial omega value is Delta_I(pair)
        with gains = (init-gains gen)
        ;; pre-compute Delta_I values for all pairs in gen
        with delta-is = (pre-compute-delta-i gen)
        ;; select pairs from gen that are compatible with current phi
        for comp = (comp phi gen)
        ;; select the best A-pair from comp according to quality
        for (A-pair A-pair-gain) = (multiple-value-list (the-biggest #'(lambda (pair) (get-gain gains pair)) comp))
        ;; repeat until there is no more A-pair to add
        ;; i.e. we have reached the generalisation of maximum length
        while A-pair
        ;; Add A-pair to phi and update values accordingly
        do (setf phi-delta-i (update-phi-delta-i phi phi-delta-i delta-is A-pair)
                 phi (adjoin A-pair phi :test #'equal)
                 gains (recompute-gains gains phi gen delta-is))
        ;; after adding A-pair, perform 1-swap(s) that improve the quality
        do (assert (= phi-delta-i (delta-intern phi)))
           (loop for (B-pair C-pair) = (multiple-value-list (select-first-B-pair-C-pair phi phi-delta-i gen gains delta-is))
                 unless (and (eql B-pair 'fail) (eql C-pair 'fail))
                 do (setf phi-delta-i (update-phi-delta-i phi phi-delta-i delta-is C-pair B-pair)
                          phi (do-replacement phi (list B-pair) (list C-pair))
                          gains (recompute-gains gains phi gen delta-is))
                    (assert (= phi-delta-i (delta-intern phi)))
                    (notify test-added-pair-in-1-swap A-pair B-pair)
                 until (and (eql B-pair 'fail) (eql C-pair 'fail)))
        ;; finally return the generalisation phi
        ;; it's Delta_I value (number of shared variables/bindings)
        finally
          (return (values phi phi-delta-i))))


(defun identify-k-swap-alignments (pattern source &key allow-generalisation-over-constants)
  (declare (ignore k W))
  (multiple-value-bind (k-swap-stable-phi phi-delta-i)
      (1-swap-generalise pattern source :allow-generalisation-over-constants allow-generalisation-over-constants)
    (values
     (list
      (make-instance 'predicate-alignment-state
                     :pattern-predicates (mapcar #'first k-swap-stable-phi)
                     :source-predicates (mapcar #'second k-swap-stable-phi)
                     :pattern-delta (set-difference pattern (mapcar #'first k-swap-stable-phi) :test #'equal)
                     :source-delta (set-difference source (mapcar #'second k-swap-stable-phi) :test #'equal)))
     phi-delta-i)))


(defmethod anti-unify-predicate-networks (pattern source (mode (eql :1-swap))
                                                  &key (cost-mode :msg)
                                                  allow-generalisation-over-constants)
  "Anti-unify pattern and source using 1-swap. Returns the anti-unification result,
   its cost, and the size of 'gen'"
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with (possible-alignments phi-delta-i)
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
                                                              cost-mode)))
          into results
        ;; Sort results based on increasing cost.
        finally (return (values (sort results #'< :key #'cost)
                                (cost (first results))
                                phi-delta-i
                                1))))



