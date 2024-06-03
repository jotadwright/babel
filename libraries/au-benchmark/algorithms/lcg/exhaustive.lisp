(in-package :au-benchmark.lcg.exhaustive)

(defun make-predicate-alignment-processor (pattern source)
  (let ((initial-state
         (make-instance 'lcg-alignment-state
                        :candidates (gen pattern source))))
    (make-instance 'predicate-alignment-processor
                   :queue (list initial-state))))


(defun next-possible-alignment (predicate-alignment-processor)
  (loop while (queue predicate-alignment-processor)
        for state = (pop (queue predicate-alignment-processor))
        do (with-slots (candidates phi) state
             (cond
              ;; no candidates remaining: we have a solution!
              ((null candidates)
               (return-from next-possible-alignment state))

              ;; still candidates remaining
              ;; if compatible, create one subset with A-pair included
              ;; and one subset with A-pair excluded. otherwise, exclude A-pair
              (t
               (let ((A-pair (first candidates)))
                 (when (compatible-with-set-of-pairs phi A-pair)
                   (push (make-instance 'lcg-alignment-state
                                        :candidates (rest candidates)
                                        :phi (append phi (list A-pair)))
                         (queue predicate-alignment-processor)))
                 (push (make-instance 'lcg-alignment-state
                                      :candidates (rest candidates)
                                      :phi phi)
                       (queue predicate-alignment-processor))))))
          finally (return nil)))


(defun anti-unify-predicate-networks (pattern source)
  "Anti-unify pattern and source. Returns all solutions of the lowest cost,
   their cost value, and the number of alignments that were found."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with predicate-alignment-processor = (make-predicate-alignment-processor pattern source)
        with best-solutions = nil
        with best-cost = nil
        with number-of-alignments-found = 0
        while (queue predicate-alignment-processor)
        for alignment = (next-possible-alignment predicate-alignment-processor)
        for pattern-predicates = (mapcar #'first (phi alignment))
        for source-predicates = (mapcar #'second (phi alignment))
        for pattern-delta = (set-difference pattern pattern-predicates :test #'equal)
        for source-delta = (set-difference source source-predicates :test #'equal)
        for anti-unification-result
          = (when alignment
              (multiple-value-bind (resulting-generalisation
                                    resulting-pattern-bindings
                                    resulting-source-bindings
                                    resulting-pattern-delta
                                    resulting-source-delta)
                (anti-unify-predicate-sequence pattern-predicates
                                               source-predicates
                                               nil nil nil
                                               pattern-delta
                                               source-delta)
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
                                                          resulting-source-delta
                                                          :lcg))))
        when anti-unification-result
        do (incf number-of-alignments-found)
           (cond ((null best-solutions)
                  (push anti-unification-result best-solutions)
                  (setf best-cost (cost anti-unification-result)))
                 ((= (cost anti-unification-result) best-cost)
                  (push anti-unification-result best-solutions))
                 ((< (cost anti-unification-result) best-cost)
                  (setf (car best-solutions) anti-unification-result)
                  (setf (cdr best-solutions) nil)
                  (setf best-cost (cost anti-unification-result))))
        finally (return (values best-solutions
                                best-cost
                                number-of-alignments-found))))




