(in-package :au-lib)

;; Baseline MSG algorithm
;; Equivalent to the exhaustive algorithm,
;; but only generates the first solution
;; on shuffled inputs

(defun make-predicate-alignment-processor (pattern source)
  (let ((initial-state
         (make-instance 'predicate-alignment-state
                        :pattern-remaining pattern
                        :source-remaining source)))
    (make-instance 'predicate-alignment-processor
                   :queue (list initial-state))))


(defun next-possible-alignment (predicate-alignment-processor &key allow-generalisation-over-constants)
  (loop while (queue predicate-alignment-processor)
        for state = (pop (queue predicate-alignment-processor))
        do (with-slots (pattern-predicates
                        source-predicates
                        pattern-delta
                        source-delta
                        pattern-remaining
                        source-remaining) state
             (cond ; no predicates remaining in pattern and source: we have a solution!
              ((and (null pattern-remaining) (null source-remaining))
               (return-from next-possible-alignment state))
              
              ; no pattern-predicates remaining: rest of source goes to source-delta
              ((null pattern-remaining)
               (push (make-instance 'predicate-alignment-state
                                    :pattern-predicates pattern-predicates
                                    :source-predicates source-predicates
                                    :pattern-delta pattern-delta
                                    :source-delta (append source-delta source-remaining))
                     (queue predicate-alignment-processor)))
            
              ; no source-predicates remaining: rest of pattern goes to pattern-delta
              ((null source-remaining)
               (push (make-instance 'predicate-alignment-state
                                    :pattern-predicates pattern-predicates
                                    :source-predicates source-predicates
                                    :pattern-delta (append pattern-delta pattern-remaining)
                                    :source-delta source-delta)
                     (queue predicate-alignment-processor)))
            
              ; pattern and source predicates remaining: consume first pattern predicate and combine with possible source-predicates
              ; if more occurrences in pattern than source also keep option that it goes into the pattern-delta
              (t
               (let* ((first-pattern-predicate (first pattern-remaining))
                      (matching-source-predicates
                       (find-all first-pattern-predicate source-remaining
                                 :test (lambda (pattern-predicate source-predicate)
                                         (matching-predicates pattern-predicate source-predicate
                                                                   :allow-generalisation-over-constants
                                                                   allow-generalisation-over-constants)))))
                 (when (or (null matching-source-predicates)
                           (>= (length (find-all first-pattern-predicate (rest pattern-remaining)
                                                 :test (lambda (pattern-predicate remaining-pattern-predicate)
                                                         (matching-predicates pattern-predicate remaining-pattern-predicate
                                                                                   :allow-generalisation-over-constants
                                                                                   allow-generalisation-over-constants))))
                               (length matching-source-predicates)))
                   (push (make-instance 'predicate-alignment-state
                                        :pattern-predicates pattern-predicates
                                        :source-predicates source-predicates
                                        :pattern-delta (append pattern-delta (list first-pattern-predicate))
                                        :source-delta source-delta
                                        :pattern-remaining (rest pattern-remaining)
                                        :source-remaining source-remaining)
                         (queue predicate-alignment-processor)))
                 (when matching-source-predicates
                   (loop for matching-source-predicate in matching-source-predicates
                         do (push (make-instance 'predicate-alignment-state
                                                 :pattern-predicates (append pattern-predicates (list first-pattern-predicate))
                                                 :source-predicates (append source-predicates (list matching-source-predicate))
                                                 :pattern-delta pattern-delta
                                                 :source-delta source-delta
                                                 :pattern-remaining (rest pattern-remaining)
                                                 :source-remaining (remove matching-source-predicate source-remaining))
                                  (queue predicate-alignment-processor))))))))
          
        finally (return nil)))


(defun alignment-quality (alignment)
  "Compute the quality of the alignment in terms of Delta_I"
  (let ((bindings
         (loop for pattern-predicate in (pattern-predicates alignment)
               for source-predicate in (source-predicates alignment)
               append (pairlis (rest pattern-predicate) (rest source-predicate))))
        (counts (make-hash-table :test #'equal)))
    (dolist (b bindings)
      (incf (gethash b counts 0)))
    (loop for count being the hash-value of counts
          when (> count 1) sum count)))


(defmethod anti-unify-predicate-networks (pattern source (mode (eql :baseline))
                                                  &key (cost-mode :msg)
                                                  allow-generalisation-over-constants)
  "Anti-unify pattern and source. Returns a random solutions,
   its cost value, and the number of alignments that were found."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Generate a random alignment between predicates in pattern and source and anti-unify them...
  ;; !!! pattern and source have to be shuffled !!!
  (let* ((predicate-alignment-processor (make-predicate-alignment-processor pattern source))
         (alignment
          (next-possible-alignment predicate-alignment-processor
                                   :allow-generalisation-over-constants
                                   allow-generalisation-over-constants))
         (anti-unification-result
          (multiple-value-bind (resulting-generalisation
                                resulting-pattern-bindings
                                resulting-source-bindings
                                resulting-pattern-delta
                                resulting-source-delta)
              (anti-unify-predicate-sequence (pattern-predicates alignment)
                                             (source-predicates alignment)
                                             nil nil nil
                                             (pattern-delta alignment)
                                             (source-delta alignment))
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
                                                        cost-mode)))))
    (values (list anti-unification-result)
            (cost anti-unification-result)
            (alignment-quality alignment)
            1)))