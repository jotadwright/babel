(in-package :au-lib)

(export '(anti-unification-cost))

(defgeneric anti-unification-cost (pattern source generalisation
                                           pattern-delta source-delta
                                           pattern-bindings source-bindings
                                           mode)
  (:documentation "Compute the cost of the anti-unification result"))

#|
(defun anti-unification-cost (pattern-bindings source-bindings pattern-delta source-delta)
  "The anti-unification cost is the sum of the number of predicates in the deltas and the number of variables that have
   been bound to more than 1 variable in the generalisation."
  (let ((nr-of-predicates-in-pattern-delta (length pattern-delta))
        (nr-of-predicates-in-source-delta (length source-delta))
        (nr-of-bindings-to-multiple-vars-in-pattern (loop for (binding . rest) on  pattern-bindings
                                                          count (find (car binding) rest :key #'car :test #'equalp)))
        (nr-of-bindings-to-multiple-vars-in-source (loop for (binding . rest) on  source-bindings
                                                         count (find (car binding) rest :key #'car :test #'equalp))))
    (+ nr-of-predicates-in-pattern-delta
       nr-of-predicates-in-source-delta
       nr-of-bindings-to-multiple-vars-in-pattern
       nr-of-bindings-to-multiple-vars-in-source)))
|#


(defmethod anti-unification-cost (pattern source generalisation
                                  pattern-delta source-delta
                                  pattern-bindings source-bindings
                                  (mode (eql :lcg)))
  "Count the number of missing predicates (size of delta's)."
  (+ (length pattern-delta)
     (length source-delta)))


(defmethod anti-unification-cost (pattern source generalisation
                                  pattern-delta source-delta
                                  pattern-bindings source-bindings
                                  (mode (eql :distinct-variables)))
  (let* ((all-variables (find-all-anywhere-if #'variable-p generalisation))
         (unique-variables (remove-duplicates all-variables)))
    (length unique-variables)))


(defmethod anti-unification-cost (pattern source generalisation
                                  pattern-delta source-delta
                                  pattern-bindings source-bindings
                                  (mode (eql :default)))
  "The anti-unification cost is the sum of the number of predicates
   in the deltas and the number of variables that have
   been bound to more than 1 variable in the generalisation."        
  (let ((nr-of-predicates-in-pattern-delta (length pattern-delta))
        (nr-of-predicates-in-source-delta (length source-delta))
        (nr-of-bindings-to-multiple-vars-in-pattern
         (loop for (binding . rest) on pattern-bindings
               count (find (car binding) rest :key #'car :test #'equalp)))
        (nr-of-bindings-to-multiple-vars-in-source
         (loop for (binding . rest) on source-bindings
               count (find (car binding) rest :key #'car :test #'equalp))))
    (+ nr-of-predicates-in-pattern-delta
       nr-of-predicates-in-source-delta
       nr-of-bindings-to-multiple-vars-in-pattern
       nr-of-bindings-to-multiple-vars-in-source)))


(defmethod anti-unification-cost (pattern source generalisation
                                  pattern-delta source-delta
                                  pattern-bindings source-bindings
                                  (mode (eql :msg)))
  "Count the number of missing predicates (size of delta's) and count
   the number of missing variable links (total number of links in pattern and source
   minus the number of links in the generalisation)."
  (let* ((all-pattern-args (mappend #'cdr pattern))
         (all-source-args (mappend #'cdr source))
         (all-generalisation-args (mappend #'cdr generalisation))
         (number-of-links-in-pattern
          (loop for (arg . rest) on all-pattern-args
                sum (count arg rest)))
         (number-of-links-in-source
          (loop for (arg . rest) on all-source-args
                sum (count arg rest)))
         (number-of-links-in-generalisation
          (loop for (arg . rest) on all-generalisation-args
                sum (count arg rest)))
         (nr-of-predicates-in-pattern-delta (length pattern-delta))
         (nr-of-predicates-in-source-delta (length source-delta)))
    (+ nr-of-predicates-in-pattern-delta
       nr-of-predicates-in-source-delta
       (- number-of-links-in-pattern number-of-links-in-generalisation)
       (- number-of-links-in-source number-of-links-in-generalisation))))


(defmethod anti-unification-cost (pattern source generalisation
                                  pattern-delta source-delta
                                  pattern-bindings source-bindings
                                  (mode (eql :pattern-finding)))
  "Count the number of missing predicates (i.e. size of the delta's)
   and count the number of broken variable links both in the generalisation
   and in the delta's (i.e. total number of links in pattern and source
   minus the number of links in the generalisation and the corresponding
   delta)."
  ;; Q: do we always want to take the links in pattern/source delta into account
  ;;    or only as a tie-breaker for generalisations with the same cost?
  (let* ((all-pattern-args (mappend #'cdr pattern))
         (all-source-args (mappend #'cdr source))
         (all-generalisation-args (mappend #'cdr generalisation))
         (all-pattern-delta-args (mappend #'cdr pattern-delta))
         (all-source-delta-args (mappend #'cdr source-delta))
         (number-of-links-in-pattern
          (loop for (arg . rest) on all-pattern-args
                sum (count arg rest)))
         (number-of-links-in-source
          (loop for (arg . rest) on all-source-args
                sum (count arg rest)))
         (number-of-links-in-generalisation
          (loop for (arg . rest) on all-generalisation-args
                sum (count arg rest)))
         (number-of-links-in-pattern-delta
          (loop for (arg . rest) on all-pattern-delta-args
                sum (count arg rest)))
         (number-of-links-in-source-delta
          (loop for (arg . rest) on all-source-delta-args
                sum (count arg rest)))
         (nr-of-predicates-in-pattern-delta (length pattern-delta))
         (nr-of-predicates-in-source-delta (length source-delta)))
    (+ nr-of-predicates-in-pattern-delta
       nr-of-predicates-in-source-delta
       (- number-of-links-in-pattern number-of-links-in-generalisation number-of-links-in-pattern-delta)
       (- number-of-links-in-source number-of-links-in-generalisation number-of-links-in-source-delta))))