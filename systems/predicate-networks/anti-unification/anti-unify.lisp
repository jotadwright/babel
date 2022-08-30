(in-package :pn)

;; ############################################################################
;; Anti-unification of predicate networks.
;; ----------------------------------------------------------------------------

(defparameter +no-bindings+ '((t . t)))
(defparameter +fail+ nil)
(defparameter +empty-accumulator+ nil)
(defparameter +initial-cost+ 0)

(defmethod anti-unify (pattern source &optional
                               (pattern-bindings +no-bindings+)
                               (source-bindings +no-bindings+)
                               &key cost-params)
  (if (<= (length pattern) (length source))
    (let* ((cost-params (or cost-params
                            '((equality . 0)
                              (subst . 0)
                              (new-var . 1)
                              (generalise-predicate . 5))))
           ;; Reorder the source network such that it aligns with pattern
           (source-network-reorderings
            (reorder-source-network pattern source cost-params))
           anti-unification-results)
      ;; For every reordering solution, update the pattern (if predicates need to be removed)
      ;; and try anti-unification. Return the resulting pattern, bindings lists for pattern
      ;; and source, cost (reordering cost + anti-unification cost) and predicates that
      ;; were removed from pattern.
      (loop for (reordered-source reordering-cost predicates-to-remove) in source-network-reorderings
            for updated-pattern = (reverse (set-difference pattern predicates-to-remove :test #'equal))
            do (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings au-cost)
                   (anti-unify-networks updated-pattern reordered-source pattern-bindings source-bindings 'network-level cost-params)
                 (push (list resulting-pattern
                             resulting-pattern-bindings
                             resulting-source-bindings
                             (+ reordering-cost au-cost)
                             predicates-to-remove)
                       anti-unification-results)))
      (show-anti-unification-results-on-web-interface
       pattern source anti-unification-results)
      (sort anti-unification-results #'< :key #'fourth))
    (error "Anti-unifying a predicate network of length ~a with a predicate network of length ~a is impossible."
           (length pattern) (length source))))


(defun anti-unify-networks (pattern source pattern-bindings source-bindings level cost-params)
  (cond
   ;; case: pattern equals source. Return pattern and all bindings
   ((equalp pattern source)
    (values pattern
            pattern-bindings
            source-bindings
            (assqv 'equality cost-params)))
   ;; case: pattern and source where already substituted by the
   ;; same binding. Return this binding and all bindings for pattern
   ;; and source
   ((subs-lookup pattern-bindings source-bindings pattern source)
    (values (subs-lookup pattern-bindings source-bindings pattern source)
            pattern-bindings
            source-bindings
            (assqv 'subst cost-params)))
   ;; case: pattern and source are sequences, anti-unify them
   ((and (not (variable-p pattern))
         (not (variable-p source))
         (listp pattern)
         (listp source)
         (= (length pattern) (length source))
         (anti-unify-sequence pattern source +empty-accumulator+ pattern-bindings source-bindings +initial-cost+ level cost-params))
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-cost)
        (anti-unify-sequence pattern source +empty-accumulator+ pattern-bindings source-bindings +initial-cost+ level cost-params)
      ;; when anti-unifying sequences on the predicate level,
      ;; check if the entire predicate needs to be generalised over
      (if (eql level 'predicate-level)
        (anti-unify-predicate pattern source pattern-bindings source-bindings cost-params
                              resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-cost)
        (values resulting-pattern
                resulting-pattern-bindings
                resulting-source-bindings
                resulting-cost))))
   ;; case: non of the above, replace both pattern and source by a new binding
   (t (let ((var (make-var)))
        (values var
                (extend-bindings pattern var pattern-bindings)
                (extend-bindings source var source-bindings)
                (assqv 'new-var cost-params))))))


(defun anti-unify-predicate (pattern source pattern-bindings source-bindings cost-params
                             resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-cost)
  ;; When anti-unifiction of two predicates returns a bindings list
  ;; where only variables are bound to other variables, do nothing.
  
  ;; When anti-unification of two predicates returns a bindings list
  ;; which has at least one constant, you probably want to generalise
  ;; over the entire predicate (is this always true??)
  ;; In that case, remove the bindings that occur only in that predicate
  ;; from the bindings lists and generalise over the entire predicate,
  ;; applying any bindings that are available...
  
  (let ((new-pattern-bindings (set-difference resulting-pattern-bindings pattern-bindings :test #'equal))
        (new-source-bindings (set-difference resulting-source-bindings source-bindings :test #'equal)))
    (if (and (all-variable-renamings-p new-pattern-bindings)
             (all-variable-renamings-p new-source-bindings))
      (values resulting-pattern
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-cost)
      ;; remove bindings that only occur in the current pattern and source, respectively
      ;; and anti-unify over the entire predicate, i.e. adding a single variable to the
      ;; resulting pattern and providing bindings for the entire predicate in both
      ;; pattern and source bindings
      (let* ((pattern-bindings-variables
              (find-all-if #'variable-p resulting-pattern-bindings :key #'car))
             (source-bindings-variables
              (find-all-if #'variable-p resulting-source-bindings :key #'car))
             (pattern-w/-vars-subst
              (apply-bindings pattern pattern-bindings-variables))
             (source-w/-vars-subst
              (apply-bindings source source-bindings-variables))
             (all-variables-in-new-pattern
              (find-all-if #'variable-p pattern-w/-vars-subst))
             (predicate-var (make-var 'predicate))
             (new-pattern-bindings-constants
              (find-all-if-not #'variable-p new-pattern-bindings :key #'car))
             (new-source-bindings-constants
              (find-all-if-not #'variable-p new-source-bindings :key #'car))
             (updated-pattern-bindings
              ;; alle resulting pattern bindings - new-pattern-bindings dat constants zijn + nieuwe binding
              (extend-bindings pattern-w/-vars-subst predicate-var 
                               (set-difference resulting-pattern-bindings new-pattern-bindings-constants
                                               :test #'equal)))
             (updated-source-bindings
              (extend-bindings source-w/-vars-subst predicate-var 
                               (set-difference resulting-source-bindings new-source-bindings-constants
                                               :test #'equal))))
        (values (cons predicate-var all-variables-in-new-pattern)
                updated-pattern-bindings
                updated-source-bindings
                (assqv 'generalise-predicate cost-params))))))


(defun anti-unify-sequence (pattern source accumulator pattern-bindings source-bindings cost level cost-params)
  (let ((next-level
         (case level
           (network-level 'predicate-level)
           (predicate-level 'argument-level)
           (argument-level nil))))
    (cond
     ;; case: reached end of sequence, return accumulator and bindings lists
     ((and (null pattern) (null source))
      (values accumulator
              pattern-bindings
              source-bindings
              cost))
     ;; case: anti-unify the cars, and try to anti-unify the cdrs with resulting bindings
     (t
      (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-cost)
          (anti-unify-networks (car pattern) (car source) pattern-bindings source-bindings next-level cost-params)
        (anti-unify-sequence (cdr pattern) (cdr source)
                             (pushend resulting-pattern accumulator)
                             resulting-pattern-bindings
                             resulting-source-bindings
                             (+ cost resulting-cost)
                             level
                             cost-params))))))
  

(defun subs-lookup (pattern-bindings source-bindings pattern source)
  "returns the binding for a given pattern and source in the bindings-lists"
  (cond
   ;; Case: no pattern-bindings left
   ((null pattern-bindings)
    +fail+)
   ;; Case: see whether pattern and source are first elements of bindings, if so, return binding
   ((and (equal (car (first pattern-bindings)) pattern)
         (equal (car (first source-bindings)) source)
         (equal (cdr (first pattern-bindings)) (cdr (first source-bindings))))
    (cdr (first pattern-bindings)))
   ;; Case: otherwise compare rests
   (t
    (subs-lookup (cdr pattern-bindings) (cdr source-bindings) pattern source))))


(defun apply-bindings (thing bindings)
  (cond
   ((consp thing)
    (cons (apply-bindings (car thing) bindings)
          (apply-bindings (cdr thing) bindings)))
   ((atom thing)
    (if (assoc thing bindings)
      (assqv thing bindings)
      thing))
   ((null thing) nil)))


(defun make-binding (var val)
  (cons var val))


(defun get-binding (var bindings)
  "Returns a binding (a cons cell of the form (var . value)) for the variable
   var as specified in bindings or NIL if var is unbound in bindings."
  (assoc var bindings))


(defun extend-bindings (var val bindings)
  "Adds the binding of var to val to bindings."
  (assert (not (get-binding var bindings)))
  (cons (make-binding var val)
        (if (equal bindings +no-bindings+)
          nil bindings)))
          

(defun all-variable-renamings-p (bindings)
  (loop for (x . y) in bindings
        always (and (variable-p x)
                    (variable-p y))))