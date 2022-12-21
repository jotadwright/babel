(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anti-unifying sets of predicates constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(pprint
 (anti-unify-predicate-network '((get-context ?c) (bind animal-cat ?dog-cat dog) (filter ?dogs ?c ?dog-cat) (count ?dogs))
                              '((get-context ?c) (bind animal-cat ?dog-cat dog) (filter ?dogs ?c ?dog-cat)
                              (bind character-cat ?mean-cat mean) (filter ?mean-dogs ?dogs ?mean-cat) (count ?mean-dogs))))
|#

(defun anti-unify-predicate-network (pattern source)
  "Anti-unifies pattern with source. Returns 5 values:
   generalisation, pattern-bindings, source-bindings, pattern-delta and source-delta."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop all possible alignments of predicates in pattern and source (with maximum length)
  (loop for alignment in (identify-possible-alignments pattern source)
        for pattern-in-alignment = (cdr (assoc :pattern alignment))
        for source-in-alignment = (cdr (assoc :source alignment))
        for pattern-delta = (cdr (assoc :pattern-delta alignment))
        for source-delta = (cdr (assoc :source-delta alignment))
        collect (multiple-value-bind (resulting-generalisation resulting-pattern-bindings resulting-source-bindings resulting-pattern-delta resulting-source-delta)
                    (anti-unify-predicate-sequence pattern-in-alignment source-in-alignment nil nil nil pattern-delta source-delta)
                  `((:generalisation . ,resulting-generalisation)
                    (:pattern-bindings . ,resulting-pattern-bindings)
                    (:source-bindings . ,resulting-source-bindings)
                    (:pattern-delta . ,resulting-pattern-delta)
                    (:source-delta . ,resulting-source-delta)))))
  
(defun identify-possible-alignments (pattern source)

  (loop with solutions = nil
        with queue = (list `((:pattern-predicates . nil)
                             (:source-predicates . nil)
                             (:pattern-delta . nil)
                             (:source-delta . nil)
                             (:pattern-remaining . ,pattern)
                             (:source-remaining . ,source)))
        while queue
        for state = (pop queue)
        for pattern-predicates = (cdr (assoc :pattern-predicates state))
        for source-predicates = (cdr (assoc :source-predicates state))
        for pattern-delta = (cdr (assoc :pattern-delta state))
        for source-delta = (cdr (assoc :source-delta state))
        for pattern-remaining = (cdr (assoc :pattern-remaining state))
        for source-remaining = (cdr (assoc :source-remaining state))

        do (cond ; no predicates remaining in pattern and source: we have a solution!
            ((and (null pattern-remaining) (null source-remaining))
             (push `((:pattern . ,pattern-predicates)
                     (:source . ,source-predicates)
                     (:pattern-delta . ,pattern-delta)
                     (:source-delta . ,source-delta))
                   solutions))
            ; no pattern-predicates remaining: rest of source goes to source-delta
            ((null pattern-remaining)
             (push `((:pattern-predicates . ,pattern-predicates)
                     (:source-predicates . ,source-predicates)
                     (:pattern-delta . ,pattern-delta)
                     (:source-delta . ,(append source-delta source-remaining))
                     (:pattern-remaining . nil)
                     (:source-remaining . nil))
                   queue))
            ; no source-predicates remaining: rest of source goes to source-delta
            ((null source-remaining)
             (push `((:pattern-predicates . ,pattern-predicates)
                     (:source-predicates . ,source-predicates)
                     (:pattern-delta . ,(append pattern-delta pattern-remaining))
                     (:source-delta . ,source-delta)
                     (:pattern-remaining . nil)
                     (:source-remaining . nil))
                   queue))
          ; pattern and source predicates remaining: consume pattern predicate and combine with possible source-predicates
          ; if more occurrences in pattern than source also keep option that it goes into the pattern-delta
            (t
             (loop with first-pattern-predicate = (first pattern-remaining)
                   with matching-source-predicates = (find-all first-pattern-predicate source-remaining
                                                               :test (lambda (pattern-predicate source-predicate)
                                                                       (and (equalp (feature-name pattern-predicate)
                                                                                    (feature-name source-predicate))
                                                                            (= (length pattern-predicate)
                                                                               (length source-predicate)))))
                   for matching-source-predicate in matching-source-predicates
                   do (push `((:pattern-predicates . ,(append pattern-predicates (list first-pattern-predicate)))
                              (:source-predicates . ,(append source-predicates (list matching-source-predicate)))
                              (:pattern-delta . ,pattern-delta)
                              (:source-delta . ,source-delta)
                              (:pattern-remaining . ,(rest pattern-remaining))
                              (:source-remaining . ,(remove matching-source-predicate source-remaining)))
                            queue)
                      (when (> (length (find-all first-pattern-predicate (rest pattern-remaining)
                                                 :test (lambda (pattern-predicate source-predicate)
                                                         (and (equalp (feature-name pattern-predicate)
                                                                      (feature-name source-predicate))
                                                              (= (length pattern-predicate)
                                                                 (length source-predicate))))))
                               (length matching-source-predicates))
                        (push `((:pattern-predicates . ,pattern-predicates)
                                (:source-predicates . ,source-predicates)
                                (:pattern-delta . ,(append pattern-delta (list (first pattern-remaining))))
                                (:source-delta . ,source-delta)
                                (:pattern-remaining . ,(rest pattern-remaining))
                                (:source-remaining . ,source-remaining))
                              queue)
                        ))
            
            
             ))
        finally (return solutions)))





(defun anti-unify-predicate (pattern
                             source
                             &optional
                             generalisation
                             pattern-bindings
                             source-bindings
                             pattern-delta
                             source-delta)
  "Generalises over pattern and source and returns 5 things:
generalisation, pattern-bindings, source-bindings, pattern-delta and source-delta."

  (cond
   
   ;; Case: pattern equals source -> simply return pattern = source as generalisation
   ((equalp pattern source)
    (values pattern
            pattern-bindings
            source-bindings
            pattern-delta
            source-delta))
   
   ;; Case: pattern and source are different, but already share a binding in the bindingslist -> return binding
   ;; as generalisation
   ((subs-lookup pattern-bindings source-bindings pattern source)
    (values (subs-lookup pattern-bindings source-bindings pattern source)
            pattern-bindings
            source-bindings
            pattern-delta
            source-delta))

   ;; Case: pattern and source are predicates with the same name and the same arity -> return predicate with
   ;; anti-unified arguments
   ((and (listp pattern) (listp source)
         (atom (feature-name source)) (atom (feature-name pattern))
         (equalp (feature-name source) (feature-name pattern))
         (= (length pattern) (length source)))
    (anti-unify-predicate-sequence pattern source generalisation pattern-bindings source-bindings pattern-delta source-delta))

   ;; None of the above (pattern and source are different and no binding is available yet) -> introduce new
   ;; binding as generalisation
   (t
    (let ((var (make-var "X")))
      (values var
              (extend-bindings pattern var pattern-bindings)
              (extend-bindings source var source-bindings)
              pattern-delta
              source-delta)))))






(defun anti-unify-predicate-sequence (pattern
                                      source
                                      &optional
                                      generalisation
                                      pattern-bindings
                                      source-bindings
                                      pattern-delta
                                      source-delta)
  "Anti-unify two sequences of the same length, return generalisation, bindingslists and deltas."

  ;; Assert that the length of the pattern and the source are equal
  (assert (= (length pattern) (length source)))
  (cond
   ;; Base case: no elements anymore, return accumulator and bindings-lists
   ((and (null pattern) (null source))
    (values generalisation
            pattern-bindings
            source-bindings
            pattern-delta
            source-delta))
   ;; Recursive case: still elements left, anti-unify first and then rest, every time passing the new bindings and deltas, accumulating the generalisation
   (t
    (multiple-value-bind (resulting-generalisation resulting-pattern-bindings resulting-source-bindings resulting-pattern-delta resulting-source-delta)
        (anti-unify-predicate (first pattern) (first source) generalisation pattern-bindings source-bindings pattern-delta source-delta)
      (anti-unify-predicate-sequence (rest pattern)
                                     (rest source)
                                     (pushend resulting-generalisation generalisation)
                                     resulting-pattern-bindings
                                     resulting-source-bindings
                                     resulting-pattern-delta
                                     resulting-source-delta)))))

; (anti-unify-predicate-network-sequence '(a b c) '(a d c))
; (anti-unify-predicate-network-sequence '(a b c b) '(a d c e))

                              
