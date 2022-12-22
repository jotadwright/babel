(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anti-unifying sets of predicates constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

 (ql:quickload :fcg)
 
 (print-anti-unification-results
  (anti-unify-predicate-network '((get-context ?a) (bind animal-cat ?b dog) (filter ?c ?a ?b) (count ?c))
                                '((get-context ?l) (bind animal-cat ?m dog) (filter ?n ?l ?m)
                                  (bind character-cat ?o mean) (filter ?p ?n ?o) (count ?p))
                                :allow-generalisation-over-constants nil))

 (print-anti-unification-results
  (anti-unify-predicate-network '((get-context ?a) (bind animal-cat ?b dog) (filter ?c ?a ?b) (count ?c))
                                '((get-context ?l) (bind animal-cat ?m dog) (filter ?n ?l ?m)(count ?n))
                                :allow-generalisation-over-constants nil))

 (print-anti-unification-results
  (anti-unify-predicate-network '( (bind animal-cat ?b dog))
                                '( (bind animal-cat ?m cat))
                                :allow-generalisation-over-constants nil))

 (print-anti-unification-results
  (anti-unify-predicate-network '((get-context ?a) (bind animal-cat ?b dog) (filter ?c ?a ?b) (count ?c))
                                '((get-context ?l) (bind animal-cat ?m dog) (filter ?n ?l ?m)
                                  (bind character-cat ?o mean) (filter ?p ?n ?o) (count ?p))
                                :allow-generalisation-over-constants nil))

 |#

(defun anti-unify-predicate-network (pattern source &key allow-generalisation-over-constants)
  "Anti-unifies pattern with source. Returns 5 values:
   generalisation, pattern-bindings, source-bindings, pattern-delta and source-delta."
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop for alignment in (identify-possible-alignments pattern source :allow-generalisation-over-constants allow-generalisation-over-constants)
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
                    (:source-delta . ,resulting-source-delta)
                    (:cost . ,(anti-unification-cost resulting-pattern-bindings
                                                     resulting-source-bindings
                                                     resulting-pattern-delta
                                                     resulting-source-delta))))
        into results
          ;; Sort results based on increasing cost.
        finally (return (sort results #'< :key #'(lambda (result) (cdr (assoc :cost result)))))))
  
(defun anti-unify-predicate (pattern
                             source
                             &optional
                             pattern-bindings
                             source-bindings
                             pattern-delta
                             source-delta)
  "Generalises over pattern and source and returns 5 things:
generalisation, pattern-bindings, source-bindings, pattern-delta and source-delta."
  (cond
   ;; Case: pattern equals source -> simply return pattern = source as generalisation
   ((and (atom pattern)
         (atom source)
         (equalp pattern source))
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
    (anti-unify-predicate-sequence pattern source nil pattern-bindings source-bindings pattern-delta source-delta))
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
  "Generalises over two sequences of predicates of the same length, returns generalisation, bindingslists and deltas."

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
        (anti-unify-predicate (first pattern) (first source) pattern-bindings source-bindings pattern-delta source-delta)
      (anti-unify-predicate-sequence (rest pattern)
                                     (rest source)
                                     (append generalisation (list resulting-generalisation))
                                     resulting-pattern-bindings
                                     resulting-source-bindings
                                     resulting-pattern-delta
                                     resulting-source-delta)))))

; (anti-unify-predicate-sequence '(a b c) '(a d c))
; (anti-unify-predicate-sequence '(a b c b) '(a d c e))





(defun identify-possible-alignments (pattern source &key allow-generalisation-over-constants)
  "Returns a list of :pattern :source :pattern-delta :source-delta alists, which list all possibilities
   to align pattern-predicates with source predicates, and the resulting delta's (= non-aligned predicates)."
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
            
          ; pattern and source predicates remaining: consume first pattern predicate and combine with possible source-predicates
          ; if more occurrences in pattern than source also keep option that it goes into the pattern-delta
            (t
             (let* ((first-pattern-predicate (first pattern-remaining))
                    (matching-source-predicates (find-all first-pattern-predicate source-remaining
                                                          :test (lambda (pattern-predicate source-predicate)
                                                                  (matching-predicates pattern-predicate source-predicate
                                                                                       :allow-generalisation-over-constants allow-generalisation-over-constants)))))
               (when matching-source-predicates
                 (loop for matching-source-predicate in matching-source-predicates
                       do (push `((:pattern-predicates . ,(append pattern-predicates (list first-pattern-predicate)))
                                  (:source-predicates . ,(append source-predicates (list matching-source-predicate)))
                                  (:pattern-delta . ,pattern-delta)
                                  (:source-delta . ,source-delta)
                                  (:pattern-remaining . ,(rest pattern-remaining))
                                  (:source-remaining . ,(remove matching-source-predicate source-remaining)))
                                queue)))
               (when (or (null matching-source-predicates)
                         (> (length (find-all first-pattern-predicate (rest pattern-remaining)
                                              :test (lambda (pattern-predicate remaining-pattern-predicate)
                                                      (matching-predicates pattern-predicate remaining-pattern-predicate
                                                                           :allow-generalisation-over-constants allow-generalisation-over-constants))))
                            (length matching-source-predicates)))
                 (push `((:pattern-predicates . ,pattern-predicates)
                         (:source-predicates . ,source-predicates)
                         (:pattern-delta . ,(append pattern-delta (list first-pattern-predicate)))
                         (:source-delta . ,source-delta)
                         (:pattern-remaining . ,(rest pattern-remaining))
                         (:source-remaining . ,source-remaining))
                       queue)))))
          
        finally (return solutions)))

#|
(identify-possible-alignments '((get-context ?c) (bind animal-cat ?dog-cat dog) (filter ?dogs ?c ?dog-cat) (count ?dogs))
                              '((get-context ?c) (bind animal-cat ?dog-cat dog) (filter ?dogs ?c ?dog-cat)
                                (bind character-cat ?mean-cat mean) (filter ?mean-dogs ?dogs ?mean-cat) (count ?mean-dogs)))
|#

(defun matching-predicates (predicate-1 predicate-2 &key allow-generalisation-over-constants)
  "Returns t if predicate-1 and predicate-2 can be aligned: same feature name and same arity.
   By default, returns nil if a constant occurs in one predicate and the same constant does
   not appear at the same position in the other predicate."
  (and
   ;; predicate names are equal
   (equalp (first predicate-1) (first predicate-2))
   ;; arity is equal
   (= (length predicate-1) (length predicate-2))
   ;; allow or disallow generalisation over constants
   (if allow-generalisation-over-constants
     t
     (loop for arg-1 in (rest predicate-1)
           for arg-2 in (rest predicate-2)
           always (or (equalp arg-1 arg-2)
                      (and (variable-p arg-1)
                           (variable-p arg-2)))))))

;; (matching-predicates '(p ?a b ?c) '(p ?x b ?y) :allow-generalisation-over-constants t)
;; (matching-predicates '(p ?a b ?c) '(p ?x c ?y) :allow-generalisation-over-constants t)
;; (matching-predicates '(p ?a b ?c) '(p ?x b ?y) :allow-generalisation-over-constants nil)
;; (matching-predicates '(p ?a b ?c) '(p ?x c ?y) :allow-generalisation-over-constants nil)
;; (matching-predicates '(p ?a b ?c) '(f ?x b ?y) :allow-generalisation-over-constants nil)

(defun print-anti-unification-results (list-of-anti-unification-results &optional (stream t))
  "Prints a list of anti-unification results."

  ;; Double check
  (assert (loop with ori-pattern = (compute-network-from-anti-unification-result (first list-of-anti-unification-results) 'pattern)
                with ori-source = (compute-network-from-anti-unification-result (first list-of-anti-unification-results) 'source)
                for a-u-result in (rest list-of-anti-unification-results)
                for this-ori-pattern = (compute-network-from-anti-unification-result a-u-result 'pattern)
                for this-ori-source = (compute-network-from-anti-unification-result a-u-result 'source)
                always (and (equivalent-predicate-networks this-ori-pattern ori-pattern)
                            (equivalent-predicate-networks this-ori-source ori-source))))
  
  (format t "~%~%~%### Anti-unifying ###~%~%")
  (format stream "------------------------------------------------------------~%")
  (format stream "- Pattern:~%~%")
  (let ((*print-pretty* t))
             (format stream "~(~a~)~%~%" (compute-network-from-anti-unification-result (first list-of-anti-unification-results) 'pattern)))
  (format stream "- Source:~%~%")
  (let ((*print-pretty* t))
    (format stream "~(~a~)~%~%" (compute-network-from-anti-unification-result (first list-of-anti-unification-results) 'source)))
  (format stream "------------------------------------------------------------~%~%")
  (loop for a-u-result in list-of-anti-unification-results
        for i from 1 upto (length list-of-anti-unification-results)
        for generalisation = (cdr (assoc :generalisation a-u-result))
        for pattern-bindings = (cdr (assoc :pattern-bindings a-u-result))
        for source-bindings = (cdr (assoc :source-bindings a-u-result))
        for pattern-delta = (cdr (assoc :pattern-delta a-u-result))
        for source-delta = (cdr (assoc :source-delta a-u-result))
        for cost = (cdr (assoc :cost a-u-result))


        do (format stream "--- Result ~a (cost: ~a) ---~%~%" i cost)
           (format stream "- Generalisation:~%~%")
           (let ((*print-pretty* t))
             (format stream "~(~a~)~%~%" generalisation))
           (format stream "- Pattern bindings:~%~%")
           (let ((*print-pretty* t))
             (format stream "~(~a~)~%~%" pattern-bindings))
           (format stream "- Source bindings:~%~%")
           (let ((*print-pretty* t))
             (format stream "~(~a~)~%~%" source-bindings))
           (format stream "- Pattern delta:~%~%")
           (let ((*print-pretty* t))
             (format stream "~(~a~)~%~%" pattern-delta))
           (format stream "- Source delta:~%~%")
           (let ((*print-pretty* t))
             (format stream "~(~a~)~%~%~%" source-delta))))


(defun anti-unification-cost (pattern-bindings source-bindings pattern-delta source-delta)
  "The anti-unification cost is the sum of the number of predicates in the deltas and the number of variables that have
   been bound two more than 1 variable in the generalisation."
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

(defun compute-network-from-anti-unification-result (au-result pattern-or-source)
  "Returns original network based on generalisation, bindings-list and delta."  
  (let* ((generalisation (cdr (assoc :generalisation au-result)))
         (bindings-key (cond ((eql pattern-or-source 'pattern)
                              :pattern-bindings)
                             ((eql pattern-or-source 'source)
                              :source-bindings)
                             (t (error "The pattern or source argument should be 'pattern or 'source (got ~a)" pattern-or-source))))
         (delta-key (cond ((eql pattern-or-source 'pattern)
                           :pattern-delta)
                          ((eql pattern-or-source 'source)
                           :source-delta)))
         (bindings-list (cdr (assoc bindings-key au-result)))
         (delta (cdr (assoc delta-key au-result))))
    (append (substitute-bindings (reverse-bindings bindings-list) generalisation)
            delta)))
                            
    