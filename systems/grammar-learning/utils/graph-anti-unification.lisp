; search for a predicate with the same arity (predicate-1 ?a) vs (predicate-1 ?a) => bindings (?a . ?a)
; see the anti-unification of cxns and transient structure, here too subnetworks are resolved!
; step 1: pair all matching primitives with the same arity. is there only 1: yay! if there are multiple, list all possible combinations, see reorder-source-units

; reorderings are incorrect!
; units = predicates, top-level-features = predicates too?



(in-package :fcg)

(defmethod anti-unify (pattern source (mode (eql :graphs)) &optional
                               (pattern-bindings +no-bindings+)
                               (source-bindings +no-bindings+)
                               &key (cost-params nil))
  "anti-unifies an fcg-pattern, including special operators and a source. Returns the resulting
   least general generalisation as well as the binding lists for pattern and source and the cost
   of the anti-unification (calculated based on cost-params)"
  ;; Source should contain more units than pattern, then call helper function, fail otherwise
  (if (<= (length pattern) (+ 1 (length source)))
    ;; Get units from source to anti-unify pattern against (ordered)
    (let* ((cost-params (or cost-params '((equality 0) ;; don't punish when source is equal to pattern 
                                          (non-matching-predicate 10) ;; Punish badly non-matching units
                                          (subst-from-bindingslist 0)
                                          (source-variable-pattern-in-bindingslist 1)
                                          (replace-by-new-var depth-of-replaced-pattern 1)
                                          (discarded-feature 5)
                                          (discarded-negated-feature 4)
                                          (removed-pattern-predicate 20))))
           (source-predicate-reorderings (reorder-source-predicates pattern source cost-params))
          anti-unification-results)
      (dolist (s-u-r source-predicate-reorderings)
          (let ((updated-pattern (remove (first (third s-u-r)) pattern :test 'equalp :key 'unit-name)))
            (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features a-u-cost)
                (anti-unify-graphs updated-pattern (first s-u-r) pattern-bindings source-bindings 'unit-level cost-params)
              (let ((total-cost (+ (second s-u-r) a-u-cost))
                    (removed-pattern-units (third s-u-r)))
                (push (list resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features total-cost removed-pattern-units) anti-unification-results)))))
      (sort anti-unification-results '< :key 'fifth))
    (error (format nil "Anti-unifying a construction containing ~a unit(s) with a transient structure containing ~a unit(s). This is impossible. Please make sure the transient structure contains more units." (length pattern) (length source)))))

(defun anti-unify-graphs (pattern source pattern-bindings source-bindings level cost-params)
  ;; Case: equality of pattern and source
  (cond
   ((equalp pattern source)
    (values pattern
            pattern-bindings
            source-bindings
            '() ;; discarded features
            (get-anti-unification-cost 'equality cost-params pattern source)))
   ;; Substitution is already in bindingslist
   ((subs-lookup pattern-bindings source-bindings pattern source)
    (values (subs-lookup pattern-bindings source-bindings pattern source)
            pattern-bindings
            source-bindings
            '()
            (get-anti-unification-cost 'subst-from-bindingslist cost-params pattern source)))
   ;; Case: unit level: unit-name can be different
   ((and (equalp level 'unit-level)
         (anti-unify-fcg-sequence pattern source '() pattern-bindings source-bindings '() 'unit-level cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
        (anti-unify-fcg-sequence pattern source '() pattern-bindings source-bindings '() 'unit-level cost-params 0)
      (values resulting-pattern
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-discarded-features
              resulting-cost)))
   ;; Case: top-feature level (eg: syn-cat, sem-cat, args, subunits,...): no special operator, but still subset;; feature name should be exact
   ((and (equalp level 'top-feature-level)
         (anti-unify-fcg-set (rest pattern) (rest source) '() pattern-bindings source-bindings '() 'top-feature-level cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern-1 resulting-pattern-bindings-1 resulting-source-bindings-1 resulting-discarded-features-1 resulting-cost-1)
        (anti-unify-fcg (first pattern) (first source) pattern-bindings source-bindings nil cost-params)
      (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
          (anti-unify-fcg-set (rest pattern) (rest source) '() resulting-pattern-bindings-1 resulting-source-bindings-1 '() 'top-feature-level cost-params resulting-cost-1)
        (values (append (list resulting-pattern-1) resulting-pattern)
                resulting-pattern-bindings
                resulting-source-bindings
                (if resulting-discarded-features-1
                  (push resulting-discarded-features-1 resulting-discarded-features)
                  resulting-discarded-features)
                resulting-cost))))
   ;; Case: subset with special operator ==1 or ==1
   ((and (listp pattern)
         (or (equalp (first pattern) '==1)
             (equalp (first pattern) '==))
         (anti-unify-fcg-set (rest pattern) source '() pattern-bindings source-bindings '() nil cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
        (anti-unify-fcg-set (rest pattern) source '() pattern-bindings source-bindings '() nil cost-params 0)
      (values (append (list (first pattern)) resulting-pattern)
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-discarded-features
              resulting-cost)))
   ;; Case: ==0
   ((and (listp pattern)
         (equalp (first pattern) '==0)
         (anti-unify-fcg-excludes (rest pattern) source '() pattern-bindings source-bindings '() cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
        (anti-unify-fcg-excludes (rest pattern) source '() pattern-bindings source-bindings '() cost-params 0)
      (values (append (list (first pattern)) resulting-pattern)
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-discarded-features
              resulting-cost)))
   ;; Case: pattern and source have same feature-name and arity (number of arguments)
   ;;       anti-unify the arguments, return resulting pattern and all bindings for source and pattern
   ((and (not (variable-p pattern))
         (not (variable-p source))
         (listp pattern)
         (listp source)
         (not (get-so (first pattern)))
         (= (length pattern) (length source))
          ;(equalp (feature-name source) (feature-name pattern)) ;; restricting anti-unification for same feature
         (anti-unify-fcg-sequence pattern source '() pattern-bindings source-bindings '() nil cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
        (anti-unify-fcg-sequence pattern source '() pattern-bindings source-bindings '() nil cost-params 0)
      (values resulting-pattern
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-discarded-features
              resulting-cost)))
   ;; Source is variable, pattern is already in bindingslist, then return its binding
   ((and (variable-p source)
         (assoc pattern pattern-bindings))
    (values (cdr (assoc pattern pattern-bindings))
            pattern-bindings
            source-bindings
            nil
            (get-anti-unification-cost 'source-variable-pattern-in-bindingslist cost-params pattern source)))
   ;; Else-case: introduce new variable
   (t
    (let ((var (make-var (if (atom pattern)
                           pattern
                           nil))))
      (values var
              (extend-bindings pattern var pattern-bindings)
              (extend-bindings source var source-bindings)
              nil
              (get-anti-unification-cost 'replace-by-new-var cost-params pattern source))))))

(defun reorder-source-predicates (pattern source cost-params)
  "Input: list of cxn-units as pattern
          list of transient structure units as source
          list of cost-paramaters for calculating reordering-cost
   Returns: list of subsets of source with:
            - as many units as in pattern
            - aligned order (matching-units at same place, all combinations of other units)"
  (let ((reordering-solutions (find-matching-predicates pattern source cost-params))
        (list-of-reordered-sources nil))
    ;; Loop over possible reorderings
    (dolist (rs reordering-solutions)
      (let ((solution-structure (solution-structure rs))
            (reordered-source (loop for n from 1 to (length pattern)
                                    collect n))
            (removed-pattern-predicates nil)
            (pattern-predicate-to-remove nil))
        ;; Loop over predicates
        (dolist (predicate solution-structure)
          (if (equalp (cdr predicate) 'remove-predicate)
            (setf pattern-predicate-to-remove (car predicate))
            (let* ((pattern-index (- (car predicate) 1))
                   (source-index (- (cdr predicate) 1))
                   (source-feature (nth source-index source)))
              (setf (nth pattern-index reordered-source) source-feature))))
        (if pattern-predicate-to-remove
          (progn
            (setf reordered-source (delete pattern-predicate-to-remove reordered-source))
            (push (nth (- pattern-predicate-to-remove 1) pattern) removed-pattern-predicates)
            (push (list reordered-source (cost rs) removed-pattern-predicates) list-of-reordered-sources))
          (push (list reordered-source (cost rs) nil) list-of-reordered-sources))))
    (sort list-of-reordered-sources '< :key 'second)))

(defun find-matching-predicates (pattern source cost-params)
  "given pattern and source (lists of fcg-units), and cost-params return:
   - list of objects with all possibilities for binding units in pattern with source, maximizing number of matching
     units objects contain: solution-structure, cost and non-matched-units of source"
  (let ((source-predicate-numbers (loop for n from 1 upto (length source)
                              collect n))
        (all-matching-predicates nil)
        (pattern-length (length pattern)))
    (loop for pattern-predicate in pattern
          for i from 1 upto pattern-length
          do
          (let ((matches-for-this-predicate nil))
            (loop for source-predicate in source
                  for j from 1 upto (length source)
                  do
                  (when (match-predicates (list pattern-predicate) (list source-predicate))
                    (push j matches-for-this-predicate)))
            (push (cons i matches-for-this-predicate) all-matching-predicates)))
    (search-predicate-reordering-solutions all-matching-predicates
                                 (if (> (length pattern) 1)
                                   (cons 'remove-predicate source-predicate-numbers)
                                   source-predicate-numbers)
                                 pattern-length
                                 cost-params)))

(defclass reordering-predicates-search-state ()
  ((solution-structure
    :type (or list null)
    :initform nil
    :initarg :solution-structure
    :accessor solution-structure)
   (remaining-matching-predicates
    :type (or list null)
    :initform nil
    :initarg :remaining-matching-predicates
    :accessor remaining-matching-predicates)
   (remaining-source-units
    :type (or list null)
    :initform nil
    :initarg :remaining-source-predicates
    :accessor remaining-source-predicates)
   (cost
    :initform 0
    :initarg :cost
    :accessor cost)))

(defun search-predicate-reordering-solutions (matching-predicates source-predicate-numbers pattern-length cost-params)
  "Given an a-list of numbers of matching units, the numbers of units in source, the number of units
   in the construction and cost params, returns a series of solution states
   This is implemented as a search process."
  (let ((solutions nil)
        (queue (list (make-instance 'reordering-predicates-search-state
                                    :solution-structure (loop for i from 1 upto pattern-length
                                                              collect
                                                              (list i))
                                    :remaining-matching-predicates matching-predicates
                                    :remaining-source-predicates source-predicate-numbers
                                    :cost 0))))
    (loop until (not queue)
          do
          (let ((current-state (pop queue)))
            (if (reordering-solution-p (solution-structure current-state))
              ;; If current-state is solution: add to solutions
              (unless (duplicate-search-state current-state solutions)
                (push current-state solutions))
              ;; Else: see wheter there are still units in solution structure which had matches
              (let ((non-matched-predicates (loop for unit in (solution-structure current-state)
                                             when (and (not (cdr unit)) (find (first unit) (remaining-matching-predicates current-state) :key 'first))
                                             collect (first unit))))
                (if non-matched-predicates
                  ;; If there are some, add their expansions as states to the queue
                  (dolist (nmu non-matched-predicates)
                    (let ((possible-matches (loop for mu in (remaining-matching-predicates current-state)
                                                  when (eq (car mu) nmu)
                                                  collect (second mu))))

                      (dolist (pm possible-matches)
                        (let ((new-search-state (make-instance 'reordering-predicates-search-state
                                                               :solution-structure (substitute (cons nmu pm) nmu (solution-structure current-state) :key 'first)
                                                               :remaining-matching-predicates (remove pm (remaining-matching-predicates current-state) :key 'cdr)
                                                               :remaining-source-predicates (remove pm (remaining-source-predicates current-state))
                                                               :cost (cost current-state))))
                          (unless (duplicate-search-state new-search-state queue)
                            (push new-search-state queue))))))
                  ;; Else: assign other non-matching units to these
                  (let ((non-matched-predicate (loop for unit in (solution-structure current-state)
                                                unless (cdr unit)
                                                return (first unit))))
                    (dolist (su (remaining-source-predicates current-state))
                      (push
                       (make-instance 'reordering-predicates-search-state
                                      :solution-structure (substitute (cons non-matched-predicate su) non-matched-predicate (solution-structure current-state) :key 'first)
                                      :remaining-matching-predicates '()
                                      :remaining-source-predicates (remove su (remaining-source-predicates current-state))
                                      :cost (if (equalp su 'remove-predicate)
                                              (+ (cost current-state) (get-anti-unification-cost 'removed-pattern-predicate cost-params nil nil))
                                              (+ (cost current-state) (get-anti-unification-cost 'non-matching-predicate cost-params nil nil))))
                       queue))))))))
    solutions))



(defun match-predicates (pattern source &optional (bindings +no-bindings+) &key cxn-inventory)
  (let ((result (unify-predicates pattern source (list bindings) :cxn-inventory cxn-inventory)))
    (notify matching-finished pattern source result)
    result))

(defun unify-predicate-contents (u1 u2 bsl &key cxn-inventory)
  (setq bsl (unify (first u1) (first u2) bsl :cxn-inventory cxn-inventory))
  (unless (fail? bsl)
    (unify (rest u1)
         (rest u2)
         bsl
         :cxn-inventory cxn-inventory)))

(defun unify-predicates (pattern source bsl &key cxn-inventory)
  (when (<= (length pattern) (length source))
    (cond ((null pattern) bsl)
	  ((<= (length pattern) (length source))
	   (subset-p pattern source bsl :unify-fn #'(lambda (u1 u2 bsl &key cxn-inventory)
						      (unify-predicate-contents u1 u2 bsl :cxn-inventory cxn-inventory))
                     :cxn-inventory cxn-inventory))
	  (t +fail+))))


#|   (pattern . source)
 ; ((1 . 1) (2 . 2) (3 . 7) (4 . 7) (5 . 4) (6 . FCG::REMOVE-PREDICATE) (7 . 6) (8 . 8))
 (anti-unify '((query ?target-4 ?target-object-1 ?attribute-2)  ;  1 what is the shape of the large thing
               (unique ?target-object-1 ?target-2)  ;  2
               (filter ?target-2 ?target-1 ?size-4) ;  3 
               (filter ?target-1 ?source-1 ?shape-8) ; 4
               (get-context ?source-1)               ; 5
               (bind attribute-category ?attribute-2 shape) ; 6
               (bind size-category ?size-4 large)    ; 7
               (bind shape-category ?shape-8 thing)) ; 8
             '((query ?target-5 ?target-object-2 ?attribute-3); 1 what is the color of the large thing
               (unique ?target-object-2 ?target-3)   ; 2
               (filter ?target-2 ?source-2 ?shape-9) ; 3
               (get-context ?source-2)               ; 4
               (bind attribute-category ?attribute-3 color) ; 5
               (bind size-category ?size-5 large)    ; 6
               (filter ?target-3 ?target-2 ?size-5)  ; 7
               (bind shape-category ?shape-9 thing)) ; 8
             :graphs
             +no-bindings+
             +no-bindings+
             :cost-params '((equality 0) ;; don't punish when source is equal to pattern 
                            (non-matching-predicate 10) ;; Punish badly non-matching units
                            (subst-from-bindingslist 0)
                            (source-variable-pattern-in-bindingslist 1)
                            (replace-by-new-var depth-of-replaced-pattern 1)
                            (discarded-feature 5)
                            (discarded-negated-feature 4)
                            (removed-pattern-predicate 20)))


 (query ?target-4 ?target-object-1 ?attribute-2) (query ?target-5 ?target-object-2 ?attribute-3)
 (unique ?target-object-1 ?target-2) (unique ?target-object-2 ?target-3)
 (filter ?target-2 ?target-1 ?size-4) (filter ?target-3 ?target-2 ?size-5)
 (filter ?target-1 ?source-1 ?shape-8) (filter ?target-3 ?target-2 ?size-5)
 (get-context ?source-1) (get-context ?source-2)
 (bind attribute-category ?attribute-2 shape) REMOVED
 (bind size-category ?size-4 large) (bind size-category ?size-5 large)
 (bind shape-category ?shape-8 thing) (bind shape-category ?shape-9 thing)

|#
 