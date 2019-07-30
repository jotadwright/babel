(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method for anti-unifying FCG constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod anti-unify-origins-of-syntax (pattern source cxn-inventory (mode (eql :fcg-with-type-hierarchy))
                               &optional (pattern-bindings +no-bindings+)
                               (source-bindings +no-bindings+)
                               &key (cost-params nil))
  ""
  (assert (<= (length pattern) (+ 2 (length source))))
  ;; Get units from source to anti-unify pattern against (ordered)
  (let* ((cost-params (or cost-params '((equality 0) ;; don't punish when source is equal to pattern 
                                        (non-matching-unit 10) ;; Punish badly non-matching units
                                        (subst-from-bindingslist 0)
                                        (th-link 1)
                                        (source-variable-pattern-in-bindingslist 100)
                                        (replace-by-new-var depth-of-replaced-pattern 100)
                                        (discarded-feature 5)
                                        (discarded-negated-feature 4)
                                        (removed-pattern-unit 10))))
         (pattern-unit-reorderings (anti-unification-categorisation-pair-units pattern source cxn-inventory))
         (anti-unification-results nil))
    (dolist (p-u-r pattern-unit-reorderings)
      (let ((updated-source (if (= (length (first p-u-r)) (length source))
                              source (remove 'root source :key #'unit-name :test #'eql))))
        (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-th-links resulting-discarded-features a-u-cost)
            (anti-unify-fcg-th (first p-u-r) updated-source pattern-bindings source-bindings nil 'unit-level cost-params cxn-inventory)
          (let ((total-cost (+ (second p-u-r) a-u-cost)))
            (push (list resulting-pattern resulting-pattern-bindings resulting-source-bindings
                        resulting-th-links resulting-discarded-features total-cost) anti-unification-results)))))
    (sort anti-unification-results '< :key 'sixth)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main recursive anti-unification function for FCG patterns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-fcg-th (pattern source pattern-bindings source-bindings th-links level cost-params cxn-inventory)
  ;; Case: equality of pattern and source
  (cond
   ((equalp pattern source)
    (values pattern
            pattern-bindings
            source-bindings
            th-links
            '() ;; discarded features
            (get-anti-unification-cost 'equality cost-params pattern source)))
   ;; Pattern and source connected through type-hierarchy
   ((and (symbolp pattern) (symbolp source)
         (type-hierarchies::node-p (intern (symbol-name pattern) :type-hierarchies)
                                   (type-hierarchies:get-type-hierarchy cxn-inventory))
         (type-hierarchies::node-p (intern (symbol-name source) :type-hierarchies)
                                   (type-hierarchies:get-type-hierarchy cxn-inventory))
         (type-hierarchies::directed-path-p (intern (symbol-name source) :type-hierarchies)
                                            (intern (symbol-name pattern) :type-hierarchies)
                                            (type-hierarchies:get-type-hierarchy cxn-inventory))
        ; (< (type-hierarchies:link-weight (intern (symbol-name source) :type-hierarchies)
        ;                                    (intern (symbol-name pattern) :type-hierarchies)
        ;                                    (type-hierarchies:get-type-hierarchy cxn-inventory)) 1.0)
         )
    (values pattern
            pattern-bindings
            source-bindings
            th-links
            '() ;; discarded features
            (get-anti-unification-cost 'equality cost-params pattern source)))
   ;; Pattern and source connected through links in th-links
   ((and (symbolp pattern) (symbolp source) (connected-through-th-links pattern source th-links))
    (values pattern
            pattern-bindings
            source-bindings
            th-links
            '() ;; discarded features
            (get-anti-unification-cost 'equality cost-params pattern source)))
   ;; Substitution is already in bindingslist
   ((subs-lookup pattern-bindings source-bindings pattern source)
    (values (subs-lookup pattern-bindings source-bindings pattern source)
            pattern-bindings
            source-bindings
            th-links
            '()
            (get-anti-unification-cost 'subst-from-bindingslist cost-params pattern source)))
   ;; Case: unit level: unit-name can be different
   ((and (equalp level 'unit-level)
         (anti-unify-fcg-sequence-th pattern source '() pattern-bindings source-bindings th-links '() 'unit-level cost-params 0 cxn-inventory)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-th-links resulting-discarded-features resulting-cost)
        (anti-unify-fcg-sequence-th pattern source '() pattern-bindings source-bindings th-links '() 'unit-level cost-params 0 cxn-inventory)
      (values resulting-pattern
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-th-links
              resulting-discarded-features
              resulting-cost)))
   ;; Case: top-feature level (eg: syn-cat, sem-cat, args, subunits,...): no special operator, but still subset;; feature name should be exact
   ((and (equalp level 'top-feature-level)
         (anti-unify-fcg-set-th (rest pattern) (rest source) '() pattern-bindings source-bindings th-links '() 'top-feature-level cost-params 0 cxn-inventory)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern-1 resulting-pattern-bindings-1 resulting-source-bindings-1 resulting-th-links-1 resulting-discarded-features-1 resulting-cost-1)
        (anti-unify-fcg-th (first pattern) (first source) pattern-bindings source-bindings th-links nil cost-params cxn-inventory)
      (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-th-links resulting-discarded-features resulting-cost)
          (anti-unify-fcg-set-th (rest pattern) (rest source) '() resulting-pattern-bindings-1 resulting-source-bindings-1 resulting-th-links-1 '() 'top-feature-level cost-params resulting-cost-1 cxn-inventory)
        (values (append (list resulting-pattern-1) resulting-pattern)
                resulting-pattern-bindings
                resulting-source-bindings
                resulting-th-links
                (if resulting-discarded-features-1
                  (push resulting-discarded-features-1 resulting-discarded-features)
                  resulting-discarded-features)
                resulting-cost))))
   ;; Case: subset with special operator ==1 or ==1
   ((and (listp pattern)
         (or (equalp (first pattern) '==1)
             (equalp (first pattern) '==))
         (anti-unify-fcg-set-th (rest pattern) source '() pattern-bindings source-bindings th-links '() nil cost-params 0 cxn-inventory)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-th-links resulting-discarded-features resulting-cost)
        (anti-unify-fcg-set-th (rest pattern) source '() pattern-bindings source-bindings th-links '() nil cost-params 0 cxn-inventory)
      (values (append (list (first pattern)) resulting-pattern)
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-th-links
              resulting-discarded-features
              resulting-cost)))
   ;; Case: ==0
   ((and (listp pattern)
         (equalp (first pattern) '==0)
         (anti-unify-fcg-excludes-th (rest pattern) source '() pattern-bindings source-bindings th-links '() cost-params 0 cxn-inventory)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-th-links resulting-discarded-features resulting-cost)
        (anti-unify-fcg-excludes-th (rest pattern) source '() pattern-bindings source-bindings th-links '() cost-params 0 cxn-inventory)
      (values (append (list (first pattern)) resulting-pattern)
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-th-links
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
         (anti-unify-fcg-sequence-th pattern source '() pattern-bindings source-bindings th-links '() nil cost-params 0 cxn-inventory)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-th-links resulting-discarded-features resulting-cost)
        (anti-unify-fcg-sequence-th pattern source '() pattern-bindings source-bindings th-links '() nil cost-params 0 cxn-inventory)
      (values resulting-pattern
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-th-links
              resulting-discarded-features
              resulting-cost)))
   ;; Source is variable, pattern is already in bindingslist, then return its binding
   ((and (variable-p source)
         (assoc pattern pattern-bindings))
    (values (cdr (assoc pattern pattern-bindings))
            pattern-bindings
            source-bindings
            th-links
            nil
            (get-anti-unification-cost 'source-variable-pattern-in-bindingslist cost-params pattern source)))
   ;; Else case for two symbols: make new th-links
   ((and (not (variable-p pattern))
         (not (variable-p source))
         (symbolp pattern)
         (symbolp source))
    (values pattern
            pattern-bindings
            source-bindings
            (cons `(,source . ,pattern) th-links ) 
            '() ;; discarded features
            (get-anti-unification-cost 'th-link cost-params pattern source)))
   ;; Else-case: introduce new variable
   (t
    (let ((var (make-var (if (atom pattern)
                           pattern
                           nil))))
      (values var
              (extend-bindings pattern var pattern-bindings)
              (extend-bindings source var source-bindings)
              th-links
              nil
              (get-anti-unification-cost 'replace-by-new-var cost-params pattern source))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive helper function for anti-unifying sequences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-fcg-sequence-th (pattern
                                source
                                accumulator
                                pattern-bindings
                                source-bindings
                                th-links
                                discarded-features
                                level
                                cost-params
                                cost
                                cxn-inventory)
  "anti-unify the elements of a feature"
  (let ((new-level))
   (if (equalp level 'unit-level)
    (setf new-level 'top-feature-level)
    (setf new-level nil))
  (cond
   ;; Case: no elements anymore, return accumulator and bindings-lists
   ((and (null pattern) (null source))
    (values accumulator
            pattern-bindings
            source-bindings
            th-links
            discarded-features
            cost))
   ;; Case: still elements, anti-unify first and then rest, every time with new bindings
   (t
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-th-links resulting-discarded-features resulting-cost)
        (anti-unify-fcg-th (first pattern) (first source)  pattern-bindings source-bindings th-links new-level cost-params cxn-inventory)
      (anti-unify-fcg-sequence-th (rest pattern)
                               (rest source)
                               (pushend resulting-pattern accumulator)
                               resulting-pattern-bindings
                               resulting-source-bindings
                               resulting-th-links
                               (if resulting-discarded-features
                                 (append resulting-discarded-features discarded-features)
                                 discarded-features)
                               level
                               cost-params
                               (+ resulting-cost cost)
                               cxn-inventory))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive helper function for anti-unifying sets ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-fcg-set-th (pattern
                           source
                           accumulator
                           pattern-bindings
                           source-bindings
                           th-links
                           discarded-features
                           level
                           cost-params
                           cost
                           cxn-inventory)
  "anti-unify the elements of a feature"
  (let ((new-level))
    (when (equalp level 'top-feature-level)
      (setf new-level 'nil))
    (cond
     ;; Case: no elements in pattern anymore, return accumulator and bindings-lists
     ((null pattern)
      (values accumulator
              pattern-bindings
              source-bindings
              th-links
              discarded-features
              cost))
     ;; Case: first element of pattern has binding with some variable in bindingslist: return binding
     ((assoc (first pattern) pattern-bindings :test 'equalp)
      (anti-unify-fcg-set-th (rest pattern)
                          (remove (cdr (assoc
                                        (cdr (assoc (first pattern) pattern-bindings :test 'equalp))
                                        (reverse-bindings source-bindings) :test 'equalp)) source :test 'equalp)
                          (pushend (cdr (assoc (first pattern) pattern-bindings :test 'equalp)) accumulator)
                          pattern-bindings
                          source-bindings
                          th-links
                          discarded-features
                          level
                          cost-params
                          (get-anti-unification-cost 'subst-from-bindingslist cost-params pattern source)
                          cxn-inventory))
     ;; first of pattern is an atom that is findable in source: return it
     ((and
       (atom (first pattern))
       (find (first pattern) source :test 'equalp)) ;; TODO find through-type-hierarchy 
      (anti-unify-fcg-set-th (rest pattern) (remove pattern source :test 'equalp)
                          (pushend (first pattern) accumulator)
                          pattern-bindings
                          source-bindings
                          th-links
                          discarded-features
                          level
                          cost-params
                          cost
                          cxn-inventory))
     ;; Case: tag: continue with third
     ((and
       (listp (first pattern))
       (string= (first (first pattern)) "TAG")
       (find (feature-name (third (first pattern))) source :key 'car :test 'equalp))
      (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-th-links resulting-discarded-features resulting-cost)
          (anti-unify-fcg-th (third (first pattern))
                          (find (feature-name (third (first pattern))) source :key 'car :test 'equalp)
                          pattern-bindings
                          source-bindings
                          th-links
                          new-level
                          cost-params
                          cxn-inventory)
        (anti-unify-fcg-set-th (rest pattern)
                            (remove (find (feature-name (third (first pattern))) source :key 'car :test 'equalp) source)
                            (pushend (append (list (first (first pattern)) (second (first pattern))) (list resulting-pattern)) accumulator)
                            resulting-pattern-bindings
                            resulting-source-bindings
                            resulting-th-links
                            (if resulting-discarded-features
                              (append resulting-discarded-features discarded-features)
                              discarded-features)
                            level
                            cost-params
                            (+ cost resulting-cost)
                            cxn-inventory)))
     ;; first of pattern is list of which feature-name is findable in source: anti-unify it return it
     ((and
       (listp (first pattern))
       (find (feature-name (first pattern)) source :key 'car :test 'equalp))
      (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-th-links resulting-discarded-features resulting-cost)
          (anti-unify-fcg-th (first pattern)
                          (find (feature-name (first pattern)) source :key 'car :test 'equalp)
                          pattern-bindings
                          source-bindings
                          th-links
                          new-level
                          cost-params
                          cxn-inventory)
        (anti-unify-fcg-set-th (rest pattern)
                            (remove (find (feature-name (first pattern)) source :key 'car :test 'equalp) source)
                            (pushend resulting-pattern accumulator)
                            resulting-pattern-bindings
                            resulting-source-bindings
                            resulting-th-links
                            (if resulting-discarded-features
                              (append resulting-discarded-features discarded-features)
                              discarded-features)
                            level
                            cost-params
                            (+ cost resulting-cost)
                            cxn-inventory)))
     ;; Case top-level-feature: feature-name of first of pattern is not found in source but unifies with (feature-name nil), append feature to accumulator and continue processing
     ((and (equalp level 'top-feature-level)
           (unify (first pattern) `(,(feature-name (first pattern)) nil)))
      (anti-unify-fcg-set-th (rest pattern)
                          source
                          (pushend (first pattern) accumulator)
                          pattern-bindings
                          source-bindings
                          th-links
                          discarded-features
                          level
                          cost-params
                          cost
                          cxn-inventory))
     ;; Case: no matching feature-name: anti-unify-fcg-set rest + append pattern to descarded features
     (t
      (anti-unify-fcg-set-th (rest pattern)
                          source
                          accumulator
                          pattern-bindings
                          source-bindings
                          th-links
                          (push (first pattern) discarded-features)
                          level
                          cost-params
                          (+ cost (get-anti-unification-cost 'discarded-feature cost-params pattern source))
                          cxn-inventory)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive helper function for anti-unifying excluded (NOT or  ==0) featuers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-fcg-excludes-th (pattern
                                source
                                accumulator
                                pattern-bindings
                                source-bindings
                                th-links
                                discarded-features
                                cost-params
                                cost
                                cxn-inventory)
  "anti-unify the elements of a feature"
  (cond
   ;; Case: no elements in pattern anymore, return accumulator and bindings-lists
   ((null pattern)
    (values accumulator
            pattern-bindings
            source-bindings
            th-links
            discarded-features
            cost
            cxn-inventory))
   ;; first of pattern is an atom that is not findable in source: return it
   ((and
     (atom (first pattern))
     (not (find (first pattern) source :test 'string=)))
    (anti-unify-fcg-excludes-th (rest pattern) source
                             (pushend (first pattern) accumulator)
                             pattern-bindings
                             source-bindings
                             th-links
                             discarded-features
                             cost-params
                             cost
                             cxn-inventory))
   ;; first of pattern is list which is not unifiable in source: return it
   ((and
     (listp (first pattern))
     (not (find (first pattern) source :test 'unify))
     (anti-unify-fcg-excludes-th (rest pattern) source
                              (pushend (first pattern) accumulator)
                              pattern-bindings
                              source-bindings
                              th-links
                              discarded-features
                              cost-params
                              cost
                              cxn-inventory))
    (anti-unify-fcg-excludes-th (rest pattern) source
                             (pushend (first pattern) accumulator)
                             pattern-bindings
                             source-bindings
                             th-links
                             discarded-features
                             cost-params
                             cost
                             cxn-inventory))
   ;; Case: non matching feature-name: anti-unify-fcg-set rest + append pattern to discarded features
   (t
    (anti-unify-fcg-excludes-th (rest pattern)
                             source
                             accumulator
                             pattern-bindings
                             source-bindings
                             th-links
                             (push (first pattern) discarded-features)
                             cost-params
                             (+ cost (get-anti-unification-cost 'discarded-negated-feature cost-params pattern source))
                             cxn-inventory
                             ))))

(defun anti-unification-categorisation-pair-units (pattern-units source-units cxn-inventory)
  ;; Not interested in root in source, if not in pattern...
  (unless (find 'root pattern-units :key #'unit-name :test #'string=)
    (setf source-units (remove-root-unit source-units)))
  ;; If now, source is longer than pattern, return nil immediately (all source-units need to be covered)
  (when (> (length source-units) (length pattern-units))
    (return-from anti-unification-categorisation-pair-units nil))
  ;; Else, try to pair the units
  (let ((pairing-solutions (pair-units-all-combinations pattern-units source-units cxn-inventory))
        (list-of-reordered-patterns nil))
    (dolist (rs pairing-solutions)
      (let ((solution-structure (solution-structure rs))
            (reordered-pattern (loop for n from 1 to (length source-units)
                                    collect n)))
        ;; Loop over units
        (dolist (unit solution-structure)
          (let* ((source-index (- (car unit) 1))
                 (pattern-index (- (cdr unit) 1))
                 (pattern-feature (nth pattern-index pattern-units)))
            (setf (nth source-index reordered-pattern) pattern-feature)))
        (push (list reordered-pattern (cost rs) nil) list-of-reordered-patterns)))
    (sort list-of-reordered-patterns '< :key 'second)))


(defun pair-units-all-combinations (pattern-units source-units cxn-inventory)
  (let ((pattern-unit-numbers (loop for n from 1 upto (length pattern-units)
                                    collect n))
        (source-length (length source-units)))
    (search-reordering-solutions-categorisation '()
                                                pattern-unit-numbers
                                                source-length)))

(defun pair-units (pattern-units source-units cxn-inventory)
  "given pattern and source (lists of fcg-units), and cost-params return:
   - list of objects with all possibilities for binding units in pattern with source, maximizing number of matching
     units objects contain: solution-structure, cost and non-matched-units of source"
  (let ((pattern-unit-numbers (loop for n from 1 upto (length pattern-units)
                                    collect n))
        (all-matching-units nil)
        (source-length (length source-units))
        (pattern-length (length pattern-units)))
    (loop for source-unit in source-units
          for i from 1 upto source-length
          do
          (let ((matches-for-this-unit nil))
            (loop for pattern-unit in pattern-units
                  for j from 1 upto pattern-length
                  do
                  (when (match-structures (list pattern-unit) (list source-unit) +no-bindings+ :cxn-inventory cxn-inventory)
                    (push j matches-for-this-unit)))
            (push (cons i matches-for-this-unit) all-matching-units)))
    (search-reordering-solutions-categorisation all-matching-units
                                                pattern-unit-numbers
                                                source-length)))

(defun search-reordering-solutions-categorisation (matching-units pattern-unit-numbers source-length)
  "Given an a-list of numbers of matching units, the numbers of units in source, the number of units
   in the construction and cost params, returns a series of solution states
   This is implemented as a search process."
  (let ((solutions nil)
        (queue (list (make-instance 'reordering-search-state
                                    :solution-structure (loop for i from 1 upto source-length
                                                              collect
                                                              (list i))
                                    :remaining-matching-units matching-units
                                    :remaining-source-units pattern-unit-numbers
                                    :cost 0))))
    (loop until (not queue)
          do
          (let ((current-state (pop queue)))
            (if (reordering-solution-p (solution-structure current-state))
              ;; If current-state is solution: add to solutions
              (unless (duplicate-search-state current-state solutions)
                (push current-state solutions))
              ;; Else: see wheter there are still units in solution structure which had matches
              (let ((non-matched-units (loop for unit in (solution-structure current-state)
                                             when (and (not (cdr unit)) (find (first unit) (remaining-matching-units current-state) :key 'first))
                                             collect (first unit))))
                (if non-matched-units
                  ;; If there are some, add their expansions as states to the queue
                  (dolist (nmu non-matched-units)
                    (let ((possible-matches (loop for mu in (remaining-matching-units current-state)
                                                  when (eq (car mu) nmu)
                                                  collect (second mu))))

                      (dolist (pm possible-matches)
                        (let ((new-search-state (make-instance 'reordering-search-state
                                                               :solution-structure (substitute (cons nmu pm) nmu (solution-structure current-state) :key 'first)
                                                               :remaining-matching-units (remove pm (remaining-matching-units current-state) :key 'second)
                                                               :remaining-source-units (remove pm (remaining-source-units current-state))
                                                               :cost (cost current-state))))
                          (unless (duplicate-search-state new-search-state queue)
                            (push new-search-state queue))))))
                  ;; Else: assign other non-matching units to these
                  (let ((non-matched-unit (loop for unit in (solution-structure current-state)
                                                unless (cdr unit)
                                                return (first unit))))
                    (dolist (su (remaining-source-units current-state))
                      (push
                       (make-instance 'reordering-search-state
                                      :solution-structure (substitute (cons non-matched-unit su) non-matched-unit (solution-structure current-state) :key 'first)
                                      :remaining-matching-units '()
                                      :remaining-source-units (remove su (remaining-source-units current-state))
                                      :cost (if (equalp su 'remove-unit)
                                              (+ (cost current-state) 2)
                                              (+ (cost current-state) 1)))
                       queue))))))))
    solutions))


(defun connected-through-th-links (pattern source th-links)
  (loop for th-link in th-links
        when (and (string= source (car th-link))
                  (string= pattern (cdr th-link)))
        return t))

(defun depth-of-replaced-pattern (pattern source)
  (declare (ignore source))
  (if (variable-p pattern)
    1
    (+ 1 (depth pattern))))
