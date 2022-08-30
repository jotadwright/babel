(in-package :pn)

;; ############################################################################
;; A first step in the anti-unification of predicate networks is 'alignment'.
;; The predicate networks are made up of sets of predicates (free order).
;; The alignment process will pair up predicates from the source network
;; and the pattern network.
;;
;; This is implemented as a search process that returns all possible solutions.
;; We loop over all predicates in the pattern network and find which predicates
;; from the source network match them. To determine this, the name of the
;; predicate, the arity and how the network is connected is taken into account. 
;; ----------------------------------------------------------------------------

;; why is it in FCG that only ONE unit is allowed to be removed from the pattern?
;; ==> because it drastically reduces the number of possible combinations...
;; 20/07/22: for the moment, multiple predicates are allowed to be removed
;; 28/07/22: now, no predicate are allowed to be removed! Because pattern and
;;           source can differ in length (e.g. addition and deletion repairs)
;;           the non-matching predicates should not be removed, since anti-
;;           unification needs them. For now, they are appended to the end...


(defun reorder-source-network (pattern source cost-params)
  "Reorder the source network `source' to align with
   the pattern network `pattern' according to the reordering
   costs `cost-params'. This function returns a list of subsets
   of source with (i) as many predicates as in pattern and
   (ii) aligned order (matching predicates in the same place
   and all combinations of other units) and (iii) the predicates
   from pattern that should be removed (if any)."
  (let ((reordering-solutions (find-matching-predicates pattern source cost-params))
        (list-of-reordered-sources nil))
    (loop for solution in reordering-solutions 
          for reordered-source = (loop for n from 1 upto (length pattern) collect n)
          for pattern-predicate-numbers-to-remove = nil
          for removed-pattern-predicates = nil
          do (loop for (pattern-predicate-number . assignment) in (pattern-assignments solution)
                   if (eql assignment 'remove-predicate)
                   do (push pattern-predicate-number pattern-predicate-numbers-to-remove)
                   else
                   do (let* ((pattern-index (- pattern-predicate-number 1))
                             (source-index (- assignment 1))
                             (source-predicate (nth source-index source)))
                        (setf (nth pattern-index reordered-source) source-predicate)))
           do (loop for number-to-remove in pattern-predicate-numbers-to-remove
                    do (push (nth (- number-to-remove 1) pattern) removed-pattern-predicates)
                    do (setf reordered-source (remove number-to-remove reordered-source)))
           do (when (remaining-source-predicates solution)
                (loop for remaining-source-number in (remaining-source-predicates solution)
                      do (pushend (nth (- remaining-source-number 1) source)
                                  reordered-source)))
           do (push (list reordered-source (cost solution) removed-pattern-predicates)
                    list-of-reordered-sources))
    ;; return the possible reorderings by cost (cheapest first)
    (sort list-of-reordered-sources #'< :key #'second)))
              
          
(defun find-matching-predicates (pattern source cost-params)
  (let* ((source-network-numbers
          (loop for n from 1 upto (length source) collect n))
         ;; determine candidate assignments based on
         ;; predicate name, arity, constants and variables
         (candidate-assignments-alist
          (loop for pattern-predicate in pattern
                for i from 1 
                for matches-for-this-predicate
                  = (loop for source-predicate in source
                          for j from 1 
                          when (matching-predicates pattern-predicate source-predicate)
                            collect j)
                collect (cons i matches-for-this-predicate)))
         ;; keep a list of the pattern predicates
         ;; that do not have any assignments
         (non-assigned-pattern-predicates
          (loop for (p . matches) in candidate-assignments-alist
                when (null matches) collect p))
         ;; generate all reordering solutions
         (reordering-solutions
          (search-reordering-solutions candidate-assignments-alist
                                       source-network-numbers
                                       (length pattern)
                                       cost-params))
         ;; check which reordering solutions can unify
         ;; and do not take into account predicates which had
         ;; no candidate assignment
         (embeddable-assignments
          (loop for solution in reordering-solutions
                for (pattern-subnet source-subnet)
                = (multiple-value-list
                   (loop for (pn . sn) in (pattern-assignments solution)
                         unless (or (find pn non-assigned-pattern-predicates)
                                    (eql sn 'remove-predicate))
                           collect (nth (- pn 1) pattern) into pattern-net
                           and collect (nth (- sn 1) source) into source-net
                         finally (return (values pattern-net source-net))))
                when (ordered-embedding source-subnet pattern-subnet)
                collect solution)))
    embeddable-assignments))


(defgeneric ordered-embedding (sub-program super-program &optional frame)
  (:documentation "Check if sub-net is an embedding of super-net,
   taking the order of sub-net and super-net into account!"))

(defmethod ordered-embedding ((sub-net null) (super-net list) 
                              &optional (frame (irl::make-map-frame)))
  (list frame))

(defmethod ordered-embedding ((sub-net list) (super-net list)
                              &optional (frame (irl::make-map-frame)))
  ;; try to unify the cars
  ;; if they do, unify the cdrs with the resulting bindings
  ;; continue until sub-net is empty
  (let ((new-frame (irl::find-map-function (car sub-net) (car super-net) frame)))
    (when new-frame
      (ordered-embedding (cdr sub-net) (cdr super-net) new-frame))))


(defun matching-predicates (predicate-1 predicate-2)
  "Returns t if predicate-1 and predicate-2 are equal in terms of constants."
  ;; easiest way to check if two predicates match
  ;; but the links in the network are NOT taken
  ;; into account...
  (when (= (length predicate-1) (length predicate-2))
    (loop for el-1 in predicate-1
          for el-2 in predicate-2
          always (or (equal el-1 el-2)
                     (and (variable-p el-1)
                          (variable-p el-2))))))


(defun search-reordering-solutions (candidate-assignments-alist source-network-numbers pattern-length cost-params)
  "Given an a-list of pattern predicates with matching source predicates,
   the numbers of predicates in the source network, the length of the
   pattern network and cost-params, return a series of solution states.
   This function implements a search process."
  (declare (ignorable cost-params)) ;; not used at the moment
  (let ((solutions nil)
        (queue
         (list
          (make-initial-reordering-state candidate-assignments-alist
                                         source-network-numbers
                                         pattern-length))))
    (loop while queue
          for current-state = (pop queue)
          if (reordering-solution-p current-state)
          ;; if the current state is a solution, add to solutions
          ;; if it is not a duplicate solution!
          do (unless (duplicate-state-p current-state solutions)
               (push current-state solutions))
          ;; else, continue searching
          else
          do (let ((non-matched-predicate-numbers
                    ;; find all predicates from pattern that have not
                    ;; been assigned a predicate from source AND still have
                    ;; possible candidate matches
                    (loop for (pattern-predicate-number . assignment) in (pattern-assignments current-state)
                          when (and (null assignment)
                                    (find pattern-predicate-number
                                          (candidate-assignments current-state)
                                          :key #'first :test #'=))
                          collect pattern-predicate-number)))
               (if non-matched-predicate-numbers
                 ;; if there are such predicates, expand the state
                 ;; by taking the first candidate for each non-matched predicate
                 (loop for non-matched-predicate-number in non-matched-predicate-numbers
                       for possible-match-numbers
                       = (assqv non-matched-predicate-number (candidate-assignments current-state) :test #'=)
                       ;= (loop for (predicate-number . candidates) in (candidate-assignments current-state)
                       ;        when (= predicate-number non-matched-predicate-number)
                       ;        collect (first candidates))
                       do (loop for match-number in possible-match-numbers
                                for new-state = (expand-reordering-state current-state
                                                                         non-matched-predicate-number
                                                                         match-number t)
                                unless (duplicate-state-p new-state queue)
                                do (push new-state queue)))
                 ;; else, try to assign each of the remaining source predicates
                 ;; to the first non-matched pattern predicate
                 (loop with non-matched-predicate-number
                         = (loop for (pattern-predicate-number . assignment) in (pattern-assignments current-state)
                                 when (null assignment) return pattern-predicate-number)
                       for source-predicate-number in (remaining-source-predicates current-state)
                       for new-state = (expand-reordering-state current-state
                                                                non-matched-predicate-number
                                                                source-predicate-number nil)
                       do (push new-state queue)))))
    (sort solutions #'< :key #'cost)))


(defclass reordering-search-state ()
  ((pattern-assignments
    :type (or null list) :initform nil
    :initarg :pattern-assignments
    :accessor pattern-assignments)
   (candidate-assignments
    :type (or null list) :initform nil
    :initarg :candidate-assignments
    :accessor candidate-assignments)
   (remaining-source-predicates
    :type (or null list) :initform nil
    :initarg :remaining-source-predicates
    :accessor remaining-source-predicates)
   (cost
    :type number :initform 0
    :initarg :cost
    :accessor cost))
  (:documentation "State in the reordering search space"))


(defun make-initial-reordering-state (candidate-assignments-alist source-network-numbers pattern-length)
  (make-instance 'reordering-search-state
                 :pattern-assignments (loop for i from 1 upto pattern-length collect (cons i nil))
                 :candidate-assignments candidate-assignments-alist
                 :remaining-source-predicates source-network-numbers
                 :cost 0))


(defun expand-reordering-state (state non-matched-predicate-number source-predicate-number matchp)
  (make-instance 'reordering-search-state
                 :pattern-assignments
                 ;; assign the source predicate number to the pattern predicate number
                 (substitute (cons non-matched-predicate-number source-predicate-number)
                             non-matched-predicate-number
                             (pattern-assignments state) :key #'car)
                 :candidate-assignments
                 ;; when source predicate and pattern predicate match
                 ;; remove source predicate from the candidate matches
                 ;; of all pattern predicates (it can no longer be assigned)
                 (when matchp
                   (loop for (predicate-number . matches) in (candidate-assignments state)
                         for remaining-matches = (remove source-predicate-number matches)
                         when remaining-matches
                         collect (cons predicate-number remaining-matches)))
                 :remaining-source-predicates
                 ;; source predicate has been assigned, remove it from the
                 ;; list of possible assignable source predicates,
                 ;; except when it is the 'remove-predicate symbol
                 ;; because it should be possible to remove multiple
                 ;; predicates when necessary.
                 (remove source-predicate-number (remaining-source-predicates state))
                 :cost
                 ;; when source and pattern predicate match, do not
                 ;; increase the cost. In all other cases, there is
                 ;; a cost (TBD).
                 (cond (matchp (cost state))
                       ((eql source-predicate-number 'remove-predicate)
                        (+ (cost state) 20))
                       (t (+ (cost state) 10)))))


(defun reordering-solution-p (state)
  "The state is a solution if all elements of the pattern
   have been assigned a matching predicate from source. This
   is the case when the solution structure consists of a list
   of cons-cells (i.e. not a list of singletons)."
  (loop for (nil . assignment) in (pattern-assignments state)
        always assignment))


(defun duplicate-state-p (state queue)
  "Returns T when the given state is a duplicate to any state
   in the queue. This is the case when there pattern
   assignments are equal"
  (let ((duplicatep nil))
    (loop for qstate in queue
          when (equal (pattern-assignments state)
                      (pattern-assignments qstate))
          do (setf duplicatep t) (return))
    duplicatep))

        

    
