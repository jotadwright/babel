;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Exploring more efficient and more correct ways of detecting duplicate nodes...  ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loading FCG and its demo grammar, and saving examples of duplicate nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Quickload the FCG system
; (ql:quickload :fcg)

;; Set *package*
(in-package :fcg)

#|

;; Load demo grammar, activate-monitor an create search tree with duplicates
(progn
  (load-demo-grammar)
  (activate-monitor trace-fcg)
  (formulate-all (instantiate-variables (comprehend "the linguist likes the mouse"))))

;; Pick a node marked as a duplicate, hover over it, click the icon to save the cipn, then:
(setf *node-1* *saved-cipn*)

;; Do the same with the node of which *node-1* is a duplicate, then:
(setf *node-2* *saved-cipn*)

;; Extract the transient structures of the two nodes:
(setf *ts-1* (left-pole-structure (car-resulting-cfs (cipn-car *node-1*))))
(setf *ts-2* (left-pole-structure (car-resulting-cfs (cipn-car *node-2*))))

|#
;; Visualise them:
; (format nil "~a" *ts-1*)

(setf *ts-1*
'((THE-WORD-48 (SYN-CAT ((LEX-CLASS ARTICLE)))
               (ARGS (X-9))
               (SEM-CAT ((SEM-CLASS REFERENT)))
               (MEANING ((UNIQUE X-9)))
               (FORM ((STRING THE-WORD-48 the)))
               (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  (ROOT (SYN-CAT NIL)
        (SEM-CAT NIL)
        (REFERENT))
  (LINGUIST-WORD-15 (SYN-CAT ((LEX-CLASS NOUN)))
                    (ARGS (X-9))
                    (SEM-CAT ((SEM-CLASS PHYSICAL-ENTITY)))
                    (MEANING ((LINGUIST X-9)))
                    (FORM ((STRING LINGUIST-WORD-15 linguist)))
                    (DEPENDENTS (THE-WORD-48))
                    (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  (NOUN-PHRASE-27 (FORM ((MEETS THE-WORD-48 LINGUIST-WORD-15)))
                  (SEM-CAT ((SEM-CLASS REFERRING-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS NOUN-PHRASE)))
                  (ARGS (X-9))
                  (CONSTITUENTS (THE-WORD-48 LINGUIST-WORD-15)))
  (THE-WORD-51 (SYN-CAT ((LEX-CLASS ARTICLE)))
               (ARGS (Y-6))
               (SEM-CAT ((SEM-CLASS REFERENT)))
               (MEANING ((UNIQUE Y-6)))
               (FORM ((STRING THE-WORD-51 the)))
               (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  (MOUSE-WORD-23 (SYN-CAT ((LEX-CLASS NOUN)))
                 (ARGS (Y-6))
                 (SEM-CAT ((SEM-CLASS PHYSICAL-ENTITY)))
                 (MEANING ((MOUSE Y-6))) (FORM ((STRING MOUSE-WORD-23 mouse)))
                 (DEPENDENTS (THE-WORD-51))
                 (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  (NOUN-PHRASE-25 (FORM ((MEETS THE-WORD-51 MOUSE-WORD-23)))
                  (SEM-CAT ((SEM-CLASS REFERRING-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS NOUN-PHRASE)))
                  (ARGS (Y-6))
                  (CONSTITUENTS (THE-WORD-51 MOUSE-WORD-23)))
  (VERB-PHRASE-33 (SEM-CAT ((SEM-CLASS RELATIONAL-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS VERB-PHRASE) (TYPE TRANSITIVE)))
                  (ARGS (X-9 Y-6))
                  (CONSTITUENTS (LIKES-WORD-16)))
  (LIKES-WORD-16 (SYN-CAT ((TYPE TRANSITIVE) (LEX-CLASS VERB)))
                 (ARGS (X-9 Y-6))
                 (SEM-CAT ((SEM-CLASS RELATION)))
                 (MEANING ((DEEP-AFFECTION X-9 Y-6)))
                 (FORM ((STRING LIKES-WORD-16 likes)))
                 (FOOTPRINTS (SEM-VERB-PHRASE-CXN SYN-VERB-PHRASE-CXN)))))

;(format nil "~a" *ts-2*)

(setf *ts-2*
'((THE-WORD-51 (SYN-CAT ((LEX-CLASS ARTICLE)))
               (ARGS (Y-6)) (SEM-CAT ((SEM-CLASS REFERENT)))
               (MEANING ((UNIQUE Y-6)))
               (FORM ((STRING THE-WORD-51 the)))
               (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  (MOUSE-WORD-23 (SYN-CAT ((LEX-CLASS NOUN)))
                 (ARGS (Y-6))
                 (SEM-CAT ((SEM-CLASS PHYSICAL-ENTITY)))
                 (MEANING ((MOUSE Y-6)))
                 (FORM ((STRING MOUSE-WORD-23 mouse)))
                 (DEPENDENTS (THE-WORD-51))
                 (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  (NOUN-PHRASE-25 (FORM ((MEETS THE-WORD-51 MOUSE-WORD-23)))
                  (SEM-CAT ((SEM-CLASS REFERRING-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS NOUN-PHRASE)))
                  (ARGS (Y-6))
                  (CONSTITUENTS (THE-WORD-51 MOUSE-WORD-23)))
  (VERB-PHRASE-31 (SEM-CAT ((SEM-CLASS RELATIONAL-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS VERB-PHRASE) (TYPE TRANSITIVE)))
                  (ARGS (X-9 Y-6))
                  (CONSTITUENTS (LIKES-UNIT-7)))
  (LIKES-UNIT-7 (SYN-CAT ((TYPE TRANSITIVE) (LEX-CLASS VERB)))
                 (ARGS (X-9 Y-6))
                 (SEM-CAT ((SEM-CLASS RELATION)))
                 (MEANING ((DEEP-AFFECTION X-9 Y-6)))
                 (FORM ((STRING LIKES-UNIT-7 likes)))
                 (FOOTPRINTS (SEM-VERB-PHRASE-CXN SYN-VERB-PHRASE-CXN)))
  (NOUN-PHRASE-26 (FORM ((MEETS THE-WORD-48 LINGUIST-WORD-16)))
                  (SEM-CAT ((SEM-CLASS REFERRING-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS NOUN-PHRASE)))
                  (ARGS (X-9))
                  (CONSTITUENTS (THE-WORD-48 LINGUIST-WORD-16)))
  (LINGUIST-WORD-16 (SYN-CAT ((LEX-CLASS NOUN)))
                    (ARGS (X-9))
                    (SEM-CAT ((SEM-CLASS PHYSICAL-ENTITY)))
                    (MEANING ((LINGUIST X-9))) (FORM ((STRING LINGUIST-WORD-16 linguist)))
                    (DEPENDENTS (THE-WORD-48))
                    (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  (ROOT (SYN-CAT NIL)
        (SEM-CAT NIL)
        (REFERENT))
  (THE-WORD-48 (SYN-CAT ((LEX-CLASS ARTICLE)))
               (ARGS (X-9))
               (SEM-CAT ((SEM-CLASS REFERENT)))
               (MEANING ((UNIQUE X-9)))
               (FORM ((STRING THE-WORD-48 the)))
               (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))))


;; A slightly improved version of the the current algorithm ;;
;; Same complexity but at least twice as fast               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equivalent-transient-structures (ts-1 ts-2)
  "TS-2 is fully instantiated, and the unit names of TS-1 are variablified.
The ==P special operator is add everywhere (which is wrong (!!) but done in the original code).
Then both TS's are unified..."
  (let* ((ts-1-with-variablified-unit-names (sublis (create-vars (mapcar #'unit-name ts-1)) ts-1))
         (ts-2-with-instantiated-variables (instantiate-expression ts-2)))
    (unify-structures (transform-structure ts-1-with-variablified-unit-names)
                      ts-2-with-instantiated-variables
                      (list +no-bindings+))))







  


;; Some tests ;;
;;;;;;;;;;;;;;;;
#|

(pprint (canonise-transient-structure *ts-1*))

(canonise-transient-structure
 '((THE-WORD-48 (SYN-CAT ((LEX-CLASS ((NOUN -)
                                      (ARTICLE +)))))
                (ARGS (X-9 ?x Y-10))
                (SEM-CAT ((SEM-CLASS REFERENT)))
                (MEANING ((UNIQUE X-9)))
                (FORM ((STRING THE-WORD-48 the)))
                (FOOTPRINTS ( SYN-NOUN-PHRASE-CXN ?test SEM-NOUN-PHRASE-CXN)))
   (test-x (SYN-CAT ((LEX-CLASS ((NOUN -)
                                 (ARTICLE +)))))
           (ARGS (X-9  Y-10 ?y))
           (SEM-CAT ((SEM-CLASS REFERENT)))
           (MEANING ((UNIQUE X-9)))
           (FORM ((STRING THE-WORD-48 the)))
           (FOOTPRINTS (SYN-NOUN-PHRASE-CXN ?bla SEM-NOUN-PHRASE-CXN))))
 :cxn-inventory *fcg-constructions*)


(canonise-transient-structure
 '((THE-WORD-48 (SYN-CAT ((LEX-CLASS ((NOUN -)
                                      (ARTICLE +)))))
                (ARGS (X-9 Y-10))
                (SEM-CAT ((SEM-CLASS REFERENT)))
                (MEANING ((UNIQUE X-9)))
                (FORM ((STRING THE-WORD-48 the)))
                (FOOTPRINTS ( SYN-NOUN-PHRASE-CXN ?test SEM-NOUN-PHRASE-CXN)))
   (test-x (SYN-CAT ((LEX-CLASS ((NOUN -)
                                 (ARTICLE +)))))
           (ARGS (X-9 Y-10))
           (SEM-CAT ((SEM-CLASS REFERENT)))
           (MEANING ((UNIQUE X-9)))
           (FORM ((STRING THE-WORD-48 the)))
           (FOOTPRINTS (SYN-NOUN-PHRASE-CXN ?bla SEM-NOUN-PHRASE-CXN))))
 :cxn-inventory *fcg-constructions*)






(equal


(canonise-transient-structure
 *ts-1*
)

(canonise-transient-structure
 *ts-2*
)
)



|#





;; Normalising a transient structure into its canonical form ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Multiple 'correct' canonisations might exist, in that case they should all be returned.

(defun canonise-transient-structure (ts &key (cxn-inventory *fcg-constructions*))
  "Normalises a transient structure into its canonical form(s)."
  (let* (;; First, let's variablify the unit names (except for the root), as these are not canoncial
         (ts-unit-renamings (create-vars (remove 'root (mapcar #'unit-name ts))))
         (ts-with-variablified-unit-names (sublis ts-unit-renamings ts))
         ;; Now, let's reorder all feature WITHIN the units and leave the order of the units themselves for later
         ;; We do not care about variables at this point, but there is ambiguity beyond variable names
         (ts-with-units-internally-reordered (loop for unit in ts-with-variablified-unit-names
                                                   collect (cons (unit-name unit)
                                                                 (reorder-set-of-feature-value-pairs (unit-body unit) :cxn-inventory cxn-inventory))))
         ;; Let's reorder the units now, in case of ambiguity, yield all solutions
         ;; Form now on, we will potentially have to deal with multiple versions of the transient structure
         (list-of-ts-with-units-reordered (sorted-permutations ts-with-units-internally-reordered :key #'hash-unit-ignoring-variables :sort-fn #'<))
         ;; Let's introduce canonical unit names - i.e. unit-1 upto unit-n
         (list-of-ts-with-units-reordered-and-renamed (mapcar #'rename-units-canonically list-of-ts-with-units-reordered))
         ;; the only challenge left are variables -
         ;; we can now rename canonically all those that do not (only) appear in sets or sets-of-predicates
         (list-of-ts-no-vars-except-sets (mapcar #'(lambda (ts) (rename-vars-canonically-except-sets ts cxn-inventory)) list-of-ts-with-units-reordered-and-renamed))
         ;;
         ;; we reorder the feature structure again
         (list-of-ts-no-vars-except-sets-reordered (mapcar #'(lambda (ts) (loop for unit in ts
                                                                                collect (cons (unit-name unit)
                                                                                              (reorder-set-of-feature-value-pairs
                                                                                               (unit-body unit) :cxn-inventory cxn-inventory))))
                                                           list-of-ts-no-vars-except-sets))
         ;; and those that appear only in sets, expanding again to all possibilities
         (list-of-ts-no-vars (mappend #'(lambda (ts) (rename-set-vars-canonically ts cxn-inventory)) list-of-ts-no-vars-except-sets)))
    list-of-ts-no-vars))



;; Reordering units  ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun reorder-feature-structure (feature-structure &key cxn-inventory)
  "Reorders a feature structure (either an atom or a feature-value pair) canonically."
  (cond ((atom feature-structure)
         feature-structure)
        ((eql 'set (feature-type feature-structure cxn-inventory))
         (list (feature-name feature-structure)
               (reorder-set (feature-value feature-structure))))
        ((eql 'set-of-predicates (feature-type feature-structure cxn-inventory))
         (list (feature-name feature-structure)
               (reorder-set-of-predicates (feature-value feature-structure))))
        ((eql 'sequence (feature-type feature-structure cxn-inventory))
         (list (feature-name feature-structure)
               (reorder-sequence (feature-value feature-structure))))
        ((eql 'sequence-of-predicates (feature-type feature-structure cxn-inventory))
         (list (feature-name feature-structure)
               (reorder-sequence-of-predicates (feature-value feature-structure))))
        (t ;; For feature-value pairs (default feature type)
           (list (feature-name feature-structure)
                 (if (atom (feature-value feature-structure))
                   (feature-value feature-structure)
                   (reorder-set-of-feature-value-pairs (feature-value feature-structure) :cxn-inventory cxn-inventory))))))

(defun reorder-set-of-feature-value-pairs (set-of-feature-value-pairs &key cxn-inventory)
  "Reorders set of feature-value pairs based on feature-name (with deep reordering of feature-values)."
  (loop for feature-value-pair in set-of-feature-value-pairs
        collect (reorder-feature-structure feature-value-pair :cxn-inventory cxn-inventory)
          into reordered-feature-value-pairs
        finally (return (sort reordered-feature-value-pairs #'(lambda (a-1 a-2) (< (sxhash a-1)
                                                                                   (sxhash a-2)))
                              :key #'feature-name))))
        
(defun reorder-sequence (sequence &key cxn-inventory)
  "Order in sequences is maintained, but the elements themselves or internally reordered."
  (loop for el in sequence
        collect (reorder-feature-structure el :cxn-inventory cxn-inventory)))

(defun reorder-sequence-of-predicates (sequence-of-predicates &key (cxn-inventory *fcg-constructions*))
  "Sequences of predicates are maintained as such."
  (declare (ignore cxn-inventory))
  sequence-of-predicates)

(defun reorder-set (set &key cxn-inventory)
  "All constants in the set are reordered, the variables are added at the end."
  (declare (ignore cxn-inventory))
  (let ((constants (remove-if #'variable-p set))
        (variables (remove-if-not #'variable-p set)))
    (append (sort constants #'(lambda (a-1 a-2) (< (sxhash a-1)
                                                   (sxhash a-2))))
            variables)))

(defun reorder-set-of-predicates (set-of-predicates &key cxn-inventory)
  "Reorder the set of predicates based on all symbols ignoring variables."
  (declare (ignore cxn-inventory))
  (sort set-of-predicates #'(lambda (p-1 p-2) (< (sxhash (remove-if #'variable-p p-1))
                                                 (sxhash (remove-if #'variable-p p-2))))))


(defun hash-unit-ignoring-variables (unit)
  "Creates a hash code for a unit, replacing all variables by nil."
  (let* ((all-unique-vars (remove-duplicates (find-all-anywhere-if #'variable-p unit)))
         (renamings (loop for var in all-unique-vars collect (cons var nil))))
    (sxhash (sublis renamings unit))))


(defun rename-units-canonically (list-of-units)
  "Replace unit names by canonical names (unit-1 to unit-n), anywhere in the list-of-units"
  (let ((canonical-renamings (loop for unit in list-of-units
                                   for i from 1
                                   unless (eq 'root (unit-name unit))
                                     collect (cons (unit-name unit) (intern (format nil "CHECK-DUPLICATE-UNIT-~a" i))))))
    (sublis canonical-renamings list-of-units)))



;; Renaming variables canonically ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rename-vars-canonically-except-sets (list-of-units cxn-inventory)
  (loop with renaming-counter = 1
        with renamings = nil
        for unit in list-of-units
        do  (loop for feature in (unit-body unit)
                  for renamings-and-counter = (multiple-value-list (collect-renamings-except-sets feature renamings renaming-counter cxn-inventory))
                  do (setf renamings (first renamings-and-counter))
                     (setf renaming-counter (second renamings-and-counter)))
        finally (return (sublis renamings list-of-units))))

(defun collect-renamings-except-sets (feature renamings renaming-counter cxn-inventory)
  (cond (;; constants - no action required
         (and (atom feature)
              (not (variable-p feature)))
         (values renamings renaming-counter))
        (;; variables - if no renaming exists yet, add renaming and increase counter
         (and (atom feature)
              (variable-p feature))
         (if (assoc feature renamings)
           (values renamings renaming-counter)
           (values (cons (cons feature (intern (format nil "CHECK-DUPLICATE-VAR-~a" renaming-counter))) renamings)
                   (+ 1 renaming-counter))))
        (;; set - do nothing at this point
         (eql 'set (feature-type feature cxn-inventory))
         (values renamings renaming-counter))
        (;; set-of-predicats -  do nothing at this point
         (eql 'set-of-predicates (feature-type feature cxn-inventory))
         (values renamings renaming-counter))
        (;; sequence - rename each element
         (eql 'sequence (feature-type feature cxn-inventory))
         (loop with new-renamings = renamings
               with new-counter = renaming-counter
               for el in (feature-value feature)
               do (setf (values new-renamings new-counter) (collect-renamings-except-sets el new-renamings new-counter cxn-inventory))
               finally (return (values new-renamings new-counter))))
        (;; sequence-of-predicates - rename each element
         (eql 'sequence-of-predicates (feature-type feature cxn-inventory))
         (loop with new-renamings = renamings
               with new-counter = renaming-counter
               for el in (feature-value feature)
               do (setf (values new-renamings new-counter) (collect-renamings-except-sets el new-renamings new-counter cxn-inventory))
               finally (return (values new-renamings new-counter))))
        (t ;; For feature-value pairs (default feature type) - renaming name each element in body
           (if (atom (feature-value feature))
             (collect-renamings-except-sets (feature-value feature) renamings renaming-counter cxn-inventory)
             (loop with new-renamings = renamings
                   with new-counter = renaming-counter
                   for el in (feature-value feature)
                   do (setf (values new-renamings new-counter) (collect-renamings-except-sets el new-renamings new-counter cxn-inventory))
                   finally (return (values new-renamings new-counter)))))))


(defun rename-set-vars-canonically (list-of-units cxn-inventory)
  (loop with renaming-counter = 1
        with renamings = nil
        for unit in list-of-units
        do  (loop for feature in (unit-body unit)
                  for renamings-and-counter = (multiple-value-list (collect-set-renamings feature renamings renaming-counter cxn-inventory))
                  do (setf renamings (first renamings-and-counter))
                     (setf renaming-counter (second renamings-and-counter)))
        finally (return (sublis renamings list-of-units))))

(defun collect-set-renamings (feature renamings renaming-counter cxn-inventory)
  (cond (;; constants - no action required
         (and (atom feature)
              (not (variable-p feature)))
         (values renamings renaming-counter))
        (;; variables - if no renaming exists yet, add renaming and increase counter
         (and (atom feature)
              (variable-p feature))
         (if (assoc feature renamings)
           (values renamings renaming-counter)
           (values (cons (cons feature (intern (format nil "CHECK-DUPLICATE-VAR-~a" renaming-counter))) renamings)
                   (+ 1 renaming-counter))))
        (;; set - do nothing at this point
         (eql 'set (feature-type feature cxn-inventory))
         (values renamings renaming-counter))
        (;; set-of-predicates -  do nothing at this point
         (eql 'set-of-predicates (feature-type feature cxn-inventory))
         (values renamings renaming-counter))
        (;; sequence - no action required
         (eql 'sequence (feature-type feature cxn-inventory))
         (values renamings renaming-counter))
        (;; sequence-of-predicates - no action required
         (eql 'sequence-of-predicates (feature-type feature cxn-inventory))
         (values renamings renaming-counter))
        (t ;; For feature-value pairs (default feature type) - renaming name each element in body
           (if (atom (feature-value feature))
             (collect-renamings-except-sets (feature-value feature) renamings renaming-counter cxn-inventory)
             (loop with new-renamings = renamings
                   with new-counter = renaming-counter
                   for el in (feature-value feature)
                   do (setf (values new-renamings new-counter) (collect-renamings-except-sets el new-renamings new-counter cxn-inventory))
                   finally (return (values new-renamings new-counter)))))))



;; Helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun sorted-permutations (list &key (key #'identity) (sort-fn #'<) (destructive t))
  "Returns all permutations of list that satisfy the sorting criterion specified by predicate.
   list: the input list to be sorted
   key:  a one-argument function that given an element of the lsit returns the value that will be past to sort-fn
   sort-fn: a two-argument function used for sorting
   destructive: a boolean indicating whether list can be destroyed as a side-effect
   example input: (sorted-permutations '((a . 1) (d . 3) (b . 1) (f . 4) (c . 2)  (e . 4)) :key #'cdr :sort-fn #'<)
   example output: '(((A . 1) (B . 1) (C . 2) (D . 3) (F . 4) (E . 4))
                     ((A . 1) (B . 1) (C . 2) (D . 3) (E . 4) (F . 4))
                     ((B . 1) (A . 1) (C . 2) (D . 3) (F . 4) (E . 4))
                     ((B . 1) (A . 1) (C . 2) (D . 3) (E . 4) (F . 4)))
"  (let* (;; If the original list needs to be kept intact, copy it first.
          (destructible-list (if destructive list (copy-seq list)))
          ;; Now we sort the list given key and sort-fn
          (sorted-list (sort destructible-list sort-fn :key key))
          ;; In the sorted list, we group elements according to key
          (sorted-groups (loop with result = nil and queue = nil
                               for (el next-el) on sorted-list
                               if ;; If el is not the last element in the list, and has no sorting order with respect to next-el
                                 (and next-el ;; it's not the last element in the list
                                      (not (funcall sort-fn (funcall key el) (funcall key next-el))) 
                                      (not (funcall sort-fn (funcall key next-el) (funcall key el))))
                                 do (setf queue (append queue (list el))) ;; then add it to the queue
                               else ;; there is no next element, or the next one has a different key
                                 do (setf result (append result (list (append queue (list el))))) ;; add (queue which can be empty) + unit to result
                                    (setf queue nil)
                               finally (return result))))
     (list-of-sets-to-all-possible-sequences sorted-groups)))


(defun list-of-sets-to-all-possible-sequences (list-of-sets)
  "Takes as input a sorted list of sets, and returns all sequences."
  ;; We start with (nil) as solutions-under-construction.
  ;; Then, we proceed set by set. Each new set provides (fac! (length set)) continuations
  ;; for each existing solutions-under-construction, i.e. each permutation of set.
  ;; We therefore return all (reduce #'* (mapcar #'(lambda (set)  (fac! (length set))) list-of-sets))
  ;; solutions.
  (loop with solutions-under-construction = (list nil)
        for set in list-of-sets
        for permutations-of-set = (list-permutations set)
        do (setf solutions-under-construction (loop for solution-under-construction in solutions-under-construction
                                                    append (loop for permutation-of-set in permutations-of-set
                                                                 collect (append solution-under-construction permutation-of-set))))
        finally (return solutions-under-construction)))


(defun list-permutations (list)
  "Returns all permutations of list."
  ;; Take the first element of the list, and cons it to all permutations of the other elements of the list.
  ;; Base cases: return nil when list is nil, '((element)) when the list has only one element.
  (loop for element in list
        for other-elements = (remove element list)
        for permutations-other-elements = (list-permutations other-elements)
        append (if permutations-other-elements
                 (mapcar #'(lambda (permutation-other-elements)
                             (cons element permutation-other-elements))
                         permutations-other-elements)
                 (list (list element)))))


(defun feature-type (feature cxn-inventory)
  "Returns the feature type of a feature."
  (second (assoc (feature-name feature) (feature-types cxn-inventory))))




















(equal

(canonise-transient-structure *ts-1*)
(canonise-transient-structure *ts-2*)

)


(setf *ts-1*
'((THE-WORD-48 (SYN-CAT ((LEX-CLASS ARTICLE)))
               (ARGS (X-9))
               (SEM-CAT ((SEM-CLASS REFERENT)))
               (MEANING ((UNIQUE X-9)))
               (FORM ((STRING THE-WORD-48 the)))
               (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  
  
  (NOUN-PHRASE-27 (FORM ((MEETS THE-WORD-48 ?a)))
                  (SEM-CAT ((SEM-CLASS REFERRING-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS NOUN-PHRASE)))
                  (ARGS (X-9))
                  (CONSTITUENTS (THE-WORD-48 ?a)))
  (THE-WORD-51 (SYN-CAT ((LEX-CLASS ARTICLE)))
               (ARGS (Y-6))
               (SEM-CAT ((SEM-CLASS REFERENT)))
               (MEANING ((UNIQUE Y-6)))
               (FORM ((STRING THE-WORD-51 the)))
               (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  
  (NOUN-PHRASE-25 (FORM ((MEETS THE-WORD-51 ?b)))
                  (SEM-CAT ((SEM-CLASS REFERRING-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS NOUN-PHRASE)))
                  (ARGS (Y-6))
                  (CONSTITUENTS (THE-WORD-51 ?b)))
  
  ))

(format nil "~a" *ts-2*)

(setf *ts-2*
'((THE-WORD-51 (SYN-CAT ((LEX-CLASS ARTICLE)))
               (ARGS (Y-6)) (SEM-CAT ((SEM-CLASS REFERENT)))
               (MEANING ((UNIQUE Y-6)))
               (FORM ((STRING THE-WORD-51 the)))
               (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))
  
  (NOUN-PHRASE-25 (FORM ((MEETS THE-WORD-51 ?c)))
                  (SEM-CAT ((SEM-CLASS REFERRING-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS NOUN-PHRASE)))
                  (ARGS (Y-6))
                  (CONSTITUENTS (THE-WORD-51 ?c)))
  
  
  (NOUN-PHRASE-26 (FORM ((MEETS THE-WORD-48 ?d)))
                  (SEM-CAT ((SEM-CLASS REFERRING-EXPRESSION)))
                  (SYN-CAT ((LEX-CLASS NOUN-PHRASE)))
                  (ARGS (X-9))
                  (CONSTITUENTS (THE-WORD-48 ?d)))
  
  
  (THE-WORD-48 (SYN-CAT ((LEX-CLASS ARTICLE)))
               (ARGS (X-9))
               (SEM-CAT ((SEM-CLASS REFERENT)))
               (MEANING ((UNIQUE X-9)))
               (FORM ((STRING THE-WORD-48 the)))
               (FOOTPRINTS (SEM-NOUN-PHRASE-CXN SYN-NOUN-PHRASE-CXN)))))
