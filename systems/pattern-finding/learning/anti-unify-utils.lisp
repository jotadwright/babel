(in-package :pf)

;;;;;;;;;;;;;;;;;;;;;
;; anti-unify form ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric anti-unify-form (source-form thing &key max-au-cost)
  (:documentation "Anti-unify the observation with the given thing on the form side"))

(defmethod anti-unify-form (source-form thing &key max-au-cost)
  (let ((form-representation (get-configuration
                              (typecase thing
                                (fcg-construction (cxn-inventory thing))
                                (cip-node (construction-inventory thing)))
                              :form-representation-formalism)))
    (anti-unify-form-aux source-form thing form-representation :max-au-cost max-au-cost)))

(defmethod anti-unify-form-aux (source-form (cxn fcg-construction) (mode (eql :sequences)) &key max-au-cost)
  (multiple-value-bind (possible-pattern-forms variable-boundaries)
      (render-all (extract-form-predicates cxn) :render-sequences)
    (setf possible-pattern-forms
          (mapcar #'(lambda (lst)
                      (list-of-strings->string lst :separator ""))
                  possible-pattern-forms))
    (let* ((anti-unification-results
            (loop for pattern-form in possible-pattern-forms
                  append (anti-unify-strings pattern-form source-form)))
           (valid-anti-unification-results
            (remove-if-not #'valid-au-result-p anti-unification-results)))
      (when max-au-cost
        (setf valid-anti-unification-results
              (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                         valid-anti-unification-results)))
      (setf valid-anti-unification-results
            (loop for au-result in valid-anti-unification-results
                  collect (replace-variables-with-matching-boundaries au-result variable-boundaries)))
      (sort valid-anti-unification-results #'< :key #'fcg::cost))))

;; TO DO:
;; check for each variable in the generalisation if it occurs in the same position
;; as one of the variable boundaries in the pattern.
;; if so, replace that variable in the generalisation and the delta
(defun replace-variables-with-matching-boundaries ()
  )

#|
  (let* (;; render all possible forms for source
         (possible-source-forms
          (mapcar #'(lambda (lst) (list-of-strings->string lst :separator ""))
                  (render-all source-form :render-sequences)))
         ;; render all possible forms for pattern
         (cxn-form (extract-form-predicates cxn))
         (possible-pattern-forms
          (mapcar #'(lambda (lst) (list-of-strings->string lst :separator ""))
                  (render-all cxn-form :render-sequences)))
         ;; anti-unify all combinations
         (anti-unification-results
          (loop for (pattern-form source-form) in (combinations possible-source-forms possible-pattern-forms)
                append (fcg::anti-unify-strings pattern-form source-form)))
         ;; remove invalid anti-unification results
         (valid-anti-unification-results
          (remove-if-not #'valid-au-result-p anti-unification-results)))
    ;; remove anti-unification results with high cost
    (when max-au-cost
      (setf valid-anti-unification-results
            (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                       valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))
|#

(defmethod anti-unify-form-aux (source-form (cipn cip-node) (mode (eql :sequences)) &key max-au-cost)
  (let* (;; render all possible forms for source
         (possible-source-forms
          (mapcar #'(lambda (lst) (list-of-strings->string lst :separator ""))
                  (render-all source-form :render-sequences)))
         ;; render all possible forms for pattern
         (ts-form
          (loop for unit in (fcg-get-transient-unit-structure cipn)
                unless (eql (unit-name unit) 'fcg::root)
                append (unit-feature-value unit 'form)))
         (possible-pattern-forms
          (mapcar #'(lambda (lst) (list-of-strings->string lst :separator ""))
                  (render-all ts-form :render-sequences)))
          ;; anti-unify all combinations
          (anti-unification-results
          (loop for (pattern-form source-form) in (combinations possible-source-forms possible-pattern-forms)
                append (fcg::anti-unify-strings pattern-form source-form :to-sequence-predicates-p t)))
         ;; remove invalid anti-unification results
         (valid-anti-unification-results
          (remove-if-not #'valid-au-result-p anti-unification-results)))
    ;; remove anti-unification results with high cost
    (when max-au-cost
      (setf valid-anti-unification-results
            (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                       valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))        

(defmethod anti-unify-form-aux (source-form (cxn fcg-construction) (mode (eql :string+meets)) &key max-au-cost)
  ;; assign fresh variables to the pattern
  (multiple-value-bind (pattern-form renamings)
      (fresh-variables (extract-form-predicates cxn))
    (let* (;; anti-unify
           (anti-unification-results
            (anti-unify-predicate-network pattern-form source-form))
           ;; remove invalid anti-unification results
           (valid-anti-unification-results
            (remove-if-not #'valid-au-result-p anti-unification-results)))
      ;; restore original variables
      (setf valid-anti-unification-results
            (loop for au-result in valid-anti-unification-results
                  collect (rerename-pattern-variables au-result renamings)))
      ;; remove anti-unification results with high cost
      (when max-au-cost
        (setf valid-anti-unification-results
              (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                         valid-anti-unification-results)))
      (sort valid-anti-unification-results #'< :key #'fcg::cost))))

(defmethod anti-unify-form-aux (source-form (cipn cip-node) (mode (eql :string+meets)) &key max-au-cost)
  (let* (;; extract pattern form
         (pattern-form
          (variablify-form-constraints-with-constants
           (loop for unit in (fcg-get-transient-unit-structure cipn)
                 unless (eql (unit-name unit) 'fcg::root)
                 append (unit-feature-value unit 'form))))
         ;; anti-unify
         (anti-unification-results
          (anti-unify-predicate-network pattern-form source-form))
         ;; remove invalid anti-unification results
         (valid-anti-unification-results
          (remove-if-not #'valid-au-result-p anti-unification-results)))
    ;; remove anti-unification results with high cost
    (when max-au-cost
      (setf valid-anti-unification-results
            (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                       valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))


#|
(defgeneric add-form-arg-predicates (set-of-predicates thing)
  (:documentation "Extract args from thing and add them as predicates
                   to the set of predicates."))

(defmethod add-form-arg-predicates (set-of-predicates (cxn fcg-construction))
  ;; top-args = args from contributing unit
  (loop for unit in (contributing-part cxn)
        for form-args = (first (fcg-unit-feature-value unit 'form-args))
        for category = (extract-category-unit unit)
        when (and form-args category)
        do (loop for arg in form-args
                 do (push (list 'top-arg arg category) set-of-predicates)))
  ;; slot-args = args from units that represent slots
  (loop for unit in (extract-slot-units cxn)
        for form-args = (remove-duplicates (first (fcg-unit-feature-value unit 'form-args)))
        for category = (extract-category-unit unit)
        when (and form-args category)
        do (loop for arg in form-args
                 do (push (list 'slot-arg arg category) set-of-predicates)))
  set-of-predicates)

(defmethod add-form-arg-predicates (set-of-predicates (args blackboard))
  ;; not used at the moment!!
  ;; will be necessary for recursion...
  (loop for arg in (find-data args :top-lvl-form-args)
        do (push (list 'top-arg arg) set-of-predicates))
  (loop for arg in (find-data args :slot-form-args)
        do (push (list 'slot-arg arg) set-of-predicates))
  set-of-predicates)

(defmethod add-form-arg-predicates (set-of-predicates (cipn cip-node))
  ;; top-args = args from slot-units that do not have 'form' and 'meaning' feature
  (let* ((ts-units (fcg-get-transient-unit-structure cipn))
         (root-unit (get-root ts-units))
         (all-slot-units (get-child-units (remove root-unit ts-units)))
         (open-slot-units
          (loop for unit in all-slot-units
                unless (and (unit-feature unit 'form) (unit-feature unit 'meaning))
                collect unit)))
    (loop for unit in open-slot-units
          for form-args = (unit-feature-value unit 'form-args)
          for category = (extract-category-unit unit)
          when (and form-args category)
          do (loop for arg in form-args
                   do (push (list 'top-arg arg category) set-of-predicates))))
  ;; slots-args = args from all top level units
  (let* ((ts-units (fcg-get-transient-unit-structure cipn))
         (root-unit (get-root ts-units))
         (top-lvl-units (remove-child-units (remove root-unit ts-units))))
    (loop for unit in top-lvl-units
          for form-args = (unit-feature-value unit 'form-args)
          for category = (extract-category-unit unit)
          when (and form-args category)
          do (loop for arg in form-args
                   do (push (list 'slot-arg arg category) set-of-predicates))))
  ;; return set of predicates
  set-of-predicates)
|#

;;;;;;;;;;;;;;;;;;;;;;;;
;; anti-unify meaning ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric anti-unify-meaning (source-meaning thing &key max-au-cost)
  (:documentation "Anti-unify the observation with the given cxn on the meaning side."))

(defmethod anti-unify-meaning (source-meaning (cxn fcg-construction) &key max-au-cost)
  (multiple-value-bind (pattern-meaning renamings)
      (fresh-variables (extract-meaning-predicates cxn))
    (let* ((anti-unification-results
            (anti-unify-predicate-network pattern-meaning source-meaning))
           (valid-anti-unification-results
            (remove-if-not #'valid-au-result-p anti-unification-results)))
      (when max-au-cost
        (setf valid-anti-unification-results
              (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                         valid-anti-unification-results)))
      (setf valid-anti-unification-results
            (loop for au-result in valid-anti-unification-results
                  collect (rerename-pattern-variables au-result renamings)))
      (sort valid-anti-unification-results #'< :key #'fcg::cost))))

(defmethod anti-unify-meaning (source-meaning (cipn cip-node) &key max-au-cost)
  (multiple-value-bind (pattern-meaning renamings)
      (fresh-variables (fcg-extract-meanings cipn))
    (let* ((anti-unification-results
            (anti-unify-predicate-network pattern-meaning source-meaning))
           ;; when are AU results valid/invalid in the case of partial analysis?
           (valid-anti-unification-results
            (remove-if-not #'valid-au-result-p anti-unification-results)))
      (when max-au-cost
        (setf valid-anti-unification-results
              (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                         valid-anti-unification-results)))
      (setf valid-anti-unification-results
            (loop for au-result in valid-anti-unification-results
                  collect (rerename-pattern-variables au-result renamings)))
      (sort valid-anti-unification-results #'< :key #'fcg::cost))))

#|
(defgeneric add-meaning-arg-predicates (set-of-predicates thing)
  (:documentation "Extract args from thing and add them as predicates
                   to the set of predicates."))

(defmethod add-meaning-arg-predicates (set-of-predicates (cxn fcg-construction))
  ;; top-args = args from contributing unit
  (loop for unit in (contributing-part cxn)
        for meaning-args = (first (fcg-unit-feature-value unit 'meaning-args))
        for category = (extract-category-unit unit)
        when (and meaning-args category)
        do (loop for arg in meaning-args
                 do (push (list 'top-arg arg category) set-of-predicates)))
  ;; slot-args = args from units that represent slots
  (loop for unit in (extract-slot-units cxn)
        for meaning-args = (remove-duplicates (first (fcg-unit-feature-value unit 'meaning-args)))
        for category = (extract-category-unit unit)
        when (and meaning-args category)
        do (loop for arg in meaning-args
                 do (push (list 'slot-arg arg category) set-of-predicates)))
  set-of-predicates)

(defmethod add-meaning-arg-predicates (set-of-predicates (args blackboard))
  ;; not used at the moment!!
  ;; will be necessary for recursion...
  (loop for arg in (find-data args :top-lvl-meaning-args)
        do (push (list 'top-arg arg) set-of-predicates))
  (loop for arg in (find-data args :slot-meaning-args)
        do (push (list 'slot-arg arg) set-of-predicates))
  set-of-predicates)

(defmethod add-meaning-arg-predicates (set-of-predicates (cipn cip-node))
  ;; top-args = args from slot-units that do not have 'form' and 'meaning' feature
  (let* ((ts-units (fcg-get-transient-unit-structure cipn))
         (root-unit (get-root ts-units))
         (all-slot-units (get-child-units (remove root-unit ts-units)))
         (open-slot-units
          (loop for unit in all-slot-units
                unless (and (unit-feature unit 'form) (unit-feature unit 'meaning))
                collect unit)))
    (loop for unit in open-slot-units
          for meaning-args = (unit-feature-value unit 'meaning-args)
          for category = (extract-category-unit unit)
          when (and meaning-args category)
          do (loop for arg in meaning-args
                   do (push (list 'top-arg arg category) set-of-predicates))))
  ;; slots-args = args from all top level units
  (let* ((ts-units (fcg-get-transient-unit-structure cipn))
         (root-unit (get-root ts-units))
         (top-lvl-units (remove-child-units (remove root-unit ts-units))))
    (loop for unit in top-lvl-units
          for meaning-args = (unit-feature-value unit 'meaning-args)
          for category = (extract-category-unit unit)
          when (and meaning-args category)
          do (loop for arg in meaning-args
                   do (push (list 'slot-arg arg category) set-of-predicates))))
  ;; return set of predicates
  set-of-predicates)
|#

;;;;;;;;;;;;;;;;;;;;;;
;; anti-unify utils ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun initialize-sequence-predicates (form-anti-unification)
  (with-slots (generalisation
               pattern-delta
               source-delta) form-anti-unification
    (let* ((generalisation-sequences
            (loop for elem in generalisation
                  if (stringp elem)
                    collect (list 'sequence elem (make-var 'lb) (make-var 'rb))
                  else collect elem))
           (pattern-delta-sequences
            (loop for (var . seq) in pattern-delta
                  collect (cons var (list 'sequence seq (make-var 'lb) (make-var 'rb)))))
           (source-delta-sequences
            (loop for (var . seq) in source-delta
                  collect (cons var (list 'sequence seq (make-var 'lb) (make-var 'rb))))))
      (setf generalisation generalisation-sequences
            pattern-delta pattern-delta-sequences
            source-delta source-delta-sequences))))

(defun rerename-pattern-variables (anti-unification-result renamings)
  (with-slots (generalisation
               pattern-bindings
               source-bindings
               pattern-delta
               source-delta) anti-unification-result
    (let ((bs (fcg::reverse-bindings renamings)))
      (setf pattern-bindings
            (loop for (x . y) in pattern-bindings
                  collect (cons (rest (assoc x bs)) y)))
      (setf pattern-delta
            (substitute-variables pattern-delta bs))
      anti-unification-result)))

(defun renamingp (bindings-list)
  "A bindings list is a renaming if the mappings are one to one"
  (let ((renamingp t))
    (loop for (binding . rest) on bindings-list
          when (find (car binding) rest :key #'car :test #'equalp)
          do (setf renamingp nil))
    renamingp))

(defun au-all-parts-present-p (au-result)
  "All parts of the anti-unification result
   (i.e. generalisation and both delta's)
   are non-empty!"
  (and (generalisation au-result)
       (source-delta au-result)
       (pattern-delta au-result)))

(defun au-partial-analysis-p (au-result)
  "The anti-unification result can be used as a partial
   analysis when the generalisation and the source-delta
   are non-empty, the pattern delta is empty (excluding args),
   and the pattern bindings list is a renaming."
  (and (generalisation au-result)
       (source-delta au-result)
       (null (pattern-delta au-result))
       (renamingp (pattern-bindings au-result))))

(defun valid-au-result-p (au-result)
  ;; valid when all parts are filled in
  ;; or when the pattern delta is empty (excluding args)
  ;; and the pattern bindings is a renaming
  (or
   (au-all-parts-present-p au-result)
   (au-partial-analysis-p au-result)))

(defun valid-au-combination-p (au-combination)
  ;; valid when both au results have both parts filled in
  ;; or both au results have both empty pattern delta's
  (or
   (and (au-all-parts-present-p (first au-combination))
        (au-all-parts-present-p (second au-combination)))
   (and (au-partial-analysis-p (first au-combination))
        (au-partial-analysis-p (second au-combination)))))

(defun sort-anti-unification-combinations (list-of-anti-unification-combinations)
  "Sort the anti-unifcation results based on cost (of both form- and
   meaning-anti-unification) and avg cxn score as a tie breaker."
  (sort list-of-anti-unification-combinations
        #'(lambda (combo-1 combo-2)
            (let ((combined-cost-1 (+ (fcg::cost (second combo-1)) (fcg::cost (third combo-1))))
                  (combined-cost-2 (+ (fcg::cost (second combo-2)) (fcg::cost (third combo-2))))
                  (cxn-score-1 (typecase (first combo-1)
                                 (fcg-construction (get-cxn-score (first combo-1)))
                                 (cip-node (average (mapcar #'get-cxn-score (original-applied-constructions (first combo-1)))))))
                  (cxn-score-2 (typecase (first combo-2)
                                 (fcg-construction (get-cxn-score (first combo-2)))
                                 (cip-node (average (mapcar #'get-cxn-score (original-applied-constructions (first combo-2))))))))
              (if (= combined-cost-1 combined-cost-2)
                (> cxn-score-1 cxn-score-2)
                (< combined-cost-1 combined-cost-2))))))


#|
(defun remove-arg-predicates (set-of-predicates)
  "Remove both top-args and slot-args predicates
   from the set of predicates."
  (let ((predicates (copy-list set-of-predicates)))
    (loop for predicate in '(top-arg slot-arg)
          do (setf predicates (remove predicate predicates :key #'first)))
    predicates))

(defun correct-au-cost (anti-unification-result)
  "Remove the cost of the top-arg and slot-arg predicates from
   the total anti-unification cost"
  (with-slots (g pb sb pattern-delta source-delta cost) anti-unification-result
    (declare (ignore g pb sb))
    (- cost (+ (count 'top-arg pattern-delta :key #'first)
               (count 'slot-arg pattern-delta :key #'first)
               (count 'top-arg source-delta :key #'first)
               (count 'slot-arg source-delta :key #'first)))))

(defun copy-arg-predicates (anti-unification-result)
  "Copy arg predicates from the pattern delta to the source delta;
   only copy predicates that include variables that can be found in
   the source bindings"
  ;; this unless clause might be dangerous when adding recursion...
  (unless (or (find 'top-arg (source-delta anti-unification-result) :key #'first)
              (find 'slot-arg (source-delta anti-unification-result) :key #'first))
    (let* ((top-arg-predicates (find-all 'top-arg (pattern-delta anti-unification-result) :key #'first))
           (slot-arg-predicates (find-all 'slot-arg (pattern-delta anti-unification-result) :key #'first)))
      (loop for predicate in (append top-arg-predicates slot-arg-predicates)
            for var = (second predicate)
            for gen-var = (rest (assoc var (pattern-bindings anti-unification-result)))
            for source-var = (first (rassoc gen-var (source-bindings anti-unification-result)))
            do (push (list (first predicate) (or source-var var) (third predicate))
                     (source-delta anti-unification-result))))))


(defun group-slot-args-into-units (predicates)
  "Group slot-arg predicates that have the same category as their
   third argument. This will be used to make units in the resulting cxn"
  (let* ((slot-arg-predicates (find-all 'slot-arg predicates :key #'first))
         (unique-categories (remove-duplicates (mapcar #'third slot-arg-predicates))))
    (loop for category in unique-categories
          collect (cons category
                        (loop for predicate in slot-arg-predicates
                              when (eql (last-elt predicate) category)
                                collect (second predicate))))))

(defun group-top-args-into-units (predicates)
  "Group top-arg predicates that have the same category as their
   third argument. This will be used to make units in the resulting cxn"
  (let* ((top-arg-predicates (find-all 'top-arg predicates :key #'first))
         (unique-categories (remove-duplicates (mapcar #'third top-arg-predicates))))
    (loop for category in unique-categories
          collect (cons category
                        (loop for predicate in top-arg-predicates
                              when (eql (last-elt predicate) category)
                                collect (second predicate))))))
|#