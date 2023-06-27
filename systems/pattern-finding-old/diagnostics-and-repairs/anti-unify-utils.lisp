(in-package :pattern-finding-old)

;;;;;;;;;;;;;;;;;;;;;
;; anti-unify form ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric anti-unify-form (source-form thing args &optional max-au-cost)
  (:documentation "Anti-unify the observation with the given thing on the form side."))

(defmethod anti-unify-form (source-form (cxn fcg-construction) (args blackboard) &optional max-au-cost)
  ;; before anti unifying, top-args and slot-args are added to the
  ;; source-form and pattern-form! This makes the learning of cxns
  ;; easier later on
  (let* ((pattern-form (extract-form-predicates cxn))
         (pattern-form-with-args (add-form-arg-predicates pattern-form cxn))
         (source-form-with-args (add-form-arg-predicates source-form args))
         (anti-unification-results
          (anti-unify-predicate-network (fresh-variables pattern-form-with-args) source-form-with-args))
         (valid-anti-unification-results
          (remove-if-not #'valid-au-result-p anti-unification-results)))
    (when max-au-cost
      (loop for au-result in valid-anti-unification-results
            do (setf (fcg::cost au-result)
                     (correct-au-cost au-result)))
      (setf valid-anti-unification-results
            (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                       valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))

(defmethod anti-unify-form (source-form (ts coupled-feature-structure) (args blackboard) &optional max-au-cost)
  ;; before anti unifying, top-args and slot-args are added to the
  ;; source-form and pattern-form! This makes the learning of cxns
  ;; easier later on
  (let* ((pattern-form
          (loop for unit in (fcg-get-transient-unit-structure ts)
                unless (eql (unit-name unit) 'fcg::root)
                append (unit-feature-value unit 'form)))
         (pattern-form-with-args (add-form-arg-predicates pattern-form ts))
         (source-form-with-args (add-form-arg-predicates source-form args))
         (anti-unification-results
          (anti-unify-predicate-network
           ;; first add the arg-predicates, before variablifying everything!
           (fresh-variables (variablify-form-constraints-with-constants pattern-form-with-args))
           source-form-with-args))
         ;; when are AU results valid/invalid in the case of partial analysis?
         (valid-anti-unification-results
          (remove-if-not #'valid-au-result-p anti-unification-results)))
    (when max-au-cost
      (loop for au-result in valid-anti-unification-results
            do (setf (fcg::cost au-result)
                     (correct-au-cost au-result)))
      (setf valid-anti-unification-results
            (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                       valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))


(defgeneric add-form-arg-predicates (set-of-predicates thing)
  (:documentation "Extract args from thing and add them as predicates
                   to the set of predicates."))

(defmethod add-form-arg-predicates (set-of-predicates (cxn fcg-construction))
  ;; top-args = args from contributing unit
  (loop for unit in (contributing-part cxn)
        for form-args = (remove-duplicates (first (fcg-unit-feature-value unit 'form-args)))
        for lex-class = (extract-lex-class-unit unit)
        when (and form-args lex-class)
        do (loop for arg in form-args
                 do (push (list 'top-arg arg lex-class) set-of-predicates)))
  ;; slot-args = args from units that represent slots
  (loop for unit in (extract-slot-units cxn)
        for form-args = (remove-duplicates (first (fcg-unit-feature-value unit 'form-args)))
        for lex-class = (extract-lex-class-unit unit)
        do (loop for arg in form-args
                 do (push (list 'slot-arg arg lex-class) set-of-predicates)))
  set-of-predicates)

(defmethod add-form-arg-predicates (set-of-predicates (args blackboard))
  ;; not used at the moment!!
  ;; will be necessary for recursion...
  (loop for arg in (find-data args :top-lvl-form-args)
        do (push (list 'top-arg arg) set-of-predicates))
  (loop for arg in (find-data args :slot-form-args)
        do (push (list 'slot-arg arg) set-of-predicates))
  set-of-predicates)

(defmethod add-form-arg-predicates (set-of-predicates (ts coupled-feature-structure))
  ;; top-args = args from slot-units that do not have 'form' and 'meaning' feature
  (let* ((ts-units (fcg-get-transient-unit-structure ts))
         (root-unit (get-root ts-units))
         (all-slot-units (get-child-units (remove root-unit ts-units)))
         (open-slot-units
          (loop for unit in all-slot-units
                unless (and (unit-feature unit 'form) (unit-feature unit 'meaning))
                collect unit)))
    (loop for unit in open-slot-units
          for form-args = (remove-duplicates (first (fcg-unit-feature-value unit 'form-args)))
          for lex-class = (extract-lex-class-unit unit)
          when (and form-args lex-class)
          do (loop for arg in form-args
                   do (push (list 'top-arg arg lex-class) set-of-predicates))))
  ;; slots-args = args from all top level units
  (let* ((ts-units (fcg-get-transient-unit-structure ts))
         (root-unit (get-root ts-units))
         (top-lvl-units (remove-child-units (remove root-unit ts-units))))
    (loop for unit in top-lvl-units
          for form-args = (remove-duplicates (first (fcg-unit-feature-value unit 'form-args)))
          for lex-class = (extract-lex-class-unit unit)
          when (and form-args lex-class)
          do (loop for arg in form-args
                   do (push (list 'slot-arg arg lex-class) set-of-predicates)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; anti-unify meaning ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric anti-unify-meaning (source-meaning thing args &optional max-au-cost)
  (:documentation "Anti-unify the observation with the given cxn on the meaning side."))

(defmethod anti-unify-meaning (source-meaning (cxn fcg-construction) (args blackboard) &optional max-au-cost)
  ;; before anti unifying, top-args and slot-args are added to the
  ;; source-meaning and pattern-meaning! This makes the learning of cxns
  ;; easier later on
  (let* ((pattern-meaning (extract-meaning-predicates cxn))
         (pattern-meaning-with-args (add-meaning-arg-predicates pattern-meaning cxn))
         (source-meaning-with-args (add-meaning-arg-predicates source-meaning args))
         (anti-unification-results
          (anti-unify-predicate-network (fresh-variables pattern-meaning-with-args) source-meaning-with-args))
         (valid-anti-unification-results
          (remove-if-not #'valid-au-result-p anti-unification-results)))
    (when max-au-cost
      (loop for au-result in valid-anti-unification-results
            do (setf (fcg::cost au-result)
                     (correct-au-cost au-result)))
      (setf valid-anti-unification-results
            (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                       valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))

(defmethod anti-unify-meaning (source-meaning (ts coupled-feature-structure) (args blackboard) &optional max-au-cost)
  ;; before anti unifying, top-args and slot-args are added to the
  ;; source-meaning and pattern-meaning! This makes the learning of cxns
  ;; easier later on
  (let* ((pattern-meaning (fcg-extract-meanings ts))
         (pattern-meaning-with-args (add-meaning-arg-predicates pattern-meaning ts))
         (source-meaning-with-args (add-meaning-arg-predicates source-meaning args))
         (anti-unification-results
          (anti-unify-predicate-network (fresh-variables pattern-meaning-with-args) source-meaning-with-args))
         ;; when are AU results valid/invalid in the case of partial analysis?
         (valid-anti-unification-results
          (remove-if-not #'valid-au-result-p anti-unification-results)))
    (when max-au-cost
      (loop for au-result in valid-anti-unification-results
            do (setf (fcg::cost au-result)
                     (correct-au-cost au-result)))
      (setf valid-anti-unification-results
            (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                       valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))


(defgeneric add-meaning-arg-predicates (set-of-predicates thing)
  (:documentation "Extract args from thing and add them as predicates
                   to the set of predicates."))

(defmethod add-meaning-arg-predicates (set-of-predicates (cxn fcg-construction))
  ;; top-args = args from contributing unit
  (loop for unit in (contributing-part cxn)
        for meaning-args = (remove-duplicates (first (fcg-unit-feature-value unit 'meaning-args)))
        for lex-class = (extract-lex-class-unit unit)
        when (and meaning-args lex-class)
        do (loop for arg in meaning-args
                 do (push (list 'top-arg arg lex-class) set-of-predicates)))
  ;; slot-args = args from units that represent slots
  (loop for unit in (extract-slot-units cxn)
        for meaning-args = (remove-duplicates (first (fcg-unit-feature-value unit 'meaning-args)))
        for lex-class = (extract-lex-class-unit unit)
        do (loop for arg in meaning-args
                 do (push (list 'slot-arg arg lex-class) set-of-predicates)))
  set-of-predicates)

(defmethod add-meaning-arg-predicates (set-of-predicates (args blackboard))
  ;; not used at the moment!!
  ;; will be necessary for recursion...
  (loop for arg in (find-data args :top-lvl-meaning-args)
        do (push (list 'top-arg arg) set-of-predicates))
  (loop for arg in (find-data args :slot-meaning-args)
        do (push (list 'slot-arg arg) set-of-predicates))
  set-of-predicates)

(defmethod add-meaning-arg-predicates (set-of-predicates (ts coupled-feature-structure))
  ;; top-args = args from slot-units that do not have 'form' and 'meaning' feature
  (let* ((ts-units (fcg-get-transient-unit-structure ts))
         (root-unit (get-root ts-units))
         (all-slot-units (get-child-units (remove root-unit ts-units)))
         (open-slot-units
          (loop for unit in all-slot-units
                unless (and (unit-feature unit 'form) (unit-feature unit 'meaning))
                collect unit)))
    (loop for unit in open-slot-units
          for meaning-args = (remove-duplicates (first (fcg-unit-feature-value unit 'meaning-args)))
          for lex-class = (extract-lex-class-unit unit)
          when (and meaning-args lex-class)
          do (loop for arg in meaning-args
                   do (push (list 'top-arg arg lex-class) set-of-predicates))))
  ;; slots-args = args from all top level units
  (let* ((ts-units (fcg-get-transient-unit-structure ts))
         (root-unit (get-root ts-units))
         (top-lvl-units (remove-child-units (remove root-unit ts-units))))
    (loop for unit in top-lvl-units
          for meaning-args = (remove-duplicates (first (fcg-unit-feature-value unit 'meaning-args)))
          for lex-class = (extract-lex-class-unit unit)
          when (and meaning-args lex-class)
          do (loop for arg in meaning-args
                   do (push (list 'slot-arg arg lex-class) set-of-predicates)))))


;;;;;;;;;;;;;;;;;;;;;;
;; anti-unify utils ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun remove-arg-predicates (set-of-predicates)
  "Remove both top-args and slot-args predicates
   from the set of predicates."
  (let ((predicates (copy-list set-of-predicates)))
    (loop for predicate in '(top-arg slot-arg)
          do (setf predicates (remove predicate predicates :key #'first)))
    predicates))

(defun correct-au-cost (anti-unification-result)
  (with-slots (g pb sb pattern-delta source-delta cost) anti-unification-result
    (declare (ignore g pb sb))
    (- cost (+ (count 'top-arg pattern-delta :key #'first)
               (count 'slot-arg pattern-delta :key #'first)
               (count 'top-arg source-delta :key #'first)
               (count 'slot-arg source-delta :key #'first)))))

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
       (remove-arg-predicates (pattern-delta au-result))))

(defun au-partial-analysis-p (au-result)
  "The anti-unification result can be used as a partial
   analysis when the generalisation and the source-delta
   are non-empty, the pattern delta is empty (excluding args),
   and the pattern bindings list is a renaming."
  (and (generalisation au-result)
       (source-delta au-result)
       (null (remove-arg-predicates (pattern-delta au-result)))
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
            (let ((combined-cost-1 (+ (cost (second combo-1)) (cost (third combo-1))))
                  (combined-cost-2 (+ (cost (second combo-2)) (cost (third combo-2))))
                  (cxn-score-1 (get-cxn-score (first combo-1)))
                  (cxn-score-2 (get-cxn-score (first combo-2))))
              (if (= combined-cost-1 combined-cost-2)
                (> cxn-score-1 cxn-score-2)
                (< combined-cost-1 combined-cost-2))))))