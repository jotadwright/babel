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

#|
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
|#

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
      ;; remove anti-unification results with high cost
      (when max-au-cost
        (setf valid-anti-unification-results
              (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                         valid-anti-unification-results)))
      ;; restore original variables
      (setf valid-anti-unification-results
            (loop for au-result in valid-anti-unification-results
                  collect (rerename-pattern-variables au-result renamings)))
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

;;;;;;;;;;;;;;;;;;;;;;;;
;; anti-unify meaning ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric anti-unify-meaning (source-meaning thing &key max-au-cost)
  (:documentation "Anti-unify the observation with the given cxn on the meaning side."))

(defmethod anti-unify-meaning (source-meaning (cxn fcg-construction) &key max-au-cost)
  ;; assign fresh variables to pattern
  (multiple-value-bind (pattern-meaning renamings)
      (fresh-variables (extract-meaning-predicates cxn))
    (let* (;; anti unify
           (anti-unification-results
            (anti-unify-predicate-network pattern-meaning source-meaning))
           ;; remove invalid anti-unification results
           (valid-anti-unification-results
            (remove-if-not #'valid-au-result-p anti-unification-results)))
      ;; remove anti-unification results with high cost
      (when max-au-cost
        (setf valid-anti-unification-results
              (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                         valid-anti-unification-results)))
      ;; restore original variables
      (setf valid-anti-unification-results
            (loop for au-result in valid-anti-unification-results
                  collect (rerename-pattern-variables au-result renamings)))
      (sort valid-anti-unification-results #'< :key #'fcg::cost))))

(defmethod anti-unify-meaning (source-meaning (cipn cip-node) &key max-au-cost)
  ;; assign fresh variables to pattern
  (multiple-value-bind (pattern-meaning renamings)
      (fresh-variables (fcg-extract-meanings cipn))
    (let* (;; anti-unify
           (anti-unification-results
            (anti-unify-predicate-network pattern-meaning source-meaning))
           ;; remove invalid anti-unification results
           (valid-anti-unification-results
            (remove-if-not #'valid-au-result-p anti-unification-results)))
      ;; remove anti-unification results with high cost
      (when max-au-cost
        (setf valid-anti-unification-results
              (remove-if #'(lambda (au-result) (> (fcg::cost au-result) max-au-cost))
                         valid-anti-unification-results)))
      ;; restore original variables
      (setf valid-anti-unification-results
            (loop for au-result in valid-anti-unification-results
                  collect (rerename-pattern-variables au-result renamings)))
      (sort valid-anti-unification-results #'< :key #'fcg::cost))))

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