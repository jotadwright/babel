(in-package :pf)

;;;;;;;;;;;;;;;;;;;;;
;; anti-unify form ;;
;;;;;;;;;;;;;;;;;;;;;

(defgeneric anti-unify-form (source-form thing &key max-au-cost no-string-cxns)
  (:documentation "Anti-unify the observation with the given thing on the form side"))

(defmethod anti-unify-form (source-form thing &key max-au-cost (no-string-cxns t))
  (let ((form-representation (get-configuration
                              (typecase thing
                                (fcg-construction (cxn-inventory thing))
                                (cip-node (construction-inventory thing)))
                              :form-representation-formalism)))
    (anti-unify-form-aux source-form thing form-representation
                         :max-au-cost max-au-cost
                         :no-string-cxns no-string-cxns)))

;;;;;;;;;;;;;;;
;; sequences ;;
;;;;;;;;;;;;;;;

(defmethod anti-unify-form-aux (source-form (cxn fcg-construction) (mode (eql :sequences)) &key max-au-cost (no-string-cxns t))
  (declare (ignore no-string-cxns))
  (multiple-value-bind (possible-pattern-forms sequence-boundaries)
      (render-all (extract-form-predicates cxn) :render-sequences)
    (let* ((possible-pattern-forms
            (mapcar #'(lambda (lst) (list-of-strings->string lst :separator "")) possible-pattern-forms))
           (source-form
            ;; !! 'first' is an assumption that will not work when adding recursion
            (first
             (mapcar #'(lambda (lst) (list-of-strings->string lst :separator ""))
                     (render-all source-form :render-sequences))))
           (anti-unification-results
            (loop for pattern-form in possible-pattern-forms
                  for boundaries in sequence-boundaries
                  for au-results = (anti-unify-strings pattern-form source-form)
                  append (loop for au-result in au-results
                               when (and (valid-au-result-p au-result)
                                         max-au-cost
                                         (<= (fcg::cost au-result) max-au-cost))
                               do (rerename-boundaries au-result boundaries)
                               and collect au-result))))
      (sort anti-unification-results #'< :key #'fcg::cost))))


(defun rerename-boundaries (au-result sequence-boundaries)
  (let ((slot-boundaries (instantiate-slot-boundaries (generalisation au-result) (pattern-delta au-result))))
    (dolist (slot-boundary slot-boundaries)
      (destructuring-bind (slot-var start end) slot-boundary
        (let* ((lb (find start sequence-boundaries :key #'cdr :test #'=))
               (rb (find end sequence-boundaries :key #'cdr :test #'=))
               (matching-boundary (cond (lb (car lb)) (rb (car rb))))
               replacement)
          (if matching-boundary
            (let* ((match-position (position matching-boundary sequence-boundaries :key #'car))
                   (buddy (unless (= match-position 0)
                            (car (if (evenp match-position)
                                   (nth (1- match-position) sequence-boundaries)
                                   (nth (1+ match-position) sequence-boundaries))))))
              (unless buddy (setf buddy (make-var)))
              (setf replacement
                    (if (evenp match-position)
                      (cons buddy matching-boundary)
                      (cons matching-boundary buddy))))
            (setf replacement (cons (make-var 'lb) (make-var 'rb))))
          (setf (generalisation au-result) (subst replacement slot-var (generalisation au-result))
                (pattern-delta au-result) (subst replacement slot-var (pattern-delta au-result))
                (source-delta au-result) (subst replacement slot-var (source-delta au-result))))))))


(defun instantiate-slot-boundaries (set-of-sequence-predicates bindings)
  (let ((index 0)
        positions)
    (dolist (elem set-of-sequence-predicates)
      (if (variable-p elem)
        (let* ((len (length (rest (assoc elem bindings))))
               (start-pos index)
               (end-pos (+ index len)))
          (push (list elem start-pos end-pos) positions)
          (incf index len))
        (incf index (length elem))))
    (reverse positions)))

;(instantiate-slot-boundaries
; '("what" ?x "is the" ?y "?")
; '((?x "color") (?y "ball")))
; => ((?x 5 10) (?y 18 22))

; (sequence-boundaries '((sequence "what color is the " ?lb ?rb)))
; => ((?lb 0) (?rb 18))

; ===> '("what" (?var-1 ?var-2) "is the" (?rb ?var) "?")
;      '((?var-1 ?var-2) . color) ((?rb ?var-3) . "ball"))



(defmethod anti-unify-form-aux (source-form (cipn cip-node) (mode (eql :sequences)) &key max-au-cost (no-string-cxns t))
  (declare (ignore no-string-cxns))
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


;;;;;;;;;;;;;;;;;;
;; string+meets ;;
;;;;;;;;;;;;;;;;;;

(defmethod anti-unify-form-aux (source-form (cxn fcg-construction) (mode (eql :string+meets)) &key max-au-cost (no-string-cxns t))
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
      ;; remove anti-unification-results with no strings in them (when set)
      (unless no-string-cxns
        (setf valid-anti-unification-results
              (remove-if-not #'(lambda (au-result)
                                 (and (find 'string (generalisation au-result) :key #'first)
                                      (find 'string (source-delta au-result) :key #'first)
                                      (find 'string (pattern-delta au-result) :key #'first)))
                             valid-anti-unification-results)))
      ;; restore original variables
      (setf valid-anti-unification-results
            (loop for au-result in valid-anti-unification-results
                  collect (rerename-pattern-variables au-result renamings)))
      (sort valid-anti-unification-results #'< :key #'fcg::cost))))


#|
(defmethod anti-unify-form-aux (source-form (cipn cip-node) (mode (eql :string+meets)) &key max-au-cost (no-string-cxns t))
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
    ;; remove anti-unification-results with no strings in them (when set)
    (unless no-string-cxns
        (setf valid-anti-unification-results
              (remove-if-not #'(lambda (au-result)
                                 (and (find 'string (generalisation au-result) :key #'first)
                                      (find 'string (source-delta au-result) :key #'first)))
                             valid-anti-unification-results)))
    (sort valid-anti-unification-results #'< :key #'fcg::cost)))
|#


(defmethod anti-unify-form-aux (source-form (cipn cip-node) (mode (eql :string+meets)) &key max-au-cost (no-string-cxns t))
  (let* (;; extract pattern form
         (pattern-form
          (variablify-form-constraints-with-constants
           (loop for unit in (fcg-get-transient-unit-structure cipn)
                 unless (eql (unit-name unit) 'fcg::root)
                 append (unit-feature-value unit 'form))))
         ;; extract remaining form from root (shortcut!)
         (form-in-root
          (variablify-form-constraints-with-constants
           (form-predicates-in-root cipn)))
         ;; make au-result
         (au-result
          (make-instance 'fcg::anti-unification-result
                         :pattern pattern-form
                         :source source-form
                         :generalisation pattern-form
                         :pattern-delta nil
                         :source-delta form-in-root
                         :cost (length form-in-root))))
    ;; apply filters
    (when max-au-cost
      (when (> (length form-in-root) max-au-cost)
        (setf au-result nil)))
    (unless no-string-cxns
      (unless (find 'string form-in-root :key #'first)
        (setf au-result nil)))
    (when au-result
      (list au-result))))

                        

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

(defun push-meets-to-deltas (anti-unification-result pattern-top-args source-top-args)
  (with-slots (generalisation
               pattern-bindings
               source-bindings
               pattern-delta
               source-delta) anti-unification-result
    (let* ((generalisation-meets
            (find-all 'meets generalisation :key #'first))
           (generalisation-string-vars
            (remove-duplicates (mapcar #'second (find-all 'string generalisation :key #'first))))
           (source-delta-string-vars
            (remove-duplicates (mapcar #'second (find-all 'string source-delta :key #'first))))
           (pattern-delta-string-vars
            (remove-duplicates (mapcar #'second (find-all 'string pattern-delta :key #'first))))
           (potential-meets-to-move
            (loop for meets-predicate in generalisation-meets
                  for left-var = (second meets-predicate)
                  for right-var = (third meets-predicate)
                  when (and (not (find left-var generalisation-string-vars))
                            (not (find right-var generalisation-string-vars)))
                  collect meets-predicate)))
      (loop for meets-predicate in potential-meets-to-move
            for left-var = (second meets-predicate)
            for right-var = (third meets-predicate)
            for connected-in-source
              = (or (find (first (rassoc left-var source-bindings)) source-delta-string-vars)
                    (find (first (rassoc right-var source-bindings)) source-delta-string-vars))
            for connected-in-pattern
              = (or (find (first (rassoc left-var pattern-bindings)) pattern-delta-string-vars)
                    (find (first (rassoc right-var pattern-bindings)) pattern-delta-string-vars))
            when (and connected-in-source connected-in-pattern)
            do (push (list 'meets
                           (first (rassoc left-var source-bindings))
                           (first (rassoc right-var source-bindings)))
                     source-delta)
               (push (list 'meets
                           (first (rassoc left-var pattern-bindings))
                           (first (rassoc right-var pattern-bindings)))
                     pattern-delta)
               (setf generalisation (remove meets-predicate generalisation :test #'equal))
            unless (or (find-anywhere left-var generalisation)
                       (find (first (rassoc left-var pattern-bindings)) pattern-top-args)
                       (find (first (rassoc left-var source-bindings)) source-top-args))
             do (progn
                  (setf source-bindings (remove left-var source-bindings :key #'cdr))
                  (setf pattern-bindings (remove left-var pattern-bindings :key #'cdr)))
            unless (or (find-anywhere right-var generalisation)
                       (find (first (rassoc right-var pattern-bindings)) pattern-top-args)
                       (find (first (rassoc right-var source-bindings)) source-top-args))
              do (progn
                   (setf source-bindings (remove right-var source-bindings :key #'cdr))
                   (setf pattern-bindings (remove right-var pattern-bindings :key #'cdr)))))
    anti-unification-result))

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