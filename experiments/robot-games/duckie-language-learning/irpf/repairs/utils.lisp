(in-package :duckie-language-learning)

;; ------------------
;; + Util functions +
;; ------------------

(defun create-new-categorial-links (lex-classes-lex-cxns lex-classes-item-based-units categorial-network)
  "Creates all CATEGORIAL-NETWORK links for matching lexical cxns using their original lex-class."
  (loop for lex-cxn-lex-class in lex-classes-lex-cxns
        for item-slot-lex-class in lex-classes-item-based-units
        unless (neighbouring-categories-p lex-cxn-lex-class item-slot-lex-class categorial-network)
          collect (cons lex-cxn-lex-class item-slot-lex-class)))

(defun create-categorial-links (problem node)
  "Return the CATEGORIAL-NETWORK links and applied cxns from a comprehend with :category-linking-mode :path-exists instead of :neighbours"
  (let* ((utterance (random-elt (get-data problem :utterances)))
         (gold-standard-meaning (random-elt (get-data problem :meanings)))
         (cxn-inventory (construction-inventory node))
         (orig-cxn-set (original-cxn-set cxn-inventory))
         (categorial-network (categorial-network cxn-inventory)))
    (disable-meta-layer-configuration cxn-inventory) ;(fcg::unify-atom
    (with-disabled-monitor-notifications
      (let* ((comprehension-result (multiple-value-list (comprehend utterance :cxn-inventory orig-cxn-set :gold-standard-meaning gold-standard-meaning)))
             (meaning-network (first comprehension-result))
             (cip-node (second comprehension-result))
             (cip (third comprehension-result)))
        (enable-meta-layer-configuration cxn-inventory)
        ;;there is a solution with connected links in the CATEGORIAL-NETWORK
        (when (member 'succeeded (statuses cip-node) :test #'string=)
          (let* ((applied-cxns (applied-constructions cip-node))
                 (lex-cxns (sort-cxns-by-form-string (filter-by-phrase-type 'lexical applied-cxns) utterance))
                 (lex-classes-lex-cxns (when lex-cxns
                                         (map 'list #'lex-class-cxn lex-cxns)))
                 (item-based-cxn (first (filter-by-phrase-type 'item-based applied-cxns)))
                 (lex-classes-item-based-units (when item-based-cxn
                                                 (get-all-unit-lex-classes item-based-cxn)))
                 (categorial-links (when (and lex-classes-lex-cxns lex-classes-item-based-units
                                              (= (length lex-classes-lex-cxns) (length lex-classes-item-based-units)))
                                     (create-new-categorial-links lex-classes-lex-cxns
                                                                  lex-classes-item-based-units
                                                                  categorial-network))))
            (when (filter-by-phrase-type 'holophrase applied-cxns)
              (format t "Something has gone horribly wrong!"))
            (list applied-cxns categorial-links)))))))

;; -------------------------
;; + Util functions for GL +
;; -------------------------

(defun initial-node-p (node) ;; has duplicate in utils.lisp
  (find 'fcg::initial (fcg::statuses node)))

(defun initial-node (node)
  "returns the first node in the cip"
  (if (all-parents node)
    (last-elt (all-parents node))
    node))

(defun initial-transient-structure (node)
  (if (find 'fcg::initial (statuses node))
    (car-source-cfs (cipn-car node))
    (car-source-cfs (cipn-car (last-elt (all-parents node))))))

;; irl utils
(defun replace-anywhere-if (new-fn predicate-fn tree)
  "Replace all items in 'tree' that return t
   by 'predicate-fn' with the result from 'new-fn'
   applied to the old element."
  (cond ((atom tree)
         ;; if the predicate returns t on some element,
         ;; apply new-fn to the old element
         ;; and use this as the new element
         (if (funcall predicate-fn tree)
           (funcall new-fn tree) tree))
        (t
         (cons (replace-anywhere-if new-fn predicate-fn (car tree))
               (replace-anywhere-if new-fn predicate-fn (cdr tree))))))

(defun deduplicate-variables (irl-program)
  "Find if there are any duplicate variables.
   If there are, deduplicate them."
  (let* ((all-vars (all-variables irl-program))
         (unique-vars (remove-duplicates all-vars))
         (duplicates
          (loop for var in unique-vars
                when (> (count var all-vars) 1)
                collect var)))
    (if duplicates
      (replace-anywhere-if
       #'(lambda (v) (make-var (get-base-name v)))
       #'(lambda (v) (member v duplicates))
       irl-program)
      irl-program)))

;; for repairing
(defun sort-cxns-by-form-string (cxns-to-sort utterance)
  "sorts lexical cxns by matching their form strings to the utterance. handles duplicate cxns in one utterance."
  ;; warning, this function depends on space separation without further punctuation!
  ;; todo: replace by looking up bindings?
  (if (< (length cxns-to-sort) 2)
    cxns-to-sort
    (let ((resulting-list (make-list (length utterance))))
      (loop for cxn-obj in cxns-to-sort
            for cxn-string = (third (first (extract-form-predicates cxn-obj)))
            do (loop
                  with sub-length = (length cxn-string)
                  for i from 0 to (- (length utterance) sub-length)
                  when (string= utterance cxn-string
                                :start1 i :end1 (+ i sub-length))
                    do (when (and
                              (or
                               (= (+ i sub-length) (length utterance)) ;; end of utterance
                               (loop for punct across ".;,!?: "
                                     thereis (string= punct utterance
                                                      :start2 (+ i sub-length)
                                                      :end2 (+ i sub-length 1)))) ;; next char is word boundary
                              (or
                               (= i 0) ;; start of utterance
                               (string= " " utterance
                                        :start2 (- i 1)
                                        :end2 i))) ;; prev char is space
                         (setf (nth i resulting-list) cxn-obj))))
      (remove nil resulting-list))))

;; to do: split into method with type specification for processing cxn vs original cxn
;fcg-construction
;construction
(defun get-all-unit-lex-classes (cxn)
  (loop for unit in (subseq (contributing-part cxn) 1)
        for lex-class = (lex-class-item-based unit)
        collect lex-class))

(defun phrase-type (cxn)
  (loop for unit in (contributing-part cxn)
        for syn-cat = (cdr (find 'syn-cat (fcg::unit-structure unit) :key #'first))
        for phrase-type = (when syn-cat (second (find 'phrase-type syn-cat :key #'first)))
        when phrase-type
          return phrase-type))

;;;;;
(defun lex-class (unit)
  (let* ((syn-cat (find 'syn-cat (unit-body unit) :key #'first))
         (lex-class (find 'fcg::lex-class (second syn-cat) :key #'first)))    
    (when lex-class
      (second lex-class))))

(defun lex-class-item-based (unit)
  (let ((syn-cat (find 'syn-cat (fcg::unit-structure unit) :key #'first)))
    (second (find 'fcg::lex-class (rest syn-cat) :key #'first))))

(defun lex-class-cxn (lexical-cxn)
  (let ((syn-cat (find 'syn-cat (fcg::unit-structure (first (contributing-part lexical-cxn))) :key #'feature-name)))
    (second (find 'fcg::lex-class (rest syn-cat) :key #'first))))

(defun non-overlapping-meaning (meaning cxn &key (nom-cxn nil) (nom-observation nil))
  (when (and nom-cxn nom-observation) (error "only nom-cxn or nom-observeration can be true"))
  (multiple-value-bind (non-overlapping-meaning-observation non-overlapping-meaning-cxn)
      (non-overlapping-predicates meaning (extract-meaning-predicates cxn))
    (cond (nom-cxn non-overlapping-meaning-cxn)
          (nom-observation non-overlapping-meaning-observation))))

(defun non-overlapping-form (utterance cxn &key (nof-cxn nil) (nof-observation nil))
  (when (and nof-cxn nof-observation) (error "only nof-cxn or nof-observeration can be true"))
  (multiple-value-bind (non-overlapping-form-observation non-overlapping-form-cxn)
      (non-overlapping-predicates (form-constraints-with-variables utterance (get-configuration (cxn-inventory cxn) :de-render-mode))
                                  (extract-form-predicates cxn))
    (cond (nof-cxn non-overlapping-form-cxn)
          (nof-observation non-overlapping-form-observation))))

(defun non-overlapping-predicates (network-1 network-2)
  (let ((unique-part-network-1 (set-difference network-1 network-2 :test #'irl:unify-irl-programs))
        (unique-part-network-2 (set-difference network-2 network-1 :test #'irl:unify-irl-programs)))
    (when (and (= (length unique-part-network-1)
                  (length unique-part-network-2))
               (irl:equivalent-irl-programs? (set-difference network-1 unique-part-network-1)
                                             (set-difference network-2 unique-part-network-2)))
      (values unique-part-network-1 unique-part-network-2))))

(defun add-cxn-suffix (string)
  (intern (string-append string "-CXN")))

(defgeneric make-cxn-name (thing cxn-inventory &key add-cxn-suffix))

(defmethod make-cxn-name ((string string) (cxn-inventory fcg-construction-set)
                          &key (add-cxn-suffix t))
  "Transform an utterance into a suitable construction name"
  (declare (ignore cxn-inventory))
  (intern
   (symbol-name
    (make-symbol
     (substitute #\- #\Space
                 (upcase
                  (if add-cxn-suffix
                    (string-append string "-cxn")
                    string)))))))

;; (make-cxn-name "What is the color of the cube" *fcg-constructions*)

(defmethod make-cxn-name ((form list) (cxn-inventory fcg-construction-set)
                          &key (add-cxn-suffix t))
  "Transform an utterance into a suitable construction name"
  (loop with string-constraints = (extract-form-predicate-by-type form 'string)
        with placeholders = '("?X" "?Y" "?Z" "?A" "?B" "?C" "?D" "?E" "?F" "?G" "?H" "?I" "?J" "?K" "?L" "?M" "?N" "?O" "?P" "?Q" "?R" "?S" "?T" "?U" "?V" "?W")
        with placeholder-index = 0
        with new-string-constraints = '()
        for order-constraint in (set-difference form string-constraints)
        for first-word-var = (second order-constraint)
        for second-word-var = (third order-constraint)
        do
          (unless (or (find first-word-var string-constraints :key #'second)
                      (find first-word-var new-string-constraints :key #'second))
            (push `(string ,first-word-var ,(nth placeholder-index placeholders)) new-string-constraints)
            (incf placeholder-index))
          (unless (or (find second-word-var string-constraints :key #'second)
                      (find second-word-var new-string-constraints :key #'second))
            (push `(string ,second-word-var ,(nth placeholder-index placeholders)) new-string-constraints)
            (incf placeholder-index))
        finally (return
                 (make-cxn-name (format nil "~{~a~^-~}"
                                        (render (append form new-string-constraints)
                                                (get-configuration cxn-inventory :render-mode)))
                                cxn-inventory :add-cxn-suffix add-cxn-suffix))))

(defun make-cxn-placeholder-name (form cxn-inventory)
  (loop with string-constraints = (extract-form-predicate-by-type form 'string)
        with placeholders = '("?X" "?Y" "?Z" "?A" "?B" "?C" "?D" "?E" "?F" "?G" "?H" "?I" "?J" "?K" "?L" "?M" "?N" "?O" "?P" "?Q" "?R" "?S" "?T" "?U" "?V" "?W")
        with placeholder-index = 0
        with new-string-constraints = '()
        for order-constraint in (set-difference form string-constraints)
        for first-word-var = (second order-constraint)
        for second-word-var = (third order-constraint)
        do
          (unless (or (find first-word-var string-constraints :key #'second)
                      (find first-word-var new-string-constraints :key #'second))
            (push `(string ,first-word-var ,(nth placeholder-index placeholders)) new-string-constraints)
            (incf placeholder-index))
          (unless (or (find second-word-var string-constraints :key #'second)
                      (find second-word-var new-string-constraints :key #'second))
            (push `(string ,second-word-var ,(nth placeholder-index placeholders)) new-string-constraints)
            (incf placeholder-index))
        finally (return (render (append form new-string-constraints) (get-configuration cxn-inventory :render-mode)))))

(defun extract-placeholder-var-list (rendered-list)
  (loop for item in rendered-list
        when (string-equal (subseq item 0 1) "?")
          collect item))

(defun extract-form-predicate-by-type (form-values symbol)
  "extract meets, precedes or string predicates from a list of form predicates"
  (loop for form-value in form-values
        when (and (consp form-value) (eq (first form-value) symbol))
          collect form-value))

(defun form-constraints-with-variables (utterance mode)
  "Extract form constraints from utterance in the format they would appear in a construction."
  (let ((form-constraints-with-constants (remove 'sequence
                                                 (extract-forms (left-pole-structure
                                                                 (de-render utterance mode)))
                                                 :key #'first)))
    (loop for fc-with-const in form-constraints-with-constants
          for first-fc-with-var = (first fc-with-const)
          for rest-fc-with-var = (if (equal 'string (first fc-with-const))
                                   (list (variablify (second fc-with-const)) (third fc-with-const))
                                   (mapcar #'variablify (rest fc-with-const)))
          collect (cons first-fc-with-var rest-fc-with-var))))

(defun form-predicates-with-variables (form-predicates)
  "Transform form constraints with constants to form constraints with variables."
  (loop for predicate in form-predicates
        collect (if (equalp (first predicate) 'string)
                  (list (first predicate)
                        (variablify (second predicate))
                        (third predicate))
                  (cons (first predicate)
                        (mapcar #'variablify (rest predicate))))))

(defun variablify (symbol)
  "Turn a symbol into a variable if it isn't one yet."
  (if (variable-p symbol)
    symbol
    (intern (format nil "?~a" symbol))))

;; holophrase addition
(defun diff-superset-subset-form (superset-cxn utterance)
  (set-difference (extract-form-predicates superset-cxn)
                  (form-constraints-with-variables utterance (get-configuration (cxn-inventory superset-cxn) :de-render-mode))
                  :test #'irl:unify-irl-programs))
  
; util for a util
(defun transient-structure-form-constraints (transient-structure)
  (remove-if-not #'(lambda (fc)
                     (equal 'string (first fc)))
                 (extract-forms (left-pole-structure transient-structure))))

(defun find-superset-holophrase-cxn (transient-structure cxn-inventory gold-standard-meaning utterance)
  ;; todo: there could also be more than one superset cxn!
  (loop with ts-form-constraints = (transient-structure-form-constraints transient-structure)
        for cxn in (constructions cxn-inventory)
        for cxn-form-constraints = (extract-form-predicates cxn)
        for cxn-meaning-constraints = (extract-meaning-predicates cxn)
        for cxn-type =  (phrase-type cxn)
        for non-overlapping-form = (diff-superset-subset-form cxn utterance)
        for non-overlapping-meaning = (set-difference (extract-meaning-predicates cxn) gold-standard-meaning :test #'irl:unify-irl-programs)
        when (and (eql cxn-type 'holophrase)
                  (equal 1 (length non-overlapping-meaning))
                  (equal 1 (length non-overlapping-form))
                  ;; check if all the strings in the form constraints are present in the superset
                  ;; precedes relations are not important
                  (loop for ts-fc in ts-form-constraints
                        always (find (third ts-fc) cxn-form-constraints :key #'third :test #'equalp)) ;; loop returns true if all are true, the third elem is the string
                  (loop for predicate in gold-standard-meaning
                        always (if (equal (first predicate) 'bind)
                                 (find (fourth predicate) cxn-meaning-constraints :key #'fourth)
                                 (find (first predicate) cxn-meaning-constraints :key #'first))))
          ;; needs to be a holophrase, the form constraints for string and precedes constraints need to be a subset of the cxn, the meaning constraints need to be a subset too (todo: see if this is really the case in IRL)
          return (values cxn non-overlapping-form non-overlapping-meaning)))

;; holo substitution
(defun select-cxn-for-making-item-based-cxn (cxn-inventory utterance meaning)
  (loop for cxn in (constructions cxn-inventory)
        do (when (eql (phrase-type cxn) 'holophrase)
             (let* ((non-overlapping-meaning-observation (non-overlapping-meaning meaning cxn :nom-observation t))
                    (non-overlapping-meaning-cxn (non-overlapping-meaning meaning cxn :nom-cxn t))
                    (overlapping-meaning-cxn (set-difference (extract-meaning-predicates cxn) non-overlapping-meaning-cxn :test #'equal))
                    (non-overlapping-form-observation (non-overlapping-form utterance cxn :nof-observation t))
                    (non-overlapping-form-cxn (non-overlapping-form utterance cxn :nof-cxn t))
                    (overlapping-form-cxn (set-difference (extract-form-predicates cxn) non-overlapping-form-cxn :test #'equal)))
               (when (and
                      (= 1 (length non-overlapping-meaning-observation))
                      (= 1 (length non-overlapping-meaning-cxn))
                      (= 1 (length non-overlapping-form-observation))
                      (= 1 (length non-overlapping-form-cxn)))
                 (return (values non-overlapping-meaning-observation non-overlapping-meaning-cxn
                                 non-overlapping-form-observation non-overlapping-form-cxn
                                 overlapping-meaning-cxn overlapping-form-cxn
                                 cxn)))))))

;;;;; lex -> item based

(defun diff-non-overlapping-form (observed-form matching-lex-cxns)
  "subtract all lexical forms from the gold standard,
   taking into account possible duplicates in the matching lex cxns
   by always taking the first unification result and removing it
   manually instead of using set-difference."
  ;; !! it is assumed the matching lex cxns are provided
  ;; in the same order as which they occur in the form
  ;; the ordering of the observed form cannot be guaranteed?
  (let ((resulting-form observed-form)
        (lex-unit-names nil))
    (loop for lex-cxn in matching-lex-cxns
          for lex-cxn-form = (extract-form-predicates lex-cxn)
          do (let* ((prev-res-form (copy-object resulting-form))
                    (elm-to-remove
                     (loop for elm in resulting-form
                           when (irl:unify-irl-programs lex-cxn-form (list elm))
                             return elm)))
               (setf resulting-form (remove elm-to-remove resulting-form :test #'equal))
               (let* ((unit-name-predicate (set-difference prev-res-form resulting-form :test #'equal))
                      (unit-name (second (find 'string unit-name-predicate :key #'first))))
                 (push unit-name lex-unit-names))))
    (values (reverse lex-unit-names) resulting-form)))

(defun diff-non-overlapping-meaning (gold-standard-meaning matching-lex-cxns)
  "subtract all lexical meanings (bind statements) from the gold standard
   taking into account possible duplicates in the matching lex cxns
   by always taking the first unification result and removing it
   manually instead of using set-difference."
  ;; !! it is assumed the matching lex cxns are provided
  ;; in the same order as which they occur in the form
  ;; the ordering of the gold standard meaning cannot be guaranteed?
  (let ((resulting-meaning gold-standard-meaning)
        (args nil))
    (loop for lex-cxn in matching-lex-cxns
          for lex-cxn-meaning = (extract-meaning-predicates lex-cxn)
          do (let* ((prev-res-meaning (copy-object resulting-meaning))
                    (elm-to-remove
                     (loop for elm in resulting-meaning
                           when (irl::unify-irl-programs lex-cxn-meaning (list elm))
                             return elm)))
               (setf resulting-meaning (remove elm-to-remove resulting-meaning :test #'equal))
               (let* ((arg-predicate (set-difference prev-res-meaning resulting-meaning :test #'equal))
                      (arg (third (find 'bind arg-predicate :key #'first))))
                 (push arg args))))
    (values (reverse args) resulting-meaning)))

(defun subunit-blocks-for-lex-cxns (lex-cxns lex-subunit-names args categorial-links)
  (loop for lex-cxn in lex-cxns
        for arg in args
        for lex-cxn-unit-name in lex-subunit-names
        for categorial-link in categorial-links
        for lex-slot-lex-class = (cdr categorial-link)
        collect `(,lex-cxn-unit-name
                  (syn-cat (fcg::lex-class ,lex-slot-lex-class))) into contributing-units
        collect `(,lex-cxn-unit-name
                  (args (,arg))
                  --) into conditional-units
        finally (return (values conditional-units contributing-units))))

(defun make-lex-class (cat-name &key add-numeric-tail)
  (intern (symbol-name (funcall (if add-numeric-tail #'make-const #'make-symbol)
                                (upcase (if cat-name cat-name "CAT"))))
          :fcg))

(defun create-categorial-network-links (lex-cxns item-based-name placeholders
                                                 &key item-based-numeric-tail)
  "Creates all CATEGORIAL-NETWORK links for matching lexical cxns using their original lex-class."
  ;; !! This function assumes the lex-cxns are provided
  ;; in the same order as they occur in the utterance
  (loop for lex-cxn in lex-cxns
        for position from 0
        for lex-cxn-lex-class = (lex-class-cxn lex-cxn)
        for placeholder = (when (< 1 (length placeholders))
                            (format nil "-(~a)" (nth position placeholders)))
        for item-slot-lex-class = (make-lex-class (concatenate 'string item-based-name placeholder)
                                                  :add-numeric-tail item-based-numeric-tail)
        collect (cons lex-cxn-lex-class item-slot-lex-class)))

;; holo addition

(defun diff-subset-superset-form (subset-cxn superset-form)
  (set-difference 
   superset-form
   (extract-form-predicates subset-cxn)
   :test #'irl:unify-irl-programs))

(defun find-subset-holophrase-cxn (transient-structure cxn-inventory gold-standard-meaning utterance)
  (loop with ts-form-constraints = (transient-structure-form-constraints transient-structure)
        for cxn in (constructions cxn-inventory)
        for cxn-form-constraints = (extract-form-predicates cxn)
        for cxn-meaning-constraints = (extract-meaning-predicates cxn)
        for superset-form = (form-constraints-with-variables utterance (get-configuration (cxn-inventory cxn) :de-render-mode))
        for non-overlapping-form = (diff-subset-superset-form cxn superset-form)
        for non-overlapping-meaning = (set-difference gold-standard-meaning (extract-meaning-predicates cxn) :test #'irl:unify-irl-programs)
        for cxn-type =  (phrase-type cxn)
        when (and (eql cxn-type 'holophrase)
                  (equal 1 (length non-overlapping-meaning))
                  (equal 1 (length non-overlapping-form))
                  ;; check if all the strings in the form constraints are present in the superset
                  ;; todo: include precedes relations
                  (loop for cxn-fc in cxn-form-constraints
                        always (if (equal (first cxn-fc) 'string)
                                 (find (third cxn-fc) ts-form-constraints :key #'third :test #'equalp)
                                 t)) ;; loop returns true if all are true, the third elem is the string
                  (loop for predicate in cxn-meaning-constraints
                        always (if (equal (first predicate) 'bind)
                                 (find (fourth predicate) gold-standard-meaning :key #'fourth)
                                 (find (first predicate) gold-standard-meaning :key #'first))))
          ;; needs to be a holophrase, the form constraints for string and precedes constraints need to be a subset of the cxn, the meaning constraints need to be a subset too (todo: see if this is really the case in IRL)
          return (values cxn superset-form non-overlapping-form non-overlapping-meaning)))




;; holo deletion
(defun find-superset-holophrase-cxn (transient-structure cxn-inventory gold-standard-meaning utterance)
  ;; todo: there could also be more than one superset cxn!
  (loop with ts-form-constraints = (transient-structure-form-constraints transient-structure)
        for cxn in (constructions cxn-inventory)
        for cxn-form-constraints = (extract-form-predicates cxn)
        for cxn-meaning-constraints = (extract-meaning-predicates cxn)
        for cxn-type =  (phrase-type cxn)
        for non-overlapping-form = (diff-superset-subset-form cxn utterance)
        for non-overlapping-meaning = (set-difference (extract-meaning-predicates cxn) gold-standard-meaning :test #'irl:unify-irl-programs)
        when (and (eql cxn-type 'holophrase)
                  (equal 1 (length non-overlapping-meaning))
                  (equal 1 (length non-overlapping-form))
                  ;; check if all the strings in the form constraints are present in the superset
                  ;; precedes relations are not important
                  (loop for ts-fc in ts-form-constraints
                        always (find (third ts-fc) cxn-form-constraints :key #'third :test #'equalp)) ;; loop returns true if all are true, the third elem is the string
                  (loop for predicate in gold-standard-meaning
                        always (if (equal (first predicate) 'bind)
                                 (find (fourth predicate) cxn-meaning-constraints :key #'fourth)
                                 (find (first predicate) cxn-meaning-constraints :key #'first))))
          ;; needs to be a holophrase, the form constraints for string and precedes constraints need to be a subset of the cxn, the meaning constraints need to be a subset too (todo: see if this is really the case in IRL)
          return (values cxn non-overlapping-form non-overlapping-meaning)))

;;;
(defun item-based-number-of-slots (cxn)
  (when (eql (get-cxn-type cxn) 'item-based)
    (1- (length (contributing-part cxn)))))

(defun get-strings-from-root (node)
  (form-predicates-with-variables
   (extract-string
    (get-root
     (if (find 'fcg::second-merge-failed (fcg::statuses node))
       (car-first-merge-structure (cipn-car node))
       (left-pole-structure
        (car-resulting-cfs
         (cipn-car node))))))))

(defun get-strings-from-root (node)
  (form-predicates-with-variables
   (extract-string
    (get-root
     (left-pole-structure
      (car-resulting-cfs
       (cipn-car node)))))))

(defun subunit-blocks-for-lex-cxns (lex-cxns lex-subunit-names args categorial-links)
  (loop for lex-cxn in lex-cxns
        for arg in args
        for lex-cxn-unit-name in lex-subunit-names
        for categorial-link in categorial-links
        for lex-slot-lex-class = (cdr categorial-link)
        collect `(,lex-cxn-unit-name
                  (syn-cat (lex-class ,lex-slot-lex-class))) into contributing-units
        collect `(,lex-cxn-unit-name
                  (args (,arg))
                  --) into conditional-units
        finally (return (values conditional-units contributing-units))))

(defun subunit-blocks-for-lex-cxns (lex-cxns lex-subunit-names args categorial-links)
  (loop for lex-cxn in lex-cxns
        for arg in args
        for lex-cxn-unit-name in lex-subunit-names
        for categorial-link in categorial-links
        for lex-slot-lex-class = (cdr categorial-link)
        collect `(,lex-cxn-unit-name
                  (syn-cat (lex-class ,lex-slot-lex-class)))
          into contributing-units
        collect `(,lex-cxn-unit-name
                  (args (,arg))
                  --)
          into conditional-units
        finally (return (values conditional-units contributing-units))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extracting form and meaning from fcg-constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric extract-meaning-predicates (object))

(defmethod extract-meaning-predicates ((cxn fcg-construction))
  (append (mappend #'extract-meaning-predicates (conditional-part cxn))
          (mappend #'extract-meaning-predicates (contributing-part cxn))))

(defmethod extract-meaning-predicates ((unit conditional-unit))
  (append (extract-meaning-predicates (comprehension-lock unit))
          (extract-meaning-predicates (formulation-lock unit))))

(defmethod extract-meaning-predicates ((unit contributing-unit))
  (extract-meaning-predicates (fcg::unit-structure unit)))

(defmethod extract-meaning-predicates ((unit-body list))
  (append (find-feature-value 'meaning unit-body)
          (find-hashed-feature-value 'meaning unit-body)))

;; (extract-meaning-predicates (first (constructions *fcg-constructions*)))

(defgeneric extract-form-predicates (object))

(defmethod extract-form-predicates ((cxn fcg-construction))
  (append (mappend #'extract-form-predicates (conditional-part cxn))
          (mappend #'extract-form-predicates (contributing-part cxn))))

(defmethod extract-form-predicates ((unit conditional-unit))
  (append (extract-form-predicates (comprehension-lock unit))
          (extract-form-predicates (formulation-lock unit))))

(defmethod extract-form-predicates ((unit contributing-unit))
  (extract-form-predicates (fcg::unit-structure unit)))

(defmethod extract-form-predicates ((unit-body list))
  (append (find-feature-value 'form unit-body)
          (find-hashed-feature-value 'form unit-body)))

;; (extract-form-predicates (first (constructions *fcg-constructions*)))

(defun find-feature-value (feature unit-body)
  (loop for feature-value in unit-body
        when (equal (feature-name feature-value) feature)
        return  (second feature-value)))

(defun find-hashed-feature-value (feature unit-body)
  (loop for feature-value in unit-body
        when (and (equal (first feature-value) 'HASH)
                  (equal (second feature-value) feature))
        return (third feature-value)))
