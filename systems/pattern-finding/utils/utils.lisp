(in-package :pattern-finding)

;;;;;
;; Accessors and Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-cxn-type (cxn)
  (attr-val cxn :cxn-type))

(defun get-cxn-score (cxn)
  (attr-val cxn :score))

(defun non-zero-cxn-p (cxn)
  (> (attr-val cxn :score) 0))

(defun routine-cxn-p (cxn)
  (eql (attr-val cxn :label) 'fcg::routine))

(defun holistic-cxn-p (cxn)
  (eql (attr-val cxn :cxn-type) 'holistic))

(defun holophrase-cxn-p (cxn)
  (eql (attr-val cxn :is-holophrase) t))

(defun routine-non-zero-cxns (agent)
  (remove-if-not #'non-zero-cxn-p
                 (remove-if-not #'routine-cxn-p
                                (constructions (grammar agent)))))

(defun get-cxns-of-type (agent type)
  (let ((found-cxns (if (eql type 'all)
                      (constructions-list (grammar agent))
                      (find-all type (constructions-list (grammar agent))
                                :key #'get-cxn-type))))
    (loop for cxn in found-cxns
          when (non-zero-cxn-p cxn)
          collect cxn)))

(defun item-based-number-of-slots (cxn)
  (when (eql (get-cxn-type cxn) 'item-based)
    (if (routine-cxn-p cxn)
      (1- (length (conditional-part cxn)))
      (1- (length (contributing-part cxn))))))

(defun count-non-zero-holophrases (grammar)
  (count-if #'(lambda (cxn)
                (and (holophrase-cxn-p cxn)
                     (routine-cxn-p cxn)
                     (non-zero-cxn-p cxn)))
            (constructions grammar)))


;;;;;
;; Search tree utils
;;;;;;;;;;;;;;;;;;;;

(defun initial-transient-structure (node)
  (if (find 'fcg::initial (statuses node))
    (car-source-cfs (cipn-car node))
    (car-source-cfs (cipn-car (last-elt (all-parents node))))))

(defun initial-node-p (node)
  "return t if node is initial node"
  (null (all-parents node)))

(defun all-leaf-nodes (cip)
  (remove nil (traverse-depth-first cip :collect-fn #'(lambda (node) (unless (children node) node)))))

(defun ignore-initial-nodes (cip-nodes)
  (loop for node in cip-nodes
        unless (find 'fcg::initial (statuses node))
        collect node))


;;;;;
;; Make cxn name
;;;;;;;;;;;;;;;;;;;;

(defconstant +placeholder-vars+
  '("?X" "?Y" "?Z" "?A" "?B" "?C"
    "?D" "?E" "?F" "?G" "?H" "?I"
    "?J" "?K" "?L" "?M" "?N" "?O"
    "?P" "?Q" "?R" "?S" "?T" "?U"
    "?V" "?W"))

(defun variablify-missing-form-strings (form-constraints)
  "create X Y Z etc variables by checking which strings are missing in meets constraints.
   return a list of placeholder bindings in the form of string predicates"
  (loop with string-constraints = (find-all 'string form-constraints :key #'first)
        with new-string-constraints = nil
        with meets-constraints = (set-difference form-constraints string-constraints)
        with placeholder-index = 0
        for meets-constraint in meets-constraints
        for first-word-var = (second meets-constraint)
        for second-word-var = (third meets-constraint)
        unless (or (find first-word-var string-constraints :key #'second)
                   (find first-word-var new-string-constraints :key #'second))
        do (push (list 'string first-word-var (nth placeholder-index +placeholder-vars+)) new-string-constraints)
           (incf placeholder-index)
        ;do (progn (push `(string ,first-word-var ,(nth placeholder-index +placeholder-vars+)) new-string-constraints)
        ;     (incf placeholder-index))
        unless (or (find second-word-var string-constraints :key #'second)
                   (find second-word-var new-string-constraints :key #'second))
        do (push (list 'string second-word-var (nth placeholder-index +placeholder-vars+)) new-string-constraints)
           (incf placeholder-index)
        finally (return new-string-constraints)))

(defgeneric make-cxn-name (thing cxn-inventory &key add-cxn-suffix add-numeric-tail))

(defmethod make-cxn-name ((string string) (cxn-inventory fcg-construction-set)
                          &key (add-cxn-suffix t) (add-numeric-tail nil))
  "Transform an utterance into a suitable construction name"
  (declare (ignore cxn-inventory))
  (let ((name-string
         (substitute #\- #\Space
                     (upcase
                      (if add-cxn-suffix
                        (string-append string "-cxn")
                        string)))))
    (if add-numeric-tail
      (make-id name-string)
      (intern name-string))))

(defmethod make-cxn-name ((form-constraints list) (cxn-inventory fcg-construction-set)
                          &key (add-cxn-suffix t) (add-numeric-tail nil))
  "Transform a list of form constraints into a suitable construction name"
  (let ((new-string-constraints (variablify-missing-form-strings form-constraints)))
    (make-cxn-name (format nil "~{~a~^-~}"
                           (render (append form-constraints new-string-constraints)
                                   (get-configuration cxn-inventory :render-mode)))
                   cxn-inventory :add-cxn-suffix add-cxn-suffix :add-numeric-tail add-numeric-tail)))

;;;;;
;; Make unit name
;;;;;;;;;;;;;;;;;;;;

(defgeneric make-unit-name (thing cxn-inventory))

(defmethod make-unit-name ((string string) (cxn-inventory fcg-construction-set))
  "Transform an utterance into a suitable construction name"
  (declare (ignore cxn-inventory))
  (variablify (intern (string-append (upcase string) "-UNIT"))))

(defmethod make-unit-name ((form-constraints list) (cxn-inventory fcg-construction-set))
  "Transform a list of form constraints into a suitable construction name"
  (let ((new-string-constraints (variablify-missing-form-strings form-constraints)))
    (make-unit-name (format nil "~{~a~^-~}"
                           (render (append form-constraints new-string-constraints)
                                   (get-configuration cxn-inventory :render-mode)))
                   cxn-inventory)))


;;;;;
;; Make lex class
;;;;;;;;;;;;;;;;;;;;

(defun replace-special-initial-chars (string)
  (if (member (subseq string 0 1) '("?" "!" "\"") :test #'string=)
    (string-append "-" string)
    string))
 
(defun make-lex-class (cat-name &key add-numeric-tail trim-cxn-suffix)
  (let* ((name-string
          (replace-special-initial-chars
           (if (equal (type-of cat-name) 'SYMBOL)
             (string-downcase (symbol-name cat-name))
             (string-downcase cat-name))))
         (cat-name
          (if trim-cxn-suffix
            (fcg::replace-all name-string "-cxn" "")
            name-string)))
    (intern
     (string-downcase
      (symbol-name
       (funcall
        (if add-numeric-tail #'make-const #'make-symbol)
        (upcase
         (if cat-name cat-name "CAT")))))
     :pattern-finding)))


;;;;;
;; Extracting lex classes from cxns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-lex-class-holistic-cxn (cxn)
  "Extract the lex class from a holistic cxn.
   Works for both routine and meta cxns."
  (let* ((unit-to-search
          (if (routine-cxn-p cxn)
            (fcg::unit-structure (first (contributing-part cxn)))
            (comprehension-lock (first (conditional-part cxn)))))
         (syn-cat (find 'syn-cat unit-to-search :key #'first)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))

(defun extract-lex-class-slot-item-based-cxn (cxn)
  "Extracts the lex classes from the slots of an item-based cxn.
   Works for both routine and meta cxns."
  (let* ((units-to-search
          (if (routine-cxn-p cxn)
            (loop for unit in (conditional-part cxn)
                  when (find 'syn-cat (comprehension-lock unit) :key #'first)
                  collect (comprehension-lock unit))
            (loop for unit in (contributing-part cxn)
                  unless (find 'subunits (fcg::unit-structure unit) :key #'first)
                  collect (fcg::unit-structure unit)))))
    (loop for unit in units-to-search
          for syn-cat = (find 'syn-cat unit :key #'first)
          for lex-class = (second (find 'lex-class (rest syn-cat) :key #'first))
          collect lex-class)))

(defun extract-lex-class-item-based-cxn (cxn)
  (let* ((unit-to-search
          (loop for unit in (contributing-part cxn)
                when (find 'subunits (fcg::unit-structure unit) :key #'first)
                return (fcg::unit-structure unit)))
         (syn-cat (find 'syn-cat unit-to-search :key #'first)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))

(defun lex-class (unit)
  (let* ((syn-cat (find 'syn-cat (unit-body unit) :key #'first))
         (lex-class (find 'lex-class (second syn-cat) :key #'first)))    
    (when lex-class
      (second lex-class))))

(defun extract-contributing-lex-class (cxn)
  "return the lex-class on the contributing part of a cxn
   works for both holistic and item-based cxns"
  (if (holistic-cxn-p cxn)
    (extract-lex-class-holistic-cxn cxn)
    (extract-lex-class-item-based-cxn cxn)))

(defun extract-conditional-lex-classes (cxn)
  "return the lex classes on the conditional part of a cxn
   works only for item-based cxns"
  (unless (holistic-cxn-p cxn)
    (extract-lex-class-slot-item-based-cxn cxn)))

;;;;;
;; Extracting args from cxns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-args-holistic-cxn (cxn)
  "Extract the form-args and meaning-args from
   a holistic cxn. Works for both routine and
   meta cxns."
  (let ((holistic-unit-structure
         (fcg::unit-structure
          (first
           (if (routine-cxn-p cxn)
             (contributing-part cxn)
             (conditional-part cxn))))))
    (list (second (find 'form-args holistic-unit-structure :key #'first))
          (second (find 'meaning-args holistic-unit-structure :key #'first)))))

(defun extract-args-item-based-cxn (cxn)
  "Extract the top-lvl form args, top-lvl meaning args,
   slot form args, and slot meaning args of an item-based
   cxn. Works for both routine and meta cxns."
  (if (routine-cxn-p cxn)
    (let* ((slot-args
            (loop for unit in (conditional-part cxn)
                  for meaning-args = (second (find 'meaning-args (formulation-lock unit) :key #'first))
                  for form-args = (second (find 'form-args (comprehension-lock unit) :key #'first))
                  when (and meaning-args form-args)
                    append form-args into all-form-args
                    and append meaning-args into all-meaning-args
                  finally (return (list all-form-args all-meaning-args))))
           (top-lvl-args
            (loop for unit in (contributing-part cxn)
                  for meaning-args = (second (find 'meaning-args (fcg::unit-structure unit) :key #'first))
                  for form-args = (second (find 'form-args (fcg::unit-structure unit) :key #'first))
                  when (and form-args meaning-args)
                    return (list form-args meaning-args))))
      (list top-lvl-args slot-args))
    (let ((slot-args
           (loop for unit in (contributing-part cxn)
                 for unit-structure = (fcg::unit-structure unit)
                 unless (find 'subunits unit-structure :key #'first)
                   append (find 'form-args unit-structure :key #'first) into all-form-args
                   and append (find 'meaning-args unit-structure :key #'first) into all-meaning-args
                 finally (return (list all-form-args all-meaning-args))))
          (top-lvl-args
           (loop for unit in (contributing-part cxn)
                 for unit-structure = (fcg::unit-structure unit)
                 when (find 'subunits unit-structure :key #'first)
                 return (list (second (find 'form-args unit-structure :key #'first))
                              (second (find 'meaning-args unit-structure :key #'first))))))
      (list top-lvl-args slot-args))))


;;;;;
;; Find identical holistic cxn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun equivalent-networks-and-args? (network cxn-network args cxn-args)
  (equivalent-irl-programs?
   (append network `((args ,@args)))
   (append cxn-network `((args ,@cxn-args)))))

(defun identical-holistic-cxn-p (form meaning form-args meaning-args cxn)
  (let ((cxn-args (extract-args-holistic-cxn cxn)))
    (and (equivalent-irl-programs? form (extract-form-predicates cxn))
         (equivalent-irl-programs? meaning (extract-meaning-predicates cxn))
         (length= form-args (first cxn-args))
         (length= meaning-args (second cxn-args))
         (equivalent-networks-and-args? form (extract-form-predicates cxn) form-args (first cxn-args))
         (equivalent-networks-and-args? meaning (extract-meaning-predicates cxn) meaning-args (second cxn-args)))))

(defun find-identical-holistic-cxn (form meaning form-args meaning-args cxn-inventory)
  "Find a routine holistic cxn that is identical to the given form, meaning, and args"
  (let ((candidate-cxns
         (remove-if-not #'non-zero-cxn-p
                        (remove-if-not #'routine-cxn-p
                                       (remove-if-not #'holistic-cxn-p
                                                      (constructions cxn-inventory))))))
    (loop for cxn in candidate-cxns
          when (identical-holistic-cxn-p form meaning form-args meaning-args cxn)
          return cxn)))

;;;;;
;; Find identical item-based cxn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun identical-item-based-cxn-p (form meaning top-lvl-form-args top-lvl-meaning-args
                                   slot-form-args slot-meaning-args cxn)
  (destructuring-bind (top-lvl-args slot-args) (extract-args-item-based-cxn cxn)
    (and (equivalent-irl-programs? form (extract-form-predicates cxn))
         (equivalent-irl-programs? meaning (extract-meaning-predicates cxn))
         (length= top-lvl-form-args (first top-lvl-args))
         (length= top-lvl-meaning-args (second top-lvl-args))
         (length= slot-form-args (first slot-args))
         (length= slot-meaning-args (second slot-args))
         (equivalent-networks-and-args? form (extract-form-predicates cxn) top-lvl-form-args (first top-lvl-args))
         (equivalent-networks-and-args? meaning (extract-meaning-predicates cxn) top-lvl-meaning-args (second top-lvl-args))
         (equivalent-networks-and-args? form (extract-form-predicates cxn) slot-form-args (first slot-args))
         (equivalent-networks-and-args? meaning (extract-meaning-predicates cxn) slot-meaning-args (second slot-args)))))
                      

(defun find-identical-item-based-cxn (form meaning top-lvl-form-args top-lvl-meaning-args slot-form-args slot-meaning-args cxn-inventory)
  "Find a routine item-based cxn that is identical to the given form, meaning, and args"
  (let ((candidate-cxns
         (remove-if-not #'non-zero-cxn-p
                        (remove-if-not #'routine-cxn-p
                                       (remove-if #'holistic-cxn-p
                                                  (constructions cxn-inventory))))))
    (loop for cxn in candidate-cxns
          when (identical-item-based-cxn-p form meaning top-lvl-form-args top-lvl-meaning-args
                                           slot-form-args slot-meaning-args cxn)
          return cxn)))


;;;;;
;; Get the alter-ego cxn
;;;;;;;;;;;;;;;;;;;;;;;;

(defun alter-ego-cxn (original-cxn cxn-inventory)
  "Given a routine cxn, return its meta counterpart
   or vice-versa."
  (when (attr-val original-cxn :bare-cxn-name)
    (loop for cxn in (constructions cxn-inventory)
          when (and (attr-val cxn :bare-cxn-name)
                    (eq (attr-val cxn :bare-cxn-name)
                        (attr-val original-cxn :bare-cxn-name))
                    (not (eql (name cxn) (name original-cxn))))
          do (return cxn))))

;;;;;
;; Enable/Disable meta layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun disable-meta-layer-configuration (cxn-inventory)
  (set-configuration cxn-inventory :category-linking-mode :categories-exist)
  (set-configuration cxn-inventory :update-categorial-links nil)
  (set-configuration cxn-inventory :use-meta-layer nil)
  (set-configuration cxn-inventory :consolidate-repairs nil))

(defun enable-meta-layer-configuration (cxn-inventory)
  (set-configuration cxn-inventory :parse-goal-tests '(:no-strings-in-root
                                                       :no-applicable-cxns
                                                       :connected-semantic-network
                                                       :connected-structure
                                                       :non-gold-standard-meaning))
  (set-configuration cxn-inventory :category-linking-mode :neighbours)
  (set-configuration cxn-inventory :update-categorial-links t)
  (set-configuration cxn-inventory :use-meta-layer t)
  (set-configuration cxn-inventory :consolidate-repairs t))

(defun disable-meta-layer-configuration-item-based-first (cxn-inventory)
  (set-configuration cxn-inventory :parse-goal-tests '(:no-applicable-cxns))
  (set-configuration cxn-inventory :parse-order '(meta-only))
  (set-configuration cxn-inventory :category-linking-mode :categories-exist)
  (set-configuration cxn-inventory :hash-mode :hash-string-meaning-lex-id) ; excludes nil hashed cxns (e.g. ?x-?y)
  (set-configuration cxn-inventory :max-nr-of-nodes 250)
  (set-configuration cxn-inventory :update-categorial-links nil)
  (set-configuration cxn-inventory :use-meta-layer nil)
  (set-configuration cxn-inventory :consolidate-repairs nil))

(defun enable-meta-layer-configuration-item-based-first (cxn-inventory)
  (set-configuration cxn-inventory :parse-goal-tests '(:no-strings-in-root
                                                       :no-applicable-cxns
                                                       :connected-semantic-network
                                                       :connected-structure
                                                       :non-gold-standard-meaning))
  (set-configuration cxn-inventory :hash-mode :hash-string-meaning)
  (set-configuration cxn-inventory :parse-order '(routine))
  (set-configuration cxn-inventory :max-nr-of-nodes (get-configuration cxn-inventory :original-max-nr-of-nodes))
  (set-configuration cxn-inventory :category-linking-mode :neighbours)
  (set-configuration cxn-inventory :update-categorial-links t)
  (set-configuration cxn-inventory :use-meta-layer t)
  (set-configuration cxn-inventory :consolidate-repairs t))
                 
;;;;;
;; Unit Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-child-units (units)
  (loop for unit in units
        when (member 'used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
        collect unit))

(defun remove-child-units (units)
  (loop for unit in units
        unless (member 'pf::used-as-slot-filler (unit-feature-value unit 'fcg:footprints))
        collect unit))

(defun extract-form-from-unit-tree (top-unit transient-structure)
  (let ((top-unit (find (unit-name top-unit) (left-pole-structure transient-structure):key #'first)))
    (extract-forms 
     (cons top-unit
           (all-subunits
            top-unit
            (left-pole-structure transient-structure))))))

;;;;;
;; Variablify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fresh-variables (predicates)
  (let* ((all-variables (find-all-anywhere-if #'variable-p predicates))
         (unique-variables (remove-duplicates all-variables))
         (renamings (loop for var in unique-variables
                          for base-name = (get-base-name var)
                          collect (cons var (make-var base-name)))))
    (values (substitute-bindings renamings predicates)
            renamings)))

(defun variablify-form-constraints-with-constants (form-constraints-with-constants)
  (loop for form-constraint in form-constraints-with-constants
        for constraint = (first form-constraint)
        collect (cons constraint
                      (case constraint
                        (string (list (variablify (second form-constraint))
                                      (third form-constraint)))
                        (meets (mapcar #'variablify (rest form-constraint)))))))

(defun devariablify (var)
  (intern (get-base-name var :remove-numeric-tail nil)))


;;;;;
;; Hash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun form-predicates->hash-string (form-predicates)
  ;; the last string predicate
  (third
   (last-elt
    (find-all 'string form-predicates
              :key #'first))))

(defgeneric meaning-predicates->hash-meaning (meaning-predicates meaning-representation))

(defmethod meaning-predicates->hash-meaning (meaning-predicates (meaning-representation (eql :irl)))
  (let* ((all-primitives
          (mapcar #'first meaning-predicates))
         (all-primitives-but-bind
          (remove 'bind all-primitives))
         (target-variable
          (get-target-var meaning-predicates)))
    ;; if there are only bind statements
    (if (null all-primitives-but-bind)
      ;; take the last element of the first binding
      (last-elt (first (find-all 'bind meaning-predicates :key #'first)))
      ;; otherwise, take the primitive that holds the target var
      (first (find target-variable meaning-predicates :key #'second)))))

(defun hash-observation (form-constraints meaning-predicates)
  ;; extract string predicates + predicate names
  (let ((meaning-predicates
         (loop for meaning in meaning-predicates
               collect (if (and (= 4 (length meaning))
                                (eql 'bind (first meaning)))
                         (fourth meaning)
                         (first meaning))))
        (form-predicates
         (mapcar #'third (find-all 'string form-constraints :key #'first))))
    (append form-predicates meaning-predicates)))
                            
(defun constructions-for-anti-unification-hashed (form-constraints meaning-predicates cxn-inventory)
  (remove-duplicates
   (append
    (loop for hash in (hash-observation form-constraints meaning-predicates)
          append (gethash hash (constructions-hash-table cxn-inventory)))
    (gethash nil (constructions-hash-table cxn-inventory)))))

;;;;;
;; Partial Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun form-predicates-in-root (cipn)
  (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car cipn)))) 'form))

(defun sort-cipns-by-coverage-and-nr-of-applied-cxns (cipn-1 cipn-2)
  "Predicate should return true if and only if the first argument
   is strictly less than the second (in some appropriate sense).
   If the first argument is greater than or equal to the second
   (in the appropriate sense), then the predicate should return false."
  (let ((cipn-1-form-in-root (length (form-predicates-in-root cipn-1)))
        (cipn-2-form-in-root (length (form-predicates-in-root cipn-2)))
        (cipn-1-applied-cxns (length (applied-constructions cipn-1)))
        (cipn-2-applied-cxns (length (applied-constructions cipn-2))))
    (if (= cipn-1-form-in-root cipn-2-form-in-root)
      (< cipn-1-applied-cxns cipn-2-applied-cxns)
      (< cipn-1-form-in-root cipn-2-form-in-root))))

(defun extract-args-from-resulting-unit (unit)
  (and (find 'form-args (rest unit) :key #'first))
       (find 'meaning-args (rest unit) :key #'first))

(defun discard-cipns-with-incompatible-meanings-and-args (cipns meanings gold-standard-meaning)
  (loop for cipn in cipns
        for candidate-meaning in meanings
        for ts-units = (left-pole-structure (car-resulting-cfs (cipn-car cipn)))
        for root-unit = (get-root ts-units)
        for top-lvl-units = (remove-child-units (remove root-unit ts-units))
        for bindings = (irl::embedding candidate-meaning gold-standard-meaning)
        when (and bindings
                  ;; why is this???
                  (loop for unit in top-lvl-units
                        always (extract-args-from-resulting-unit unit)))
        collect cipn))

(defun compatible-cipn-with-largest-coverage (form-constraints gold-standard-meaning cxn-inventory)
  (with-disabled-monitor-notifications
    (multiple-value-bind (meanings cip-nodes) (comprehend-all form-constraints :cxn-inventory cxn-inventory)
      (let* ((cip-nodes-with-meaning
              (loop for meaning in meanings
                    for cipn in cip-nodes
                    when meaning collect cipn))
             (compatible-cip-nodes
              (discard-cipns-with-incompatible-meanings-and-args cip-nodes-with-meaning
                                                                 meanings gold-standard-meaning)))
        (when compatible-cip-nodes
          (first (sort compatible-cip-nodes #'sort-cipns-by-coverage-and-nr-of-applied-cxns)))))))

(defun best-partial-analysis-cipn-with-meta-cxns (form-constraints gold-standard-meaning cxn-inventory)
  (when (constructions-list cxn-inventory)
    (let (best-cipn)
      (disable-meta-layer-configuration-item-based-first cxn-inventory)
      (setf best-cipn (compatible-cipn-with-largest-coverage form-constraints gold-standard-meaning cxn-inventory))
      (enable-meta-layer-configuration-item-based-first cxn-inventory)
      best-cipn)))

(defun best-partial-analysis-cipn-with-routine-cxns (form-constraints gold-standard-meaning cxn-inventory)
  (when (constructions-list cxn-inventory)
    (let (best-cipn)
      (disable-meta-layer-configuration cxn-inventory)
      (setf best-cipn (compatible-cipn-with-largest-coverage form-constraints gold-standard-meaning cxn-inventory))
      (enable-meta-layer-configuration cxn-inventory)
      best-cipn)))

(defun anti-unify-partial-analysis-with-observation (observation-form observation-meaning partial-analysis-cipn)
  ;; the generalisation is identical to the meaning/form of the cipn,
  ;; the pattern delta is empty
  ;; and the source delta contains the material for the new cxn to be learned
  (let* ((cipn-meaning (fcg-extract-meanings partial-analysis-cipn))
         ;; cipn-form has the same variables as the observation-form
         ;; --> these variables do not occur in the bindings of the anti-unification...
         ;; --> make fresh variables and store the mappings from the original
         ;;     constants to the fresh variables
         (cipn-form-and-variable-renamings
          (multiple-value-list
           (fresh-variables
            (variablify-form-constraints-with-constants
             (loop for unit in (fcg-get-transient-unit-structure partial-analysis-cipn)
                   unless (eql (unit-name unit) 'fcg::root)
                     append (unit-feature-value unit 'form))))))
         (cipn-form (first cipn-form-and-variable-renamings))
         (form-const-renamings
          (loop for (var . fresh-var) in (second cipn-form-and-variable-renamings)
                collect (cons (devariablify var) fresh-var)))
         (meaning-a-u (first (anti-unify-predicate-network cipn-meaning observation-meaning)))
         (form-a-u (first (anti-unify-predicate-network cipn-form observation-form))))
    (unless (or (null (source-delta meaning-a-u))
                (null (source-delta form-a-u)))
      ;; store the renamings in the cipn
      (set-data partial-analysis-cipn :form-const-renamings form-const-renamings)
      ;; nazi checks
      (assert (and (null (pattern-delta meaning-a-u))
                   (null (pattern-delta form-a-u))
                   (equivalent-irl-programs? (generalisation meaning-a-u) cipn-meaning)
                   (equivalent-irl-programs? (generalisation form-a-u) cipn-form)))
      ;; return AU results
      (values form-a-u meaning-a-u))))


;;;;;
;; Sort meets constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-boundary-units (form-constraints)
  "returns the leftmost and rightmost unit
   based on meets constraints"
  (let* ((left-units
          (loop for fc in form-constraints
                when (eql 'meets (first fc))
                collect (second fc)))
         (right-units
          (loop for fc in form-constraints
                when (eql 'meets (first fc))
                collect (third fc)))
         (left-most-unit (first (set-difference left-units right-units)))
         (right-most-unit (first (set-difference right-units left-units))))
    (if (and left-most-unit right-most-unit)
      (list left-most-unit right-most-unit)
      (when (and (= (length form-constraints) 1)
                 (eql 'string (first (first form-constraints))))
        (list (second (first form-constraints))
              (second (first form-constraints)))))))


(defun sort-meets-constraints (meets-constraints)
  "return the sorted list of meets constraints"
  (let* ((begin-var (first (get-boundary-units meets-constraints)))
         (first-predicate (find begin-var meets-constraints :key #'second)))
    (loop with next-predicate = first-predicate
          with resulting-list = nil
          for next-var = (third next-predicate)
          while next-predicate
          do (push next-predicate resulting-list)
          (setf next-predicate (find next-var meets-constraints :key #'second))
          finally (return (reverse resulting-list)))))


(defun continuous-meets-p (form-constraints)
  "check if within a holistic chunk, all form strings are connected"
  (let* ((left-units
          (loop for fc in form-constraints
                when (equal 'meets (first fc))
                  collect (second fc)))
         (right-units
          (loop for fc in form-constraints
                when (equal 'meets (first fc))
                  collect (third fc)))
         (string-units
          (loop for fc in form-constraints
                when (equal 'string (first fc))
                  collect (second fc)))
         (left-most-diff (set-difference left-units right-units))
         (right-most-diff (set-difference right-units left-units))
         (all-units (remove-duplicates (append left-units right-units)))
         (string-meets-diff (set-difference string-units all-units))
         (meets-string-diff (set-difference all-units string-units)))
    
    (if (and left-most-diff right-most-diff)
      (and (= 1 (length left-most-diff))
           (= 1 (length right-most-diff))
           (not string-meets-diff)
           (not meets-string-diff)
           (get-boundary-units form-constraints))
      (get-boundary-units form-constraints))))

;;;;;
;; Input Processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun form-constraints-with-variables (utterance de-render-mode)
  "Extract form constraints from utterance in the format they would appear in a construction."
  (let ((form-constraints-with-constants
         (remove 'sequence
                 (extract-forms (left-pole-structure
                                 (de-render utterance de-render-mode)))
                 :key #'first)))
    (variablify-form-constraints-with-constants form-constraints-with-constants)))

(defgeneric meaning-predicates-with-variables (meaning mode))

(defmethod meaning-predicates-with-variables (meaning (mode (eql :irl)))
  "Transform meaning network with constants to meaning network with variables."
  (loop for predicate in meaning
        collect (if (equal (first predicate) 'bind)
                  (list (first predicate)
                        (second predicate)
                        (variablify (third predicate))
                        (fourth predicate))
                  (cons (first predicate)
                        (mapcar #'variablify (rest predicate))))))

(defmethod meaning-predicates-with-variables (meaning (mode (eql :geo)))
  "Transform meaning network with constants to meaning network with variables."
  (loop for predicate in meaning
        collect (cons (first predicate)
                      (mapcar #'variablify (rest predicate)))))

(defmethod meaning-predicates-with-variables (meaning (mode (eql :amr)))
  "Transform meaning network with constants to meaning network with variables."
  (amr:variablify-amr-network meaning))

;;;;;
;; Equivalent Meaning Networks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric equivalent-meaning-networks (m1 m2 mode))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :irl)))
  (equivalent-irl-programs? m1 m2))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :amr)))
  (amr::equivalent-amr-predicate-networks m1 m2))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :geo)))
  (amr::equivalent-amr-predicate-networks m1 m2))
          