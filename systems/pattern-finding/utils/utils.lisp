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
                                (constructions (construction-inventory agent)))))

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
        do (progn (push `(string ,first-word-var ,(nth placeholder-index +placeholder-vars+)) new-string-constraints)
             (incf placeholder-index))
        unless (or (find second-word-var string-constraints :key #'second)
                   (find second-word-var new-string-constraints :key #'second))
        do (progn (push `(string ,second-word-var ,(nth placeholder-index +placeholder-vars+)) new-string-constraints)
             (incf placeholder-index))
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
  (let* ((unit-to-search
          (if (routine-cxn-p cxn)
            (loop for unit in (conditional-part cxn)
                  when (find 'syn-cat (comprehension-lock unit) :key #'first)
                  return (comprehension-lock unit))
            (loop for unit in (contributing-part cxn)
                  unless (find 'subunits (fcg::unit-structure unit) :key #'first)
                  return (fcg::unit-structure unit))))
         (syn-cat (find 'syn-cat unit-to-search :key #'first)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))

(defun extract-lex-class-item-based-cxn (cxn)
  (let* ((unit-to-search
          (if (routine-cxn-p cxn)
            (fcg::unit-structure (first (contributing-part cxn)))
            (loop for unit in (contributing-part cxn)
                  when (find 'subunits (fcg::unit-structure unit) :key #'first)
                  return (fcg::unit-structure unit))))
         (syn-cat (find 'syn-cat unit-to-search :key #'first)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))


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
                  return (list form-args meaning-args)))
           (contributing-unit-structure
            (fcg::unit-structure (first (contributing-part cxn))))
           (top-lvl-args
            (list (second (find 'form-args contributing-unit-structure :key #'first))
                  (second (find 'meaning-args contributing-unit-structure :key #'first)))))
      (list top-lvl-args slot-args))
    (let ((slot-args
           (loop for unit in (contributing-part cxn)
                 for unit-structure = (fcg::unit-structure unit)
                 unless (find 'subunits unit-structure :key #'first)
                 return (list (second (find 'form-args unit-structure :key #'first))
                              (second (find 'meaning-args unit-structure :key #'first)))))
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

(defun identical-item-based-cxn-p (form meaning top-lvl-form-args top-lvl-meaning-args slot-form-args slot-meaning-args cxn)
  (destructuring-bind (top-lvl-args slot-args) (extract-args-item-based-cxn cxn)
    (let ((equivalent-form-meaning-and-top-lvl-args
           (and (equivalent-irl-programs? form (extract-form-predicates cxn))
                (equivalent-irl-programs? meaning (extract-meaning-predicates cxn))
                (length= top-lvl-form-args (first top-lvl-args))
                (length= top-lvl-meaning-args (second top-lvl-args))
                (length= slot-form-args (first slot-args))
                (length= slot-meaning-args (second slot-args))
                (equivalent-networks-and-args? form (extract-form-predicates cxn) top-lvl-form-args (first top-lvl-args))
                (equivalent-networks-and-args? meaning (extract-meaning-predicates cxn) top-lvl-meaning-args (second top-lvl-args)))))
      (when equivalent-form-meaning-and-top-lvl-args
        (let (reordered-slot-form-args reordered-slot-meaning-args)
          (loop for candidate-args in (reverse (permutations-of-length slot-form-args (length slot-form-args)))
                when (equivalent-networks-and-args? form (extract-form-predicates cxn) candidate-args (first slot-args))
                do (setf reordered-slot-form-args candidate-args) and return nil)
          (loop for candidate-args in (reverse (permutations-of-length slot-meaning-args (length slot-meaning-args)))
                when (equivalent-networks-and-args? meaning (extract-meaning-predicates cxn) candidate-args (second slot-args))
                do (setf reordered-slot-meaning-args candidate-args) and return nil)
          (when (and reordered-slot-form-args reordered-slot-meaning-args)
            (values t reordered-slot-form-args reordered-slot-meaning-args)))))))

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
                 





























(defun sort-cxns-by-form-string (cxns-to-sort utterance cxn-inventory)
  "sorts lexical cxns by matching their form strings to the utterance. handles duplicate cxns in one utterance."
  ;; warning, this function depends on space separation without further punctuation!
  ;; todo: replace by looking up meets constraints!
  (if (< (length cxns-to-sort) 2)
    cxns-to-sort
    (let ((resulting-list (make-list (length utterance))))
      (loop for cxn-obj in cxns-to-sort
            for cxn-string = (format nil "~{~a~^ ~}"
                                     (render (extract-form-predicates cxn-obj)
                                             (get-configuration cxn-inventory :render-mode))) 
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

(defun  sort-cxns-by-meets-constraints (cxns-to-sort meets-constraints)
  (let* ((meets-fc (extract-form-predicate-by-type meets-constraints 'meets))
         (sorted-meets-fc (sort-meets-constraints meets-fc))
         (flat-meets-fc (apply 'concatenate 'list sorted-meets-fc)))
    (sort cxns-to-sort #'< :key #'(lambda (cxn) (cond ((position (variablify (extract-form-predicates cxn))
                                                                 flat-meets-fc))
                                                      (t 0))
                                    ))))

(defun sort-unvariablified-units-by-meets-constraints (units-to-sort meets-constraints)
  (let* ((meets-fc (extract-form-predicate-by-type meets-constraints 'meets))
         (sorted-meets-fc (sort-meets-constraints meets-fc))
         (flat-meets-fc (apply 'concatenate 'list sorted-meets-fc)))
    (sort units-to-sort #'< :key #'(lambda (unit) (let ((pos (position (variablify (second (first (unit-feature-value unit 'boundaries))))
                                                                       flat-meets-fc)))
                                                    (if pos
                                                      pos
                                                      0))))))

;; unsafe - meets constraints are not necessarily ordered correctly! fill in the boundaries and sort the meets constraints!
(defun sort-units-by-meets-constraints (units-to-sort meets-constraints)
  (let* ((meets-fc (extract-form-predicate-by-type meets-constraints 'meets))
         (sorted-meets-fc (sort-meets-constraints meets-fc))
         (flat-meets-fc (apply 'concatenate 'list sorted-meets-fc)))
  (sort units-to-sort #'< :key #'(lambda (unit) (let ((pos (position (second (unit-feature-value unit 'boundaries))
                                                                     flat-meets-fc)))
                                                  (if pos
                                                    pos
                                                    0))))))
(defun make-dummy-fc-from-unit-boundaries (units)
  (loop for unit in units
        for boundaries = (cdr (find 'fcg::boundaries unit :key #'(lambda (item) (if (equal (type-of item) 'cons)
                                                                                  (first item)
                                                                                  item))))
        when boundaries
          collect (list 'fcg::meets (second (first boundaries)) (second (second boundaries)))))

(defun sort-meets-constraints (form-constraints)
  "return the sorted list of meets constraints"
  (let* ((begin-var (first (get-boundary-units form-constraints)))
         (first-predicate (find begin-var form-constraints :key #'second)))
    (loop with next-predicate = first-predicate
          with resulting-list = nil
          for next-var = (third next-predicate)
          while next-predicate
          do (setf resulting-list (pushend next-predicate resulting-list))
             (setf next-predicate (find next-var form-constraints :key #'second))
          finally (return resulting-list))))

(defun sort-units-by-form-string (units-to-sort utterance cxn-inventory)
  "sorts lexical cxns by matching their form strings to the utterance. handles duplicate cxns in one utterance."
  ;; warning, this function depends on space separation without further punctuation!
  ;; todo: replace by looking up meets constraints!
  (if (< (length units-to-sort) 2)
    units-to-sort
    (let ((resulting-list (make-list (length utterance))))
      (loop for cxn-obj in units-to-sort
            for cxn-string = (format nil "~{~a~^ ~}" (render (unit-feature-value cxn-obj 'form) (get-configuration cxn-inventory :render-mode)))
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

(defun check-meets-continuity (form-constraints)
  "check if within a holistic chunk, all form strings are connected"
  (let* ((left-units (loop for fc in form-constraints
                           when (equal 'meets (first fc))
                             collect (second fc)))
         (right-units (loop for fc in form-constraints
                            when (equal 'meets (first fc))
                              collect (third fc)))
         (string-units (loop for fc in form-constraints
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

(defun add-boundaries-to-form-constraints (form-constraints slot-boundaries &key placeholder-var)
  "given a list of boundaries that correspond to a certain slot and a list of form constraints,
   create a new variable for left and right boundary, also if the original left and right boundary vars were identical
   return both the form constraints and the new boundary lists"
  (let* ((new-form-constraints (copy-object form-constraints))
         (placeholder-var (string-upcase (if placeholder-var placeholder-var "?X")))
         (right-var (make-var (make-const (format nil "?RIGHT-~a-BOUNDARY" placeholder-var))))
         (left-var (make-var (make-const (format nil "?LEFT-~a-BOUNDARY" placeholder-var))))
         (left-boundary (first slot-boundaries))
         (right-boundary (second slot-boundaries))
         (matching-left-predicate (find left-boundary (extract-form-predicate-by-type new-form-constraints 'meets) :key #'third))
         (matching-right-predicate (find right-boundary (extract-form-predicate-by-type new-form-constraints 'meets) :key #'second)))
    (when matching-right-predicate
      (setf (nth 1 matching-right-predicate) right-var))
    (when matching-left-predicate
      (setf (nth 2 matching-left-predicate) left-var))
    (values new-form-constraints (list left-var right-var))))

(defun fix-dummy-edge-boundaries (temp-item-based-boundaries rewritten-boundaries)
  (list (if (equal (first temp-item-based-boundaries) ; left-x-boundary
                   (first rewritten-boundaries)) ; the var was at the beginning of the observation, replace left with right boundary
          (second rewritten-boundaries)
          (first temp-item-based-boundaries)
          )
        (if (equal (second temp-item-based-boundaries) ; right-x-boundary
                   (second rewritten-boundaries)) ; the var was at theend of the observation, replace right with left boundary
          (first rewritten-boundaries)
          (second temp-item-based-boundaries))))

(defun get-boundary-units (form-constraints)
  "returns the leftmost and rightmost unit based on meets constraints, even when the meets predicates are in a random order"
  (let* ((left-units (loop for fc in form-constraints
                           when (equal 'meets (first fc))
                             collect (second fc)))
         (right-units (loop for fc in form-constraints
                            when (equal 'meets (first fc))
                              collect (third fc)))
         (left-most-unit (first (set-difference left-units right-units)))
         (right-most-unit (first (set-difference right-units left-units))))
    (if (and left-most-unit right-most-unit)
      (list left-most-unit right-most-unit)
      (when (and (equal 1 (length form-constraints))
                 (equal 'string (first (first form-constraints))))
        (list (second (first form-constraints)) (second (first form-constraints)))))))

(defun get-strings-from-root (node)
  (pf::form-predicates-with-variables
   (extract-string
    (get-root
     (left-pole-structure
        (car-resulting-cfs
         (cipn-car node)))))))

(defun extract-and-render (cxn)
  (list-of-strings->string
   (render (extract-form-predicates cxn)
           (get-configuration (cxn-inventory cxn) :render-mode))))







(defun boundary-list (cxn)
  (loop for unit in (conditional-part cxn)
        for comprehension-lock = (comprehension-lock unit)
        for boundaries = (cdr (find 'boundaries comprehension-lock :key #'first))
        when boundaries
          return (list (second (first boundaries)) (second (second boundaries)))
          ))

(defun transient-structure-form-constraints (transient-structure)
  (remove-if-not #'(lambda (fc)
                     (equal 'string (first fc)))
                 (extract-forms (left-pole-structure transient-structure))))

(defun arg-is-part-of-meaning-p (arg-var meaning)
  (when (or (find arg-var (remove-bind-statements meaning) :key #'second)
            (find arg-var (remove-bind-statements meaning) :key #'third))
    t))
                    
(defun add-cxn-suffix (string &key add-numeric-tail)
  (if add-numeric-tail
    (make-id (upcase (string-append string "-CXN")))
    (intern (upcase (string-append string "-CXN")))))

(defun replace-chunk-variables (fc first-chunk-var last-chunk-var new-slot-var)
  (when (equalp first-chunk-var (third fc))
    (setf (nth 2 fc) new-slot-var))
  (when (equalp last-chunk-var (second fc))
    (setf (nth 1 fc) new-slot-var))
   fc
  )

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

;;(defmethod make-cxn-name ((form list) (cxn-inventory fcg-construction-set) &key (add-cxn-suffix t))
;;  "Transform an utterance into a suitable construction name"
;;  (make-cxn-name (format nil "~{~a~^-~}" (render form (get-configuration cxn-inventory :render-mode))) cxn-inventory))

;; (make-cxn-name '((string ?x "x") (string ?y "y") (precedes ?y ?x)) *fcg-constructions*)
;; (make-cxn-name '((precedes ?y ?x)) *fcg-constructions*)

(defun extract-form-predicate-by-type (form-values symbol)
  "extract meets, precedes or string predicates from a list of form predicates"
  (loop for form-value in form-values
        when (and (consp form-value) (eq (first form-value) symbol))
        collect form-value))

(defun variablify-form-constraints-with-constants (form-constraints-with-constants)
  (loop for fc-with-const in form-constraints-with-constants
        for first-fc-with-var = (first fc-with-const)
        for rest-fc-with-var = (if (equal 'string (first fc-with-const))
                                 (list (variablify (second fc-with-const)) (third fc-with-const))
                                 (mapcar #'variablify (rest fc-with-const)))
        collect (cons first-fc-with-var rest-fc-with-var)))

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

(defun create-item-based-lex-class-with-var (placeholder-var-string-predicates cxn-name-item-based-cxn slot-var)
  "create the what-is-the-size-of-the-?Y-?X-12-(?X) lex class for a specific slot var"
  (let ((placeholder (third (find slot-var placeholder-var-string-predicates :key #'second))))
    (unless placeholder
      (+ 1 1))
    (assert (not (equal nil placeholder)))
    (make-lex-class (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-(" placeholder ")"))))


(defun get-car-for-unit (unit cars)
  (loop for car in cars
        for resulting-left-pole-structure = (left-pole-structure (car-resulting-cfs car))
        for root = (get-root resulting-left-pole-structure)
        for res-unit = (last-elt (remove root resulting-left-pole-structure)) ;;match with last
        when (equal res-unit unit)
          return car))


(defun get-inverted-cxn-meanings (cxns gold-standard-meaning)
  "does the inverse set difference between the gold std meaning and the cxn meanings, as to return the matching part from the gold standard meanings that can be subtracted with equals"
  (loop for cxn in cxns
        for meaning = (get-subtracted-meaning-from-cxn cxn gold-standard-meaning)
        when meaning
          collect meaning))

(defun cxn-meaning-is-valid-gold-standard-subset-p (cxn-meanings)
  (not (member nil cxn-meanings)))

(defun subtract-cxn-meanings-from-gold-standard-meaning (cxn-meanings gold-standard-meaning)
  (loop with resulting-meaning = gold-standard-meaning
        for meaning in cxn-meanings
        do (setf resulting-meaning (set-difference resulting-meaning meaning :test #'equal))
        finally (return resulting-meaning)))

(defun get-subtracted-meaning-from-cxn (cxn gold-standard-meaning)
  (let* ((cxn-meaning (extract-meaning-predicates (original-cxn cxn)))
         (subtracted-meaning (second (multiple-value-list (commutative-irl-subset-diff gold-standard-meaning cxn-meaning)))))
    subtracted-meaning))
    

(defun get-subtracted-meaning-from-car (car gold-standard-meaning)
  (let* ((cxn-meaning (extract-meaning-predicates (original-cxn (car-applied-cxn car))))
         (subtracted-meaning (second (multiple-value-list (commutative-irl-subset-diff gold-standard-meaning cxn-meaning)))))
    subtracted-meaning))

(defun subtract-holistic-from-item-based-meaning (gold-standard-meaning subtracted-meanings)
  (loop with item-based-meaning = (copy-object gold-standard-meaning)
        for network in subtracted-meanings
        do (setf item-based-meaning (set-difference item-based-meaning network :test #'equal))
        finally (return item-based-meaning)))

(defun make-item-based-name-form-constraints-from-units (item-based-cxn-form-constraints resulting-units)
  (loop with item-based-fc = item-based-cxn-form-constraints
        for unit in resulting-units
        for boundaries = (unit-feature-value unit 'boundaries)
        for left-boundary = (variablify (second (first boundaries)))
        for right-boundary = (variablify (second (second boundaries)))
        do (setf item-based-fc (loop for fc in (copy-object item-based-fc)
                                     collect (replace-chunk-variables fc left-boundary right-boundary left-boundary)))
        finally (return item-based-fc)))

(defun substitute-slot-meets-constraints (chunk-meet-constraints item-based-meet-constraints)
  "for slots that hold larger chunks, replace the rightmost boundary of the chunk with the leftmost variable, so that it appears as one slot in the cxn name"
  (let* ((slot-boundaries (get-boundary-units chunk-meet-constraints))
         (left-boundary (first slot-boundaries))
         (right-boundary (second slot-boundaries)))
    (loop for fc in (copy-object item-based-meet-constraints)
          collect (replace-chunk-variables fc left-boundary right-boundary left-boundary))))

(defun make-item-based-name-form-constraints-from-cxns (item-based-cxn-form-constraints boundary-list)
  (loop with item-based-fc = item-based-cxn-form-constraints
        for (left-boundary right-boundary) in boundary-list
        do (setf item-based-fc (loop for fc in (copy-object item-based-fc)
                                     collect (replace-chunk-variables fc left-boundary right-boundary left-boundary)))
        finally (return item-based-fc)))

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
        

;; (meaning-predicates-with-variables '((get-context source) (bind attribute-category attribute color) (bind shape-category shape cube) (unique object cubes) (filter cubes source shape) (query response object attribute)))

(defun form-predicates-with-variables (form-predicates)
  "Transform form constraints with constants to form constraints with variables."
    (loop for predicate in form-predicates
          collect (if (equalp (first predicate) 'string)
                    (list (first predicate)
                          (variablify (second predicate))
                          (third predicate))
                    (cons (first predicate)
                          (mapcar #'variablify (rest predicate))))))
        

;; (meaning-predicates-with-variables '((get-context source) (bind attribute-category attribute color) (bind shape-category shape cube) (unique object cubes) (filter cubes source shape) (query response object attribute)))


(defun unit-ify (symbol)
  (intern  (string-downcase (format nil "?~a-unit" (get-base-name symbol)))))

(defun variablify (symbol)
  "Turn a symbol into a variable if it isn't one yet."
  (if (variable-p symbol)
    symbol
    (intern (format nil "?~a" symbol))))

;; (variablify 'x)

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








(defmethod irl::find-map-function ((v1 string) (v2 string) 
                                   &optional (frame (irl::make-map-frame))
                                   &key (extension-test #'irl::function-frame))
  "Adding case for strings, used when comparing predicate networks"
  (declare (ignore extension-test))
  (when (string= v1 v2) 
    frame))
 
(defun subunit-names-for-lex-cxns (lex-cxns)
  (loop for lex-cxn in lex-cxns
        for lex-cxn-form = (extract-form-predicates lex-cxn)
        for lex-cxn-unit-name = (second (find 'string lex-cxn-form :key #'first))
        collect lex-cxn-unit-name))

(defun subunit-blocks-for-holistic-cxns (holistic-subunit-names boundaries args th-links)
  (loop for holistic-cxn-unit-name in holistic-subunit-names
        for arg in args
        for boundary-list in boundaries
        for th-link in th-links
        for holistic-slot-lex-class = (cdr th-link)
        for leftmost-unit-holistic-cxn = (first boundary-list)
        for rightmost-unit-holistic-cxn = (second boundary-list)
        collect `(,holistic-cxn-unit-name
                  (syn-cat (pf::lex-class ,holistic-slot-lex-class))) into contributing-units
        collect `(,holistic-cxn-unit-name
                  (args (,arg))
                  --
                  (boundaries
                   (left ,leftmost-unit-holistic-cxn)
                   (right ,rightmost-unit-holistic-cxn))) into conditional-units
        finally (return (values conditional-units contributing-units))))

(defun find-matching-holistic-cxns-in-root (cxn-inventory root-strings)
  (remove nil (loop for remaining-form in root-strings
                    for root-string = (third remaining-form)
                    collect (loop for cxn in (constructions cxn-inventory)
                                  when (and (eql (attr-val cxn :cxn-type) 'holistic)
                                            (string= (third (first (extract-form-predicates cxn))) root-string))
                                    return cxn))))

(defun subtract-holistic-cxn-meanings (lex-cxns gold-standard-meaning)
  (let ((lex-cxn-meanings (map 'list #'extract-meaning-predicates lex-cxns)))
    (loop for lex-cxn-meaning in lex-cxn-meanings
          do (setf gold-standard-meaning (set-difference gold-standard-meaning lex-cxn-meaning :test #'irl:unify-irl-programs)))
    gold-standard-meaning))

(defun subtract-holistic-cxn-forms (lex-cxns string-predicates-in-root)
  (loop for lex-cxn in lex-cxns
        for lex-form = (extract-form-predicates lex-cxn)
        do (setf string-predicates-in-root (set-difference string-predicates-in-root lex-form :test #'irl:unify-irl-programs)))
  string-predicates-in-root)

(defun reverse-bindings (bindings)
  (loop for binding in bindings
        for car = (car binding)
        for cdr = (cdr binding)
        collect (cons cdr car)))

(defgeneric equivalent-meaning-networks (m1 m2 mode))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :irl)))
  (equivalent-irl-programs? m1 m2))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :amr)))
  (amr::equivalent-amr-predicate-networks m1 m2))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :geo)))
  (amr::equivalent-amr-predicate-networks m1 m2))
         


(defun extract-args-from-resulting-unit (unit)
  (second (find 'args (rest unit) :key #'first)))

(defun extract-args-from-irl-network (irl-network)
  "return all unbound variables as list"
  (sort irl-network #'string-lessp :key (lambda (predicate)
                                          (if (equal (first predicate) 'bind)
                                            (symbol-name (third predicate))
                                            (symbol-name (second predicate)))))
  (let* ((in-vars (loop for predicate in irl-network
                        when (not (equal (first predicate) 'bind))
                          collect (third predicate)))
         (out-vars (loop for predicate in irl-network
                         when (not (equal (first predicate) 'bind))
                           collect (second predicate)))
         (unresolved-vars (remove nil (append (set-difference in-vars out-vars) (set-difference out-vars in-vars)))))
    unresolved-vars
    ))

(defun extract-vars-from-irl-network (irl-network)
  "return the in-var, out-var and list of open variables from a predicate"
  (values (last-elt (get-open-vars irl-network))
          (get-target-var irl-network)
          (set-difference (get-open-vars irl-network) (last (get-open-vars irl-network)))))

(defun get-irl-predicate-from-in-var (var irl-program)
  "Find the next predicate, given an input variable"
  (loop for predicate in irl-program
        for in-var = (first (multiple-value-list (extract-vars-from-irl-network (list predicate))))
        when (equal var in-var)
          return predicate))

(defmethod get-best-partial-analysis-cipn ((form-constraints list) (gold-standard-meaning list) (original-cxn-inventory fcg-construction-set) (mode (eql :optimal-form-coverage)))
  (disable-meta-layer-configuration original-cxn-inventory) ;; also relaxes cat-network-lookup to path-exists without transitive closure!
  (set-configuration original-cxn-inventory :parse-goal-tests '(:no-applicable-cxns))
  (with-disabled-monitor-notifications
    (let* ((comprehension-result (multiple-value-list (comprehend-all form-constraints :cxn-inventory original-cxn-inventory)))
           (cip-nodes (discard-partial-solutions-with-incompatible-args 
                       (discard-cipns-with-incompatible-meanings (second comprehension-result) (first comprehension-result) gold-standard-meaning)
                       gold-standard-meaning
                       (get-configuration original-cxn-inventory :meaning-representation-formalism)
                       )))
      (enable-meta-layer-configuration original-cxn-inventory)
      (first (sort cip-nodes #'sort-cipns-by-coverage-and-nr-of-applied-cxns)))))

(defun discard-cipns-with-incompatible-meanings-and-args (candidate-cip-nodes candidate-meanings gold-standard-meaning)
  (loop for cipn in candidate-cip-nodes
        for candidate-meaning in candidate-meanings
        for resulting-left-pole-structure = (left-pole-structure (car-resulting-cfs (cipn-car cipn)))
        for resulting-root = (get-root resulting-left-pole-structure)
        for units = (remove-child-units (remove resulting-root resulting-left-pole-structure))
        for bindings = (irl::embedding candidate-meaning gold-standard-meaning)
        when (and bindings
                  ;(no-duplicate-bindings-p bindings)
                  (loop for unit in units
                        always (extract-args-from-resulting-unit unit)))
          collect cipn))


(defmethod get-best-partial-analysis-cipn ((form-constraints list) (gold-standard-meaning list) (original-cxn-inventory fcg-construction-set) (mode (eql :optimal-form-coverage-irl)))
  (disable-meta-layer-configuration original-cxn-inventory) ;; also relaxes cat-network-lookup to path-exists without transitive closure!
  (set-configuration original-cxn-inventory :parse-goal-tests '(:no-applicable-cxns))
  (with-disabled-monitor-notifications
    (let* ((comprehension-result (multiple-value-list (comprehend-all form-constraints :cxn-inventory original-cxn-inventory)))
           (cip-nodes (discard-cipns-with-incompatible-meanings-and-args (second comprehension-result) (first comprehension-result) gold-standard-meaning)))
      (enable-meta-layer-configuration original-cxn-inventory)
      (first (sort cip-nodes #'sort-cipns-by-coverage-and-nr-of-applied-cxns)))))


(defmethod get-best-partial-analysis-cipn ((form-constraints list) (gold-standard-meaning list) (original-cxn-inventory fcg-construction-set) (mode (eql :optimal-form-coverage-item-based-first)))
  (disable-meta-layer-configuration-item-based-first original-cxn-inventory) ;; also relaxes cat-network-lookup to path-exists without transitive closure!
  
  (with-disabled-monitor-notifications
    (let* ((comprehension-result (multiple-value-list (comprehend-all form-constraints :cxn-inventory original-cxn-inventory :n 1)))
           (cip-nodes (ignore-initial-nodes (discard-cipns-with-incompatible-meanings (second comprehension-result) (first comprehension-result) gold-standard-meaning))))
      (enable-meta-layer-configuration-item-based-first original-cxn-inventory)
      (first (sort cip-nodes #'sort-cipns-by-coverage-and-nr-of-applied-cxns)))))

(defun get-root-form-predicates (cipn)
  (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car cipn)))) 'form))

(defun extract-meaning-from-tree (top-unit-name transient-structure)
  (let ((top-unit (find top-unit-name (left-pole-structure transient-structure) :key #'first :test #'string=)))
    (extract-meanings
     (cons top-unit
           (all-subunits
            top-unit
            (left-pole-structure transient-structure))))))

(defun sort-cipns-by-coverage-and-nr-of-applied-cxns (cipn-1 cipn-2)
  (cond ((< (length (get-root-form-predicates cipn-1))
            (length (get-root-form-predicates cipn-2)))
         cipn-1)
        ((> (length (get-root-form-predicates cipn-1))
            (length (get-root-form-predicates cipn-2)))
         cipn-2)
        ((>= (length (applied-constructions cipn-1))
             (length (applied-constructions cipn-2)))
         cipn-1)
        (t
         cipn-2)))


(defun no-duplicate-bindings-p (u-frames)
  "return nil if two variables collide after renamings"
  (loop for frame in u-frames
        for bindings = (irl::map-frame-bindings frame)
        for tgt-bindings = (mapcar #'cdr bindings)
        always (equal tgt-bindings (remove-duplicates tgt-bindings))))

(defun discard-cipns-with-incompatible-meanings (candidate-cip-nodes candidate-meanings gold-standard-meaning)
  (loop for cipn in candidate-cip-nodes
        for candidate-meaning in candidate-meanings
        for bindings = (irl::embedding candidate-meaning gold-standard-meaning)
        when bindings
          collect cipn))

(defun remove-nodes-containing-applied-cxns-with-type (type nodes)
  (loop for node in nodes
        for applied-cxns = (applied-constructions node)
        unless (member type applied-cxns :key (lambda (cxn) (attr-val cxn :cxn-type)))
          collect node))

(defun remove-cxns-with-phrase-type (type cxn-inventory)
  (loop for cxn in (constructions cxn-inventory)
        for cxn-type = (attr-val cxn :cxn-type)
        when (equal cxn-type type)
          do (delete-cxn cxn cxn-inventory)
        finally (return cxn-inventory)))


(defmethod comprehend-w-derenderer (utterance &key
                                              (cxn-inventory *fcg-constructions*)
                                              (silent nil))
  (let ((initial-cfs (de-render utterance :de-render-string-meets-no-punct :cxn-inventory cxn-inventory))
        (processing-cxn-inventory (processing-cxn-inventory cxn-inventory)))
    ;; Add utterance and meaning to blackboard
    (set-data initial-cfs :utterances (listify utterance))
    
    ;; Notification
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    ;; Construction application
    (multiple-value-bind (solution cip)
        (fcg-apply processing-cxn-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning (and solution
                          (extract-meanings
                           (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        ;; Notification
        (unless silent (notify parse-finished meaning processing-cxn-inventory))
        ;; Return value
        (values meaning solution cip)))))


(defun run-validation (cxn-inventory data)
  ;; reset configurations of inventory
  (set-configuration cxn-inventory :parse-goal-tests '(:no-strings-in-root :no-applicable-cxns :connected-semantic-network :connected-structure))
  (set-configuration cxn-inventory :cxn-supplier-mode :hashed-and-scored-routine-cxn-set-only)
  (set-configuration cxn-inventory :category-linking-mode :neighbours)
  (set-configuration cxn-inventory :update-categorial-links nil)
  (set-configuration cxn-inventory :use-meta-layer nil)
  (set-configuration cxn-inventory :consolidate-repairs nil)
  ;; process all data
  (loop with success-count = 0
        for interaction from 1
        for entry in data
        for utterance = (first entry)
        for gold-std-meaning = (cdr entry)
        for comprehended-meaning = (first (multiple-value-list (comprehend-w-derenderer utterance :cxn-inventory cxn-inventory)))
        for success-p = (irl:equivalent-irl-programs? comprehended-meaning gold-std-meaning)
        do (if success-p
             (progn (incf success-count)
               (format t "~a" "."))
             (format t "~a" "!"))
           (when (= 0 (mod interaction 100))
             (format t " (~a | ~a% overall avg.)~%" interaction (* 100 (float (/ success-count interaction)))))
        finally (return (* 100 (float (/ success-count (length data)))))))


(defun discard-partial-solutions-with-incompatible-args (cip-nodes gold-standard-meaning meaning-representation-formalism)
  "embed the unit-meaning with the gold standard, rename it, take the set diff, then get the args of the set diff vs gold std. the unit args should match these renamed gold std args"
  (loop for cip-node in (ignore-initial-nodes cip-nodes)
        when (loop with root = (get-root (left-pole-structure (car-resulting-cfs (cipn-car cip-node))))
                   with top-level-units = (remove root (remove-child-units (left-pole-structure (car-resulting-cfs (cipn-car cip-node)))))
                   for top-unit in top-level-units
                   for top-unit-meaning = (extract-meaning top-unit)
                   for unit-args = (second (find 'ARGS (rest top-unit) :key #'first))
                   for embedding = (irl::embedding top-unit-meaning gold-standard-meaning)
                   for overlapping-predicates = (when embedding
                                                  (loop for predicate in top-unit-meaning
                                                        collect (substitute-predicate-bindings predicate (first embedding))))
                   for non-overlapping-predicates = (set-difference gold-standard-meaning overlapping-predicates :test #'equal)            
                   for diff-args = (extract-args-from-meaning-networks overlapping-predicates non-overlapping-predicates meaning-representation-formalism)
                   for renamed-unit-args = (when embedding (substitute-predicate-bindings unit-args (first embedding)))
                   ;always (and non-overlapping-predicates ;; if there are no non-overlapping predicates, then it's a full solution, not a partial one.
                    ;           (equal renamed-unit-args diff-args)))
                   always (equal renamed-unit-args diff-args))
          collect cip-node))

(defun ignore-initial-nodes (cip-nodes)
  (loop for node in cip-nodes
        unless (find 'FCG::INITIAL (statuses node))
          collect node))

(defun remove-holophrases (grammar)
  (loop for cxn in (constructions grammar)
        when (eql (attr-val cxn :is-holophrase) t)
          do (delete-cxn cxn grammar)))

(defun print-holophrases (grammar)
  (loop for cxn in (constructions grammar)
        when (and (eql (attr-val cxn :is-holophrase) t)
                  (string= (attr-val cxn :label) 'routine))
          do (format t "~a (~a)~%" (attr-val cxn :bare-cxn-name) (attr-val cxn :score))))