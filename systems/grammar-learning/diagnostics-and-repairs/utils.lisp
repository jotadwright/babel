
(in-package :grammar-learning)

(defconstant +placeholder-vars+ '("?X" "?Y" "?Z" "?A" "?B" "?C" "?D" "?E" "?F" "?G" "?H" "?I" "?J" "?K" "?L" "?M" "?N" "?O" "?P" "?Q" "?R" "?S" "?T" "?U" "?V" "?W"))

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

(defun add-boundaries-to-form-constraints (form-constraints boundaries &key placeholder-var)
  "given a list of boundaries that correspond to a certain slot and a list of form constraints,
   create a new variable for left and right boundary, also if the original left and right boundary vars were identical
   return both the form constraints and the new boundary list"
  (let* ((new-form-constraints (copy-object form-constraints))
         (placeholder-var (string-upcase (if placeholder-var placeholder-var "?X")))
         (right-var (make-var (make-const (format nil "?RIGHT-~a-BOUNDARY" placeholder-var))))
         (left-var (make-var (make-const (format nil "?LEFT-~a-BOUNDARY" placeholder-var))))
         (left-boundary (first boundaries))
         (right-boundary (second boundaries))
         (matching-left-predicate (find left-boundary (extract-form-predicate-by-type new-form-constraints 'meets) :key #'third))
         (matching-right-predicate (find right-boundary (extract-form-predicate-by-type new-form-constraints 'meets) :key #'second)))
    (if (equal right-boundary (first (get-boundary-units form-constraints))) ; the variable is a the beginning of an utterance when the right-boundary is the leftmost-boundary
      (values new-form-constraints (list left-var right-boundary))
      (when matching-right-predicate
        (setf (nth 1 matching-right-predicate) right-var)))
    (values new-form-constraints (list left-boundary right-var))
    ))

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
                           

(defun initial-transient-structure (node)
  (if (find 'fcg::initial (statuses node))
    (car-source-cfs (cipn-car node))
    (car-source-cfs (cipn-car (last-elt (all-parents node))))))

(defun initial-node (node)
  "returns the first node in the cip"
  (if (all-parents node)
    (last-elt (all-parents node))
    node))

;; to do: split into method with type specification for processing cxn vs original cxn
;fcg-construction
;construction
(defun get-all-unit-lex-classes (cxn)
  (loop for unit in (subseq (contributing-part cxn) 1)
        for lex-class = (lex-class-item-based unit)
        collect lex-class))

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

(defun lex-class (unit)
  (let* ((syn-cat (find 'syn-cat (unit-body unit) :key #'first))
         (lex-class (find 'lex-class (second syn-cat) :key #'first)))    
    (when lex-class
      (second lex-class))))

(defun lex-class-item-based (unit)
  (let ((syn-cat (find 'syn-cat (fcg::unit-structure unit) :key #'first)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))

(defun lex-class-cxn (cxn)
  "return the lex-class of a cxn"
  (let ((syn-cat (find 'syn-cat (fcg::unit-structure (last-elt (contributing-part cxn))) :key #'feature-name)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))
         

(defun non-overlapping-meaning (meaning cxn &key (nom-cxn nil) (nom-observation nil))
  (when (and nom-cxn nom-observation) (error "only nom-cxn or nom-observeration can be true"))
  (multiple-value-bind (non-overlapping-meaning-observation non-overlapping-meaning-cxn)
      (non-overlapping-predicates meaning (extract-meaning-predicates cxn))
      (cond (nom-cxn non-overlapping-meaning-cxn)
            (nom-observation non-overlapping-meaning-observation))))

(defun non-overlapping-form (utterance-form-constraints cxn &key (nof-cxn nil) (nof-observation nil))
  (when (and nof-cxn nof-observation) (error "only nof-cxn or nof-observation can be true"))
  (multiple-value-bind (non-overlapping-form-observation non-overlapping-form-cxn)
      (non-overlapping-predicates-ignore-length utterance-form-constraints
                                                (extract-form-predicates cxn))
    (let* ((meets-constraints-cxn (cdr (find-meets-constraints
                                   (set-difference (extract-form-predicates cxn) non-overlapping-form-cxn :test #'equal)
                                   non-overlapping-form-cxn)))
           (meets-constraints-observation (cdr (find-meets-constraints
                                           (set-difference utterance-form-constraints non-overlapping-form-observation :test #'equal)
                                           non-overlapping-form-observation))))
      (cond (nof-cxn (append non-overlapping-form-cxn meets-constraints-cxn))
            (nof-observation (append non-overlapping-form-observation meets-constraints-observation))))))

(defun non-overlapping-predicates-ignore-length (network-1 network-2)
  (let ((unique-part-network-1 (set-difference network-1 network-2 :test #'irl:unify-irl-programs))
        (unique-part-network-2 (set-difference network-2 network-1 :test #'irl:unify-irl-programs)))
      (values unique-part-network-1 unique-part-network-2)))

(defun non-overlapping-predicates (network-1 network-2)
  (let ((unique-part-network-1 (set-difference network-1 network-2 :test #'irl:unify-irl-programs))
        (unique-part-network-2 (set-difference network-2 network-1 :test #'irl:unify-irl-programs)))
    (when (and (= (length unique-part-network-1)
                  (length unique-part-network-2))
               (irl:equivalent-irl-programs? (set-difference network-1 unique-part-network-1)
                                             (set-difference network-2 unique-part-network-2)))
      (values unique-part-network-1 unique-part-network-2))))

(defun find-cxn-by-form-and-meaning (form meaning cxn-inventory &key cxn-type)
  "returns a cxn with the same meaning and form if it's in the cxn-inventory"
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        for cxn-type-cxn = (attr-val cxn :cxn-type)
        when (and
              (if cxn-type (equal cxn-type cxn-type-cxn) t)
              (irl:equivalent-irl-programs? form (extract-form-predicates cxn))
              (irl:equivalent-irl-programs? meaning (extract-meaning-predicates cxn))
                  ; note: boundaries and args are ignored, as they are designed to always match, and fully depend on form and meaning anyway.
              )
        return cxn))



(defun initial-node-p (node)
  "return t if node is initial node"
  (null (all-parents node)))

(defun add-cxn-suffix (string &key add-numeric-tail)
  (if add-numeric-tail
    (make-id (upcase (string-append string "-CXN")))
    (intern (upcase (string-append string "-CXN")))))

(defun replace-special-initial-chars (string)
  (if (member (subseq string 0 1) '("?" "!" "\"") :test #'string=)
    (string-append "-" string)
    string))
 
(defun make-lex-class (cat-name &key add-numeric-tail trim-cxn-suffix)
  (let* ((name-string (replace-special-initial-chars (if (equal (type-of cat-name) 'SYMBOL)
                       (string-downcase (symbol-name cat-name))
                       (string-downcase cat-name))))
        (cat-name (if trim-cxn-suffix (fcg::replace-all name-string "-cxn" "")
                    name-string)))
  (intern
   (string-downcase (symbol-name
    (funcall (if add-numeric-tail #'make-const #'make-symbol)
             (upcase
              (if cat-name cat-name "CAT")))))
   :grammar-learning)))

(defgeneric make-cxn-name (thing cxn-inventory &key add-cxn-suffix add-numeric-tail))

(defmethod make-cxn-name ((string string) (cxn-inventory fcg-construction-set)
                          &key (add-cxn-suffix t) (add-numeric-tail nil))
  "Transform an utterance into a suitable construction name"
  (declare (ignore cxn-inventory))
  (let ((name-string (substitute #\- #\Space
                   (upcase
                    (if add-cxn-suffix
                      (string-append string "-cxn")
                      string)))))
  (if add-numeric-tail
    (make-id name-string)
    (intern name-string))))



(defun variablify-missing-form-strings (form-constraints)
  "create X Y Z etc variables by checking which strings are missing in meets constraints return a list of placeholder bindings in the form of string predicates"
  (loop with string-constraints = (extract-form-predicate-by-type form-constraints 'string)
        with placeholder-index = 0
        with new-string-constraints = nil
        with meets-constraints = (set-difference form-constraints string-constraints)
        for meets-constraint in meets-constraints
        for first-word-var = (second meets-constraint)
        for second-word-var = (third meets-constraint)
        do
        (unless (or (find first-word-var string-constraints :key #'second)
                    (find first-word-var new-string-constraints :key #'second))
          (push `(string ,first-word-var ,(nth placeholder-index +placeholder-vars+)) new-string-constraints)
          (incf placeholder-index))
        (unless (or (find second-word-var string-constraints :key #'second)
                    (find second-word-var new-string-constraints :key #'second))
          (push `(string ,second-word-var ,(nth placeholder-index +placeholder-vars+)) new-string-constraints)
          (incf placeholder-index))
        finally (return
                 new-string-constraints)))
  

;; (make-cxn-name "What is the color of the cube" *fcg-constructions*)

(defmethod make-cxn-name ((form-constraints list) (cxn-inventory fcg-construction-set)
                          &key (add-cxn-suffix t) (add-numeric-tail nil))
  "Transform a list of form constraints into a suitable construction name"
  (let ((new-string-constraints (variablify-missing-form-strings form-constraints)))
    (make-cxn-name (format nil "~{~a~^-~}"
                           (render (append form-constraints new-string-constraints)
                                   (get-configuration cxn-inventory :render-mode)))
                   cxn-inventory :add-cxn-suffix add-cxn-suffix :add-numeric-tail add-numeric-tail)))

(defun substitute-slot-meets-constraints (chunk-meet-constraints item-based-meet-constraints)
  "for slots that hold larger chunks, replace the rightmost boundary of the chunk with the leftmost variable, so that it appears as one slot in the cxn name"
  (let* ((slot-boundaries (get-boundary-units chunk-meet-constraints))
         (left-boundary (first slot-boundaries))
         (right-boundary (second slot-boundaries)))
    (loop for fc in (copy-object item-based-meet-constraints)
          collect (replace-chunk-variables fc left-boundary right-boundary left-boundary))))

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
        finally return item-based-meaning))

(defun make-item-based-name-form-constraints-from-units (item-based-cxn-form-constraints resulting-units)
  (loop with item-based-fc = item-based-cxn-form-constraints
        for unit in resulting-units
        for fc = (variablify-form-constraints-with-constants (unit-feature-value unit 'form))
        do (setf item-based-fc (substitute-slot-meets-constraints fc item-based-fc))
        finally return item-based-fc))


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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding possible alignments for predicate networks and form ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass alignment-state ()
  ((assigned-predicates
    :type (or list null)
    :initform nil
    :initarg :assigned-predicates
    :accessor assigned-predicates)
   (remaining-predicates-longest-list
    :type (or list null)
    :initform nil
    :initarg :remaining-predicates-longest-list
    :accessor remaining-predicates-longest-list)
   (remaining-predicates-shortest-list
    :type (or list null)
    :initform nil
    :initarg :remaining-predicates-shortest-list
    :accessor remaining-predicates-shortest-list)))

(defun all-possible-alignments (list-1 list-2 &key
                                       (nr-of-unpaired-predicates-allowed 0))
  (let* ((longest-list  (if (>= (length list-1) (length list-2))
                        list-1 list-2))
        (shortest-list (if (< (length list-1) (length list-2))
                         list-1 list-2))
        (queue (list (make-instance 'alignment-state
                                    :assigned-predicates nil
                                    :remaining-predicates-longest-list longest-list
                                    :remaining-predicates-shortest-list shortest-list)))
        (possible-alignments nil))
    (loop until (not queue)
          for current-state = (pop queue)
          for assigned-predicates = (assigned-predicates current-state)
          for remaining-predicates-longest-list = (remaining-predicates-longest-list current-state)
          for remaining-predicates-shortest-list = (remaining-predicates-shortest-list current-state)
          do
          (if (or remaining-predicates-longest-list remaining-predicates-shortest-list)
            (loop for predicate-1 in (cons nil remaining-predicates-longest-list)
                  do
                  (loop for predicate-2 in (cons nil remaining-predicates-shortest-list)
                        for state = (make-instance 'alignment-state
                                          :assigned-predicates (cons `(,predicate-1 ,predicate-2) assigned-predicates)
                                          :remaining-predicates-longest-list (remove predicate-1 remaining-predicates-longest-list)
                                          :remaining-predicates-shortest-list (remove predicate-2 remaining-predicates-shortest-list))
                        if (and (or predicate-1 predicate-2)
                                (if (and predicate-1 predicate-2)
                                  (and (= (length predicate-1) (length predicate-2))
                                       (equalp (first predicate-1) (first predicate-2))
                                       (loop with equal-symbols = t
                                             for el-1 in predicate-1
                                             for el-2 in predicate-2
                                             if (and (not (or (variable-p el-1) (variable-p el-2)))
                                                     (not (equalp el-1 el-2)))
                                             do (setf equal-symbols nil)
                                             finally (return equal-symbols)))
                                  t)
                                (<= (count nil (mapcar #'first (assigned-predicates state))) nr-of-unpaired-predicates-allowed)
                                (<= (count nil (mapcar #'second (assigned-predicates state))) nr-of-unpaired-predicates-allowed)
                                (<= (length assigned-predicates) (+ (length longest-list) (length shortest-list))))
                        do (push state queue)))
            (unless (find assigned-predicates possible-alignments :test #'(lambda (l1 l2) (permutation-of? l1 l2 :test #'equal)))
              (push assigned-predicates possible-alignments))))
    possible-alignments))

#| 
(length (all-possible-alignments (shuffle '((get-context ?source)
                                            (bind attribute-category ?attribute color)
                                            (bind shape-category ?shape cube)
                                            (unique ?object ?cubes)
                                            (filter ?cubes ?source ?shape)
                                            (query ?response ?object ?attribute)))
                                 (shuffle '((get-context ?source)
                                            (bind attribute-category ?attribute color)
                                            (bind shape-category ?shape sphere)
                                            (unique ?object ?spheres)
                                            (filter ?spheres ?source ?shape)
                                            (query ?response ?object ?attribute)))
                                 :nr-of-unpaired-predicates-allowed 2))
|#


(defmethod irl::find-map-function ((v1 string) (v2 string) 
                        &optional (frame (irl::make-map-frame))
                        &key (extension-test #'irl::function-frame))
  "Adding case for strings, used when comparing predicate networks"
  (declare (ignore extension-test))
      (when (string= v1 v2) 
        frame))

(defun diff-superset-subset-form (superset-cxn utterance)
  (set-difference (extract-form-predicates superset-cxn)
                  (form-constraints-with-variables utterance (get-configuration (cxn-inventory superset-cxn) :de-render-mode))
                  :test #'irl:unify-irl-programs))

(defun find-subset-holophrase-cxn (cxn-inventory gold-standard-meaning utterance meaning-representation-formalism)
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        for cxn-form-constraints = (extract-form-predicates cxn)
        for cxn-meaning-constraints = (extract-meaning-predicates cxn)
        for superset-form = (form-constraints-with-variables utterance (get-configuration (cxn-inventory cxn) :de-render-mode))
        for non-overlapping-form = (non-overlapping-form superset-form cxn :nof-observation t)
        for non-overlapping-form-inverted = (set-difference cxn-form-constraints superset-form  :test #'irl:unify-irl-programs)
        for non-overlapping-meanings = (multiple-value-list (diff-meaning-networks gold-standard-meaning cxn-meaning-constraints meaning-representation-formalism))
        for non-overlapping-meaning = (first non-overlapping-meanings)
        for non-overlapping-meaning-inverted = (second non-overlapping-meanings)
        for cxn-type = (attr-val cxn :cxn-type)
        when (and (eql cxn-type 'holophrase) ; todo: we might want to remove this!
                  non-overlapping-form
                  non-overlapping-meaning
                  (not non-overlapping-form-inverted) ; the set diff of smaller - larger = nil
                  (not non-overlapping-meaning-inverted)
                  (check-meets-continuity non-overlapping-form))
                  
        ;; needs to be a holophrase, the form constraints for string and precedes constraints need to be a subset of the cxn, the meaning constraints need to be a subset too, and the meets of the new holistic cxn should all be connected
        return (values cxn superset-form non-overlapping-form non-overlapping-meaning)))

(defun find-superset-holophrase-cxn (cxn-inventory gold-standard-meaning utterance meaning-representation-formalism)
  ;; todo: there could also be more than one superset cxn!
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        for cxn-form-constraints = (extract-form-predicates cxn)
        for cxn-meaning-constraints = (extract-meaning-predicates cxn)
        for cxn-type =  (attr-val cxn :cxn-type)
        for superset-form = (form-constraints-with-variables utterance (get-configuration (cxn-inventory cxn) :de-render-mode))
        for non-overlapping-form = (non-overlapping-form superset-form cxn :nof-cxn t)
        for non-overlapping-meanings = (multiple-value-list (diff-meaning-networks cxn-meaning-constraints gold-standard-meaning meaning-representation-formalism))
        for non-overlapping-meaning = (first non-overlapping-meanings)
        for non-overlapping-form-inverted = (set-difference superset-form cxn-form-constraints :test #'irl:unify-irl-programs)
        for non-overlapping-meaning-inverted = (second non-overlapping-meanings)
        when (and (eql cxn-type 'holophrase) ; todo: we might want to remove this!
                  non-overlapping-form
                  non-overlapping-meaning
                  (not non-overlapping-form-inverted) ; the set diff of smaller - larger = nil
                  (not non-overlapping-meaning-inverted)
                  (check-meets-continuity non-overlapping-form))
        ;; needs to be a holophrase, the form constraints for string and precedes constraints need to be a subset of the cxn, the meaning constraints need to be a subset too (todo: see if this is really the case in IRL)
        return (values cxn non-overlapping-form non-overlapping-meaning)))


(defun find-meets-constraints (superset-with-meets subset-without-meets)
  (loop for fc in subset-without-meets
        for var = (second fc)
        for superset-meets-constraints = (extract-form-predicate-by-type superset-with-meets 'meets)
        for meet = (or (find var superset-meets-constraints :key #'second)
                       (find var superset-meets-constraints :key #'third))
        collect meet))

(defun select-cxn-for-making-item-based-cxn (cxn-inventory utterance-form-constraints meaning meaning-representation-formalism)
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        do (when (eql (attr-val cxn :cxn-type) 'holophrase)
             (let* ((non-overlapping-meanings (multiple-value-list (diff-meaning-networks meaning (extract-meaning-predicates cxn) meaning-representation-formalism)))
                    (non-overlapping-meaning-observation (first non-overlapping-meanings))
                    (non-overlapping-meaning-cxn (second non-overlapping-meanings))
                    (overlapping-meaning-observation (set-difference meaning non-overlapping-meaning-observation :test #'equal))
                    (overlapping-meaning-cxn (set-difference (extract-meaning-predicates cxn) non-overlapping-meaning-cxn :test #'equal))
                    (non-overlapping-form-observation (non-overlapping-form utterance-form-constraints cxn :nof-observation t))
                    (non-overlapping-form-cxn (non-overlapping-form utterance-form-constraints cxn :nof-cxn t))
                    (overlapping-form-cxn (set-difference (extract-form-predicates cxn) non-overlapping-form-cxn :test #'equal))
                    (overlapping-form-observation (set-difference utterance-form-constraints non-overlapping-form-observation :test #'equal)))
               (when (and
                      (> (length overlapping-meaning-observation) 0)
                      (> (length overlapping-meaning-cxn) 0)
                      (> (length non-overlapping-meaning-observation) 0)
                      (> (length non-overlapping-meaning-cxn) 0)
                      (> (length non-overlapping-form-observation) 0)
                      (> (length non-overlapping-form-cxn) 0)
                      (check-meets-continuity non-overlapping-form-cxn)
                      (check-meets-continuity non-overlapping-form-observation)
                      (equivalent-irl-programs?
                       (substitute-slot-meets-constraints non-overlapping-form-observation overlapping-form-observation)
                       (substitute-slot-meets-constraints non-overlapping-form-cxn overlapping-form-cxn)))
                 (return (values non-overlapping-meaning-observation
                                 non-overlapping-meaning-cxn
                                 non-overlapping-form-observation
                                 non-overlapping-form-cxn
                                 overlapping-meaning-observation
                                 overlapping-meaning-cxn
                                 overlapping-form-observation
                                 overlapping-form-cxn
                                 cxn)))))))

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
                  (syn-cat (gl::lex-class ,holistic-slot-lex-class))) into contributing-units
        collect `(,holistic-cxn-unit-name
                  (args (,arg))
                  --
                  (boundaries
                   (left ,leftmost-unit-holistic-cxn)
                   (right ,rightmost-unit-holistic-cxn))) into conditional-units
        finally (return (values conditional-units contributing-units))))

(defun create-type-hierarchy-links (lex-cxns item-based-name placeholders
                                             &key item-based-numeric-tail)
  "Creates all TH links for matching lexical cxns using their original lex-class."
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


; todo: replace the below with a fn that returns the open variables:

(defgeneric equivalent-meaning-networks (m1 m2 mode))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :irl)))
  (equivalent-irl-programs? m1 m2))

(defmethod equivalent-meaning-networks (m1 m2  (mode (eql :amr)))
  (amr::equivalent-amr-predicate-networks m1 m2))

(defun compare-irl-predicates (predicate-1 predicate-2 network-1 network-2)
  ; todo: find subnetworks of open vars, then use equivalent-irl-programs? on those! these subnetworks can have a depth larger than 1!
  (let* ((open-vars-1 (third (multiple-value-list (extract-vars-from-irl-network (list predicate-1)))))
         (open-vars-2 (third (multiple-value-list (extract-vars-from-irl-network (list predicate-2)))))
         (predicate-unification-bindings (unify-irl-programs (list predicate-1) (list predicate-2)))
         (sub-predicate-1 (get-irl-predicate-from-in-var (first open-vars-1) network-1))
         (sub-predicate-2 (get-irl-predicate-from-in-var (first open-vars-2) network-2))
         (sub-predicate-unification-bindings (unify-irl-programs (list sub-predicate-1)
                                                                 (list sub-predicate-2))))
    (cond ((and (not (or open-vars-1 open-vars-2))
                predicate-unification-bindings)
           ;; no open vars, and it unifies
           (values t nil nil))
             
          ((and open-vars-1
                open-vars-2
                predicate-unification-bindings
                sub-predicate-unification-bindings)
           ;; both have open vars, and the bound predicates unify
           (values t sub-predicate-1 sub-predicate-2))
          )))

(defmethod diff-meaning-networks (network-1 network-2 (mode (eql :amr)))
  (multiple-value-bind (n1-diff n2-diff bindings) (amr::diff-amr-networks network-1 network-2)
    (values n1-diff n2-diff)))


(defmethod diff-meaning-networks (network-1 network-2 (mode (eql :irl)))
  "traverse both networks, return the overlapping predicates, assumes the network to be linear, and the variables to have a fixed position"
  ;todo: identify all subnetworks, resolve them, then loop through parent network and combine resolved result with subnetwork
  (loop with current-predicate-1 = (get-predicate-with-target-var network-1)
        with current-predicate-2 = (get-predicate-with-target-var network-2)
        with last-equivalent-predicate-1 = current-predicate-1
        with overlapping-predicates-1 = nil
        with overlapping-predicates-2 = nil
        with rest-network-1 = (set-difference network-1 overlapping-predicates-1)
        with rest-network-2 = (set-difference network-2 overlapping-predicates-2)
        while (or current-predicate-1 current-predicate-2)
        for next-predicate-1 = (get-next-irl-predicate current-predicate-1 rest-network-1)
        for next-predicate-2 = (get-next-irl-predicate current-predicate-2 rest-network-2)
        do (multiple-value-bind (equivalent-predicates-p bind-1 bind-2)
               (compare-irl-predicates current-predicate-1 current-predicate-2 rest-network-1 rest-network-2)
             (if equivalent-predicates-p
               (progn ;; true condition
                 (setf last-equivalent-predicate-1 current-predicate-1) ;; keep track of last successful comparison
                 (push current-predicate-1 overlapping-predicates-1)
                 (push current-predicate-2 overlapping-predicates-2)
                 (when bind-1 (push bind-1 overlapping-predicates-1))
                 (when bind-2 (push bind-2 overlapping-predicates-2))
                 (setf current-predicate-1 next-predicate-1)
                 (setf current-predicate-2 next-predicate-2))
               ;; false condition
               (setf current-predicate-1 next-predicate-1)) ; traverse network 1 while network 2 stays static
             (when (and (not current-predicate-1) current-predicate-2) ;; stack 1 is empty, stack 2 is not so go back to the last equivalent predicate, and take the next predicate
               (setf current-predicate-1 (get-next-irl-predicate last-equivalent-predicate-1 rest-network-1))
               (setf current-predicate-2 next-predicate-2)))
               
        finally (return (values (set-difference network-1 overlapping-predicates-1)
                                (set-difference network-2 overlapping-predicates-2)))))

(defun substitute-predicate-bindings (predicate bindings)
  (loop with frame-bindings = (irl::map-frame-bindings bindings)
        for elt in predicate
        for assoc-res = (assoc elt frame-bindings)
        collect (if assoc-res
                  (cdr assoc-res)
                 elt)))

(defun commutative-irl-subset-diff (network-1 network-2)
  "given two networks where one is a superset of the other, return non-overlapping and overlapping predicates
   if the shortest is subtracted from the longest"
  (let* ((longest-network (if (< (length network-1) (length network-2))
                            network-2
                            network-1))
         (shortest-network (if (< (length network-1) (length network-2))
                             network-1
                             network-2))            
         (bindings (first (irl::embedding shortest-network longest-network)))
         (overlapping-predicates (if bindings
                                   (loop for predicate in shortest-network
                                       collect (substitute-predicate-bindings predicate bindings))
                                   nil))
         (non-overlapping-predicates (set-difference longest-network overlapping-predicates :test #'equal)))
    (values non-overlapping-predicates overlapping-predicates)))



;(extract-args-from-meaning-networks '((eh ?e)  (:mode ?e expressive)) '((:polarity ?e ?a) (amr-unknown ?a)) :amr)




(defgeneric extract-args-from-meaning-networks (child-meaning parent-meaning mode))

(defmethod extract-args-from-meaning-networks (child-meaning parent-meaning (mode (eql :irl)))
  (extract-args-from-irl-network child-meaning))

;(extract-args-from-meaning-network '((i ?i) (:mod ?j ?k)) :amr)

(defmethod extract-args-from-meaning-networks (child-meaning parent-meaning (mode (eql :amr)))
  "look up the vars from the child network in the parent network, if found, it's an arg that connects"
  (loop for el in (remove-duplicates (apply 'concatenate 'list child-meaning))
        when (and (variable-p el)
                  (find el (apply 'concatenate 'list parent-meaning)))
                 collect el))

(defun extract-args-from-irl-network (irl-network)
  "return all unbound variables as list"
  (sort irl-network #'string-lessp :key (lambda (predicate) ;; TODO: get rid of sort, do search until connected meaning goal test succeeds instead
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

(defun get-predicate-with-target-var (irl-program)
  "Find the predicate with the target var, given an irl program"
  (find (get-target-var irl-program) irl-program :test #'member))

(defun get-next-irl-predicate (predicate irl-program)
  "Find the next predicate, given a variable"
  (let ((in-var (first (multiple-value-list (extract-vars-from-irl-network (list predicate))))))
    (find in-var (remove predicate irl-program) :test #'member)))

(defun get-irl-predicate-from-in-var (var irl-program)
  "Find the next predicate, given an input variable"
  (loop for predicate in irl-program
        for in-var = (first (multiple-value-list (extract-vars-from-irl-network (list predicate))))
        when (equal var in-var)
        return predicate))

(defun disable-meta-layer-configuration (cxn-inventory)
  (set-configuration cxn-inventory :category-linking-mode :path-exists-ignore-transitive-closure)
  (set-configuration cxn-inventory :update-categorial-links nil)
  (set-configuration cxn-inventory :use-meta-layer nil)
  (set-configuration cxn-inventory :consolidate-repairs nil))

(defun enable-meta-layer-configuration (cxn-inventory)
  (set-configuration cxn-inventory :category-linking-mode :neighbours)
  (set-configuration cxn-inventory :update-categorial-links t)
  (set-configuration cxn-inventory :use-meta-layer t)
  (set-configuration cxn-inventory :consolidate-repairs t)
  (set-configuration cxn-inventory :parse-goal-tests '(:non-gold-standard-meaning)))

(defmethod get-best-partial-analysis-cipn ((utterance string) (original-cxn-inventory fcg-construction-set) (mode (eql :optimal-form-coverage)))
  (disable-meta-layer-configuration original-cxn-inventory) ;; also relaxes cat-network-lookup to path-exists without transitive closure!
  (set-configuration original-cxn-inventory :parse-goal-tests '(:no-applicable-cxns))
    (with-disabled-monitor-notifications
      (let* ((comprehension-result (multiple-value-list (comprehend-all utterance :cxn-inventory original-cxn-inventory)))
             (cip-nodes (second comprehension-result)))
        (enable-meta-layer-configuration original-cxn-inventory)
        (first (sort cip-nodes #'< :key #'(lambda (cipn) (length (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car cipn)))) 'form))))))))

(defmethod get-best-partial-analysis-cipn ((utterance string) (original-cxn-inventory fcg-construction-set) (mode (eql :optimal-form-coverage-exclude-item-based)))
  (disable-meta-layer-configuration original-cxn-inventory) ;; also relaxes cat-network-lookup to path-exists without transitive closure!
  (set-configuration original-cxn-inventory :parse-goal-tests '(:no-applicable-cxns))
    (with-disabled-monitor-notifications
      (let* ((temp-inventory (remove-cxns-with-phrase-type 'item-based (copy-object original-cxn-inventory)))
             (comprehension-result (multiple-value-list (comprehend-all utterance :cxn-inventory temp-inventory)))
             (cip-nodes (second comprehension-result)))
        (enable-meta-layer-configuration original-cxn-inventory)
        (first (sort cip-nodes #'< :key #'(lambda (cipn) (length (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car cipn)))) 'form))))))))

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
        finally return cxn-inventory))