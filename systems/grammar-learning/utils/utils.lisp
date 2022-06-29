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

#|(defun add-boundaries-to-form-constraints (form-constraints boundaries &key placeholder-var)
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
    (if (= (length (remove-duplicates boundaries)) 1)
      (if (equal right-boundary (first (get-boundary-units form-constraints)))  
        ; the variable is at the beginning of an utterance when the right-boundary is the leftmost-boundary
        (values new-form-constraints (list left-var right-boundary))
        (progn (when matching-right-predicate
                 (setf (nth 1 matching-right-predicate) right-var))
          (values new-form-constraints (list left-boundary right-var))))
      ;; the boundaries are different anyway, don't touch them!
      (values form-constraints boundaries)
      )))
   |# 
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
(defun get-all-conditional-unit-lex-classes (cxn)
  (loop for unit in (subseq (conditional-part cxn) 1)
        for lex-class = (lex-class-item-based-apply-last unit)
        collect lex-class))


(defun get-all-unit-lex-classes (cxn)
  (loop for unit in (subseq (contributing-part cxn) 1)
        for lex-class = (lex-class-item-based unit)
        collect lex-class))

(defun get-conditional-unit-lex-classes (cxn)
  (loop for unit in (subseq (conditional-part cxn) 1)
        for lex-class = (lex-class-item-based-apply-last unit)
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

(defun lex-class-item-based-apply-last (unit)
  (let ((syn-cat (find 'syn-cat (comprehension-lock unit) :key #'first)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))

(defun extract-args-apply-last (cxn)
  (let* ((units (conditional-part cxn))
         (args (loop for unit in (rest units)
                     for formulation-lock = (formulation-lock unit)
                     for arg-list = (second (find 'args formulation-lock :key #'feature-name))
                     collect arg-list)))
    args))

(defun extract-args-from-holistic-cxn-apply-last (cxn)
  (second (find 'args (formulation-lock (first (conditional-part cxn))) :key #'feature-name)))


(defun extract-args-apply-first (cxn)
  (let* ((contributing-units (contributing-part cxn))
         (top-unit (cond ((eql 'holistic (attr-val cxn :cxn-type))
                          (first contributing-units))
                         ((eql 'item-based (attr-val cxn :cxn-type))
                          (loop for unit in contributing-units
                                if (eql 'item-based (second (find 'phrase-type (rest (find 'syn-cat (fcg::unit-structure unit) :key #'first)) :key #'first)))
                                  do (return unit))))))
    (assert top-unit)
    (second (find 'args (fcg::unit-structure top-unit) :key #'first))))

    


(defun lex-class-apply-last-cxn (cxn)
  "return the lex-class of a cxn"
  (let ((syn-cat (find 'syn-cat (comprehension-lock (last-elt (conditional-part cxn))) :key #'feature-name)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))

(defun lex-class-cxn (cxn)
  "return the lex-class of a cxn"
  (let ((syn-cat (find 'syn-cat (fcg::unit-structure (last-elt (contributing-part cxn))) :key #'feature-name)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))

(defun extract-contributing-lex-class (cxn)
  "return the lex-class of a cxn"
  (let ((syn-cat (find 'syn-cat (fcg::unit-structure (first (contributing-part cxn))) :key #'feature-name)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))
         
(defun get-cxn-boundaries-apply-first (cxn)
  (find 'boundaries (fcg::unit-structure (last-elt (contributing-part cxn))) :key #'feature-name))

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

(defun arg-is-part-of-meaning-p (arg-var meaning)
  (when (or (find arg-var (remove-bind-statements meaning) :key #'second)
            (find arg-var (remove-bind-statements meaning) :key #'third))
    t))

(defun find-cxn-by-form-and-meaning (form meaning args-list cxn-inventory &key cxn-type cxn-set)
  "returns a cxn with the same meaning and form if it's in the cxn-inventory"
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        for cxn-type-cxn = (attr-val cxn :cxn-type)
        for cxn-args = (extract-args-apply-last cxn)
        when (and
              (if cxn-type (equal cxn-type cxn-type-cxn) t)
              (if cxn-set (equal cxn-set (attr-val cxn :label)) t)
              (irl:equivalent-irl-programs? form (extract-form-predicates cxn))
              (irl:equivalent-irl-programs? meaning (extract-meaning-predicates cxn))
              (if args-list
                (equal (loop for args in args-list
                             for left-res = (arg-is-part-of-meaning-p (first args) meaning)
                             for right-res = (arg-is-part-of-meaning-p (second args) meaning)
                             append (list left-res right-res))
                       (loop for args in cxn-args
                             for left-res = (arg-is-part-of-meaning-p (first args) (extract-meaning-predicates cxn))
                             for right-res = (arg-is-part-of-meaning-p (second args) (extract-meaning-predicates cxn))
                             append (list left-res right-res)))                           
                t)
              ;; check args: look up if the first arg is in the meaning representation, or if the second arg is in the meaning representation - this order should match!
              
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

(defmethod make-cxn-name ((form-constraints list) (cxn-inventory fcg-construction-set)
                          &key (add-cxn-suffix t) (add-numeric-tail nil))
  "Transform a list of form constraints into a suitable construction name"
  (let ((new-string-constraints (variablify-missing-form-strings form-constraints)))
    (make-cxn-name (format nil "~{~a~^-~}"
                           (render (append form-constraints new-string-constraints)
                                   (get-configuration cxn-inventory :render-mode)))
                   cxn-inventory :add-cxn-suffix add-cxn-suffix :add-numeric-tail add-numeric-tail)))

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
        finally (return item-based-meaning)))

(defun make-item-based-name-form-constraints-from-units (item-based-cxn-form-constraints resulting-units)
  (loop with item-based-fc = item-based-cxn-form-constraints
        for unit in resulting-units
        for fc = (variablify-form-constraints-with-constants (unit-feature-value unit 'form))
        do (setf item-based-fc (substitute-slot-meets-constraints fc item-based-fc))
        finally (return item-based-fc)))

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

(defun find-subset-holistic-cxn (cxn-inventory utterance-form-constraints meaning meaning-representation-formalism)
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        do (when (and (eql (attr-val cxn :cxn-type) 'holistic)
                      (eql (attr-val cxn :label) 'fcg::routine))
             
             (let* ((non-overlapping-meanings (multiple-value-list (diff-meaning-networks meaning (extract-meaning-predicates cxn) meaning-representation-formalism)))
                    (non-overlapping-meaning-observation (first non-overlapping-meanings))
                    (non-overlapping-meaning-cxn (second non-overlapping-meanings))
                    (overlapping-meaning-observation (set-difference meaning non-overlapping-meaning-observation :test #'equal))
                    
                    (nof-obs-and-cxn (multiple-value-list (diff-form-constraints utterance-form-constraints (extract-form-predicates cxn))))
                    (non-overlapping-form-observation (first nof-obs-and-cxn))
                    (non-overlapping-form-cxn (second nof-obs-and-cxn))
                    (overlapping-form-observation (set-difference utterance-form-constraints non-overlapping-form-observation :test #'equal))
                    ;; args
                    (args-holistic-cxn
                     (extract-args-from-meaning-networks non-overlapping-meaning-observation overlapping-meaning-observation meaning-representation-formalism)))
               (when (and
                      overlapping-meaning-observation
                      non-overlapping-meaning-observation
                      (not non-overlapping-meaning-cxn)
                      non-overlapping-form-observation
                      (not non-overlapping-form-cxn)
                      overlapping-form-observation
                      (<= (length args-holistic-cxn) 2) ; check if the meaning network is continuous
                      cxn
                      (check-meets-continuity non-overlapping-form-observation)
                      )
                 (return (values
                          cxn
                          non-overlapping-form-observation
                          non-overlapping-meaning-observation      
                          overlapping-form-observation
                          overlapping-meaning-observation       
                          )))))))
(defun find-superset-holistic-cxn (cxn-inventory utterance-form-constraints meaning meaning-representation-formalism)
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        do (when (and (eql (attr-val cxn :cxn-type) 'holistic)
                      (eql (attr-val cxn :label) 'fcg::routine))
             
             (let* ((non-overlapping-meanings (multiple-value-list (diff-meaning-networks (extract-meaning-predicates cxn) meaning meaning-representation-formalism)))
                    (non-overlapping-meaning-observation (first non-overlapping-meanings))
                    (non-overlapping-meaning-cxn (second non-overlapping-meanings))
                    (overlapping-meaning-observation (set-difference (extract-meaning-predicates cxn) non-overlapping-meaning-observation :test #'equal))
                    
                    (nof-obs-and-cxn (multiple-value-list (diff-form-constraints (extract-form-predicates cxn) utterance-form-constraints)))
                    (non-overlapping-form-observation (first nof-obs-and-cxn))
                    (non-overlapping-form-cxn (second nof-obs-and-cxn))
                    (overlapping-form-observation (set-difference (extract-form-predicates cxn) non-overlapping-form-observation :test #'equal))
                    ;; args
                    (args-holistic-cxn
                     (extract-args-from-meaning-networks non-overlapping-meaning-observation overlapping-meaning-observation meaning-representation-formalism)))
               (when (and
                      overlapping-meaning-observation
                      non-overlapping-meaning-observation
                      (not non-overlapping-meaning-cxn)
                      non-overlapping-form-observation
                      (not non-overlapping-form-cxn)
                      overlapping-form-observation
                      (<= (length args-holistic-cxn) 2) ; check if the meaning network is continuous
                      cxn
                      (check-meets-continuity non-overlapping-form-observation)
                      )
                 (return (values
                          cxn
                          non-overlapping-form-observation
                          non-overlapping-meaning-observation      
                          overlapping-form-observation
                          overlapping-meaning-observation       
                          )))))))
(defun find-superset-holophrase-cxn (cxn-inventory gold-standard-meaning utterance meaning-representation-formalism)
  ;; todo: check only routine cxns, do all processing after the when!
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
        for overlapping-form = (set-difference (extract-form-predicates cxn) non-overlapping-form :test #'equal)
        for overlapping-meaning = (set-difference (extract-meaning-predicates cxn) non-overlapping-meaning :test #'equal)
        for args-holistic-cxn = (extract-args-from-meaning-networks non-overlapping-meaning overlapping-meaning meaning-representation-formalism)   
        when (and (eql cxn-type 'holistic) ; todo: we might want to remove this!
                  non-overlapping-form
                  non-overlapping-meaning
                  (<= (length args-holistic-cxn) 2) ; check if the meaning network is continuous
                  (not non-overlapping-form-inverted) ; the set diff of smaller - larger = nil
                  (not non-overlapping-meaning-inverted)
                  (check-meets-continuity non-overlapping-form))
        ;; needs to be a holophrase, the form constraints for string and precedes constraints need to be a subset of the cxn, the meaning constraints need to be a subset too (todo: see if this is really the case in IRL)
        return (values cxn
                       non-overlapping-form
                       non-overlapping-meaning
                       overlapping-form
                       overlapping-meaning
                       args-holistic-cxn
                       )))


(defun find-meets-constraints (superset-with-meets subset-without-meets)
  (loop for fc in subset-without-meets
        for var = (second fc)
        for superset-meets-constraints = (extract-form-predicate-by-type superset-with-meets 'meets)
        for meet = (or (find var superset-meets-constraints :key #'second)
                       (find var superset-meets-constraints :key #'third))
        collect meet))

(defun select-cxn-for-making-item-based-cxn (cxn-inventory utterance-form-constraints meaning meaning-representation-formalism)
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        do (when (and (eql (attr-val cxn :cxn-type) 'holistic)
                      (eql (attr-val cxn :label) 'fcg::routine))
             
             (let* ((non-overlapping-meanings (multiple-value-list (diff-meaning-networks meaning (extract-meaning-predicates cxn) meaning-representation-formalism)))
                    (non-overlapping-meaning-observation (first non-overlapping-meanings))
                    (non-overlapping-meaning-cxn (second non-overlapping-meanings))
                    (overlapping-meaning-observation (set-difference meaning non-overlapping-meaning-observation :test #'equal))
                    (overlapping-meaning-cxn (set-difference (extract-meaning-predicates cxn) non-overlapping-meaning-cxn :test #'equal))
                    (nof-obs-and-cxn (multiple-value-list (diff-form-constraints utterance-form-constraints (extract-form-predicates cxn))))
                    (non-overlapping-form-observation (first nof-obs-and-cxn))
                    (non-overlapping-form-cxn (second nof-obs-and-cxn))
                    (overlapping-form-cxn (set-difference (extract-form-predicates cxn) non-overlapping-form-cxn :test #'equal))
                    (overlapping-form-observation (set-difference utterance-form-constraints non-overlapping-form-observation :test #'equal))
                    ;; args
                    (args-holistic-cxn-1
                     (extract-args-from-meaning-networks non-overlapping-meaning-cxn overlapping-meaning-cxn meaning-representation-formalism))
                    (args-holistic-cxn-2
                     (extract-args-from-meaning-networks non-overlapping-meaning-observation overlapping-meaning-observation meaning-representation-formalism)))
               (when (and
                      (> (length overlapping-meaning-observation) 0)
                      (> (length overlapping-meaning-cxn) 0)
                      (> (length non-overlapping-meaning-observation) 0)
                      (> (length non-overlapping-meaning-cxn) 0)
                      (> (length non-overlapping-form-observation) 0)
                      (> (length non-overlapping-form-cxn) 0)
                      (> (length overlapping-form-observation) 0)
                      (<= (length args-holistic-cxn-1) 2) ; check if the meaning network is continuous
                      (<= (length args-holistic-cxn-2) 2) ; check if the meaning network is continuous
                      overlapping-form-cxn
                      cxn
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
                                 overlapping-form-observation
                                 cxn
                                 )))))))

(defun create-dummy-predicates-for-args (args-list)
  (loop for args in args-list
        collect (list 'dummy (second args) (first args))))
  

(defun select-item-based-cxn-for-making-item-based-cxn (cxn-inventory intermediary-item-based-cxn meaning-representation-formalism)
  (loop for cxn in (sort (constructions cxn-inventory) #'> :key #'(lambda (x) (attr-val x :score)))
        do (when (and
                  (eql (attr-val cxn :cxn-type) 'item-based)
                  (eql (attr-val cxn :label) 'fcg::routine))
             (let* ((cxn-args (extract-args-apply-last cxn)) ;; fill up the gaps created by args with a dummy predicate, remove it in the end
                    (cxn-dummy-predicates (create-dummy-predicates-for-args cxn-args))
                    (cxn-meaning (append cxn-dummy-predicates (extract-meaning-predicates cxn)))
                    (intermediary-item-based-form-constraints (extract-form-predicates intermediary-item-based-cxn))
                    (intermediary-args (extract-args-apply-last intermediary-item-based-cxn))
                    (intermediary-dummy-predicates (create-dummy-predicates-for-args intermediary-args))
                    (intermediary-item-based-meaning (append intermediary-dummy-predicates (extract-meaning-predicates intermediary-item-based-cxn)))
                    (non-overlapping-meanings (multiple-value-list (diff-meaning-networks intermediary-item-based-meaning cxn-meaning meaning-representation-formalism)))
                    (non-overlapping-meaning-observation (first non-overlapping-meanings))
                    (non-overlapping-meaning-cxn (second non-overlapping-meanings))
                    (overlapping-meaning-observation (set-difference (extract-meaning-predicates intermediary-item-based-cxn) non-overlapping-meaning-observation :test #'equal))
                    (overlapping-meaning-cxn (set-difference (extract-meaning-predicates cxn) non-overlapping-meaning-cxn :test #'equal))
                    (non-overlapping-form-observation (non-overlapping-form intermediary-item-based-form-constraints cxn :nof-observation t))
                    (non-overlapping-form-cxn (non-overlapping-form intermediary-item-based-form-constraints cxn :nof-cxn t))
                    (overlapping-form-cxn (set-difference (extract-form-predicates cxn) non-overlapping-form-cxn :test #'equal))
                    (overlapping-form-observation (set-difference intermediary-item-based-form-constraints non-overlapping-form-observation :test #'equal))
                    ;; args
                    (args-holistic-cxn-1
                     (extract-args-from-meaning-networks non-overlapping-meaning-cxn overlapping-meaning-cxn meaning-representation-formalism))
                    (args-holistic-cxn-2
                     (extract-args-from-meaning-networks non-overlapping-meaning-observation overlapping-meaning-observation meaning-representation-formalism)))
               (when (and
                      (> (length overlapping-meaning-observation) 0)
                      (> (length overlapping-meaning-cxn) 0)
                      (> (length non-overlapping-meaning-observation) 0)
                      (> (length non-overlapping-meaning-cxn) 0)
                      (> (length non-overlapping-form-observation) 0)
                      (> (length non-overlapping-form-cxn) 0)
                      (> (length overlapping-form-observation) 0)
                      (<= (length args-holistic-cxn-1) 2) ; check if the meaning network is continuous
                      (<= (length args-holistic-cxn-2) 2) ; check if the meaning network is continuous
                      overlapping-form-cxn
                      cxn
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
                                 ;overlapping-meaning-cxn
                                 overlapping-form-observation
                                 args-holistic-cxn-1
                                 args-holistic-cxn-2
                                 cxn
                                 )))))))

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



(defmethod diff-meaning-networks (network-1 network-2 (mode (eql :amr)))
  (multiple-value-bind (n1-diff n2-diff bindings) (amr::diff-amr-networks network-1 network-2)
    (values n1-diff n2-diff)))


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

(defun get-irl-predicate-from-in-var (var irl-program)
  "Find the next predicate, given an input variable"
  (loop for predicate in irl-program
        for in-var = (first (multiple-value-list (extract-vars-from-irl-network (list predicate))))
        when (equal var in-var)
        return predicate))

(defun disable-meta-layer-configuration (cxn-inventory)
  (set-configuration cxn-inventory :category-linking-mode :categories-exist)
  (set-configuration cxn-inventory :update-categorial-links nil)
  (set-configuration cxn-inventory :use-meta-layer nil)
  (set-configuration cxn-inventory :consolidate-repairs nil))

(defun enable-meta-layer-configuration (cxn-inventory)
  (set-configuration cxn-inventory :parse-goal-tests '(:no-strings-in-root :no-applicable-cxns :connected-semantic-network :connected-structure :non-gold-standard-meaning))
  (set-configuration cxn-inventory :category-linking-mode :neighbours)
  (set-configuration cxn-inventory :update-categorial-links t)
  (set-configuration cxn-inventory :use-meta-layer t)
  (set-configuration cxn-inventory :consolidate-repairs t))

(defun disable-meta-layer-configuration-item-based-first (cxn-inventory)
  (set-configuration cxn-inventory :parse-goal-tests '(:no-applicable-cxns))
  (set-configuration cxn-inventory :cxn-supplier-mode :hashed-and-scored-meta-layer-cxn-set-only)
  (set-configuration cxn-inventory :category-linking-mode :categories-exist)
  (set-configuration cxn-inventory :update-categorial-links nil)
  (set-configuration cxn-inventory :use-meta-layer nil)
  (set-configuration cxn-inventory :consolidate-repairs nil))

(defun enable-meta-layer-configuration-item-based-first (cxn-inventory)
  (set-configuration cxn-inventory :parse-goal-tests '(:no-strings-in-root :no-applicable-cxns :connected-semantic-network :connected-structure :non-gold-standard-meaning))
  (set-configuration cxn-inventory :cxn-supplier-mode :hashed-and-scored-routine-cxn-set-only)
  (set-configuration cxn-inventory :category-linking-mode :neighbours)
  (set-configuration cxn-inventory :update-categorial-links t)
  (set-configuration cxn-inventory :use-meta-layer t)
  (set-configuration cxn-inventory :consolidate-repairs t))

(defmethod get-best-partial-analysis-cipn ((form-constraints list) (gold-standard-meaning list) (original-cxn-inventory fcg-construction-set) (mode (eql :optimal-form-coverage)))
  (disable-meta-layer-configuration original-cxn-inventory) ;; also relaxes cat-network-lookup to path-exists without transitive closure!
  (set-configuration original-cxn-inventory :parse-goal-tests '(:no-applicable-cxns))
    (with-disabled-monitor-notifications
      (let* ((comprehension-result (multiple-value-list (comprehend-all form-constraints :cxn-inventory original-cxn-inventory)))
             (cip-nodes (discard-cipns-with-incompatible-meanings (second comprehension-result) (first comprehension-result) gold-standard-meaning)))
        (enable-meta-layer-configuration original-cxn-inventory)
        (first (sort cip-nodes #'sort-cipns-by-coverage-and-nr-of-applied-cxns)))))


(defmethod get-best-partial-analysis-cipn ((form-constraints list) (gold-standard-meaning list) (original-cxn-inventory fcg-construction-set) (mode (eql :optimal-form-coverage-item-based-first)))
  (disable-meta-layer-configuration-item-based-first original-cxn-inventory) ;; also relaxes cat-network-lookup to path-exists without transitive closure!
  
    (with-disabled-monitor-notifications
      (let* ((comprehension-result (multiple-value-list (comprehend-all form-constraints :cxn-inventory original-cxn-inventory)))
             (cip-nodes (discard-cipns-with-incompatible-meanings (second comprehension-result) (first comprehension-result) gold-standard-meaning)))
        (enable-meta-layer-configuration-item-based-first original-cxn-inventory)
        (first (sort cip-nodes #'sort-cipns-by-coverage-and-nr-of-applied-cxns)))))

(defun get-root-form-predicates (cipn)
  (unit-feature-value (get-root (left-pole-structure (car-resulting-cfs (cipn-car cipn)))) 'form))

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



(defun discard-cipns-with-incompatible-meanings (candidate-cip-nodes candidate-meanings gold-standard-meaning)
  (loop for cipn in candidate-cip-nodes
        for candidate-meaning in candidate-meanings
        for subset-meaning = (second (multiple-value-list (commutative-irl-subset-diff gold-standard-meaning candidate-meaning)))
        when subset-meaning
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

(defun create-item-based-cxn (cxn-inventory
                              overlapping-form
                              non-overlapping-form
                              overlapping-meaning
                              non-overlapping-meaning
                              meaning
                              meaning-representation-formalism
                              repair-name)             
  (let* (;; cxn names
         (cxn-name-item-based-cxn
          (make-cxn-name (substitute-slot-meets-constraints non-overlapping-form overlapping-form) cxn-inventory :add-numeric-tail t))
         (cxn-name-item-based-cxn-apply-last (intern (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-LAST")))
         (cxn-name-item-based-cxn-apply-first (intern (concatenate 'string (symbol-name cxn-name-item-based-cxn) "-APPLY-FIRST")))
         ;; slot boundaries (leftmost/rightmost)
         (slot-boundaries (get-boundary-units non-overlapping-form))
         (overlapping-form-and-rewritten-boundaries
          (multiple-value-list (add-boundaries-to-form-constraints overlapping-form slot-boundaries)))
         (overlapping-form-with-rewritten-boundaries (first overlapping-form-and-rewritten-boundaries))
         (rewritten-boundaries (second overlapping-form-and-rewritten-boundaries))
         (dummy-slot-fc (list (list 'fcg::meets (first rewritten-boundaries) (second rewritten-boundaries))))
         (rewritten-item-based-boundaries (get-boundary-units (append dummy-slot-fc overlapping-form-with-rewritten-boundaries)))
         
         ;; args
         (slot-args (extract-args-from-meaning-networks non-overlapping-meaning meaning meaning-representation-formalism))
         ;(alt-slot-args (extract-args-apply-first (last-elt (first cxns-and-links-holistic-part)))) ; this should work too!
         (item-based-args (extract-args-from-meaning-networks meaning nil meaning-representation-formalism))
         (existing-item-based-cxn-apply-last (find-cxn-by-form-and-meaning
                                              overlapping-form-with-rewritten-boundaries
                                              overlapping-meaning
                                              (list slot-args)
                                              cxn-inventory
                                              :cxn-type 'item-based
                                              :cxn-set 'fcg::routine))
         (existing-item-based-cxn-apply-first (when existing-item-based-cxn-apply-last
                                                (alter-ego-cxn existing-item-based-cxn-apply-last cxn-inventory)))
         ;; lex classes
         (lex-class-item-based-cxn
          (if existing-item-based-cxn-apply-first
            (extract-contributing-lex-class existing-item-based-cxn-apply-first)
            (make-lex-class (symbol-name cxn-name-item-based-cxn) :trim-cxn-suffix t)))
         (lex-class-item-based-cxn-slot
          (if existing-item-based-cxn-apply-first
            (lex-class-cxn existing-item-based-cxn-apply-first)
            (make-lex-class (concatenate 'string (symbol-name lex-class-item-based-cxn) "-(x)"))))
         (cxn-inventory-copy (copy-object cxn-inventory))
         (new-item-based-cxn-apply-last
          (or existing-item-based-cxn-apply-last 
              (second (multiple-value-list (eval
                                            `(def-fcg-cxn ,cxn-name-item-based-cxn-apply-last
                                                          ((?item-based-unit
                                                            (syn-cat (phrase-type item-based)
                                                                     (lex-class ,lex-class-item-based-cxn))
                                                            (boundaries
                                                             (left ,(first rewritten-item-based-boundaries))
                                                             (right ,(second rewritten-item-based-boundaries)))
                                                            (args ,item-based-args)
                                                            (subunits (?slot-unit)))
                                                           (?slot-unit 
                                                            (footprints (used-as-slot-filler)))
                                                           <-
                                                           (?item-based-unit
                                                            (HASH meaning ,overlapping-meaning)
                                                            --
                                                            (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                           (?slot-unit
                                                            (footprints (NOT used-as-slot-filler))
                                                            (args ,slot-args)
                                                            --
                                                            (footprints (NOT used-as-slot-filler))
                                                            (syn-cat (lex-class ,lex-class-item-based-cxn-slot))
                                                            (boundaries
                                                             (left ,(first rewritten-boundaries))
                                                             (right ,(second rewritten-boundaries)))
                                                            ))
                                                          :attributes (:label fcg::routine
                                                                       :cxn-type item-based
                                                                       :bare-cxn-name ,cxn-name-item-based-cxn
                                                                       :repair ,repair-name
                                                                       :meaning ,(loop for predicate in overlapping-meaning
                                                                                       unless (or
                                                                                               (equal (first predicate) 'get-context)
                                                                                               (equal (first predicate) 'bind))
                                                                                       return (first predicate))
                                                                       :string ,(third (find 'string overlapping-form :key #'first)))
                                                                           
                                                          :cxn-inventory ,cxn-inventory-copy))))))
         (new-item-based-cxn-apply-first
          (or existing-item-based-cxn-apply-first
              (second (multiple-value-list (eval
                                            `(def-fcg-cxn ,cxn-name-item-based-cxn-apply-first
                                                          ((?item-based-unit
                                                            (syn-cat (phrase-type item-based)
                                                                     (lex-class ,lex-class-item-based-cxn))
                                                            (boundaries
                                                             (left ,(first rewritten-item-based-boundaries))
                                                             (right ,(second rewritten-item-based-boundaries)))
                                                            (args ,item-based-args)
                                                            (subunits (?slot-unit)))
                                                           (?slot-unit
                                                            (footprints (used-as-slot-filler))
                                                            (syn-cat (phrase-type holistic)
                                                                     (lex-class ,lex-class-item-based-cxn-slot))
                                                            (args ,slot-args)
                                                            (boundaries
                                                             (left ,(first rewritten-boundaries))
                                                             (right ,(second rewritten-boundaries)))
                                                            )
                                                           <-
                                                           (?item-based-unit
                                                            (HASH meaning ,overlapping-meaning)
                                                            --
                                                            (HASH form ,overlapping-form-with-rewritten-boundaries))
                                                           )
                                                          :attributes (:label fcg::meta-only
                                                                       :cxn-type item-based
                                                                       :bare-cxn-name ,cxn-name-item-based-cxn
                                                                       :repair ,repair-name
                                                                       :meaning ,(loop for predicate in overlapping-meaning
                                                                                       unless (or
                                                                                               (equal (first predicate) 'get-context)
                                                                                               (equal (first predicate) 'bind))
                                                                                       return (first predicate))
                                                                       :string ,(third (find 'string overlapping-form :key #'first)))
                                                                           
                                                          :cxn-inventory ,cxn-inventory-copy)))))))
    (values new-item-based-cxn-apply-first
            new-item-based-cxn-apply-last
            lex-class-item-based-cxn
            lex-class-item-based-cxn-slot)))

(defun create-temp-cxn-inventory (original-cxn-inventory)
  (let ((inventory-name (gensym)))
    (eval `(def-fcg-constructions
                                       ,inventory-name
                                     :cxn-inventory ,inventory-name
                                     :hashed t
                                     :feature-types ((args sequence)
                                                     (form set-of-predicates)
                                                     (meaning set-of-predicates)
                                                     (subunits set)
                                                     (footprints set))
                                     :fcg-configurations ((:node-tests :restrict-nr-of-nodes :restrict-search-depth :check-duplicate)
                                                          (:cxn-supplier-mode . ,(get-configuration original-cxn-inventory :learner-cxn-supplier))
                                                          (:parse-goal-tests :no-strings-in-root :no-applicable-cxns :connected-semantic-network :connected-structure :non-gold-standard-meaning)
                                                          (:de-render-mode . ,(get-configuration original-cxn-inventory :de-render-mode))
                                                          (:parse-order routine)
                                                          (:max-nr-of-nodes . 250)
                                                          (:production-order routine)
                                                          (:meaning-representation-formalism . ,(get-configuration original-cxn-inventory :meaning-representation))
                                                          (:render-mode . :generate-and-test)
                                                          (:category-linking-mode . :categories-exist)
                                                          (:update-categorial-links . t)
                                                          (:consolidate-repairs . t)
                                                          (:use-meta-layer . nil)
                                                          (:update-categorial-links . nil)
                                                          (:consolidate-repairs . nil)
                                                          (:initial-categorial-link-weight . ,(get-configuration original-cxn-inventory :initial-categorial-link-weight))
                                                          (:ignore-transitive-closure . t)
                                                          (:hash-mode . :hash-string-meaning-lex-id))))))
        
