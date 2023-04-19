
(in-package :grammar-learning)

;; ------------------
;; + Util functions +
;; ------------------

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

(defun phrase-type (cxn)
  (loop for unit in (contributing-part cxn)
        for syn-cat = (cdr (find 'syn-cat (fcg::unit-structure unit) :key #'first))
        for phrase-type = (when syn-cat (second (find 'phrase-type syn-cat :key #'first)))
        when phrase-type
        return phrase-type))

(defun transient-structure-form-constraints (transient-structure)
  (remove-if-not #'(lambda (fc)
                     (equal 'string (first fc)))
                 (extract-forms (left-pole-structure transient-structure))))

(defun initial-transient-structure (node)
  (if (find 'fcg::initial (statuses node))
    (car-source-cfs (cipn-car node))
    (car-source-cfs (cipn-car (last-elt (all-parents node))))))

(defun diff-subset-superset-form (subset-cxn superset-form)
  (set-difference 
   superset-form
   (extract-form-predicates subset-cxn)
   :test #'irl:unify-irl-programs))

(defun lex-class (unit)
  (let* ((syn-cat (find 'syn-cat (unit-body unit) :key #'first))
         (lex-class (find 'lex-class (second syn-cat) :key #'first)))    
    (when lex-class
      (second lex-class))))

(defun lex-class-item-based (unit)
  (let ((syn-cat (find 'syn-cat (fcg::unit-structure unit) :key #'first)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))

(defun lex-class-cxn (lexical-cxn)
  (let ((syn-cat (find 'syn-cat (fcg::unit-structure (first (contributing-part lexical-cxn))) :key #'feature-name)))
    (second (find 'lex-class (rest syn-cat) :key #'first))))
         

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

(defun find-cxn-by-form-and-meaning (form meaning cxn-inventory)
  "returns a cxn with the same meaning and form if it's in the cxn-inventory"
  (loop for cxn in (constructions cxn-inventory)
        when (and (irl:equivalent-irl-programs? form (extract-form-predicates cxn))
                  (irl:equivalent-irl-programs? meaning (extract-meaning-predicates cxn)))
        return cxn))

(defun initial-node-p (node)
  "return t if node is initial node"
  (null (all-parents node)))

(defun add-cxn-suffix (string)
  (intern (string-append string "-CXN")))

(defun make-lex-class (cat-name &key add-numeric-tail)
  (intern
   (symbol-name
    (funcall (if add-numeric-tail #'make-const #'make-symbol)
             (upcase
              (if cat-name cat-name "CAT"))))
   :type-hierarchies))

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

;; (form-constraints-with-variables "what is the color of the cube" :de-render-string-meets-precedes)

(defun meaning-predicates-with-variables (meaning)
  "Transform meaning network with constants to meaning network with variables."
    (loop for predicate in meaning
          collect (if (equal (first predicate) 'bind)
                    (list (first predicate)
                          (second predicate)
                          (variablify (third predicate))
                          (fourth predicate))
                    (cons (first predicate)
                          (mapcar #'variablify (rest predicate))))))
        

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

(defun find-matching-lex-cxns (cxn-inventory observed-form gold-standard-meaning utterance)
  "return all lexical cxns that can apply by checking whether they are a subset of the observed form and meaning"
  ;; if a certain item matches twice, we'll discard it to avoid ambiguity
  ;; e.g.: is there a cylinder next to the blue cylinder? will only return blue (if in inventory), not cylinder
  (let ((remaining-form (form-predicates-with-variables observed-form)))
    (sort (loop for cxn in (constructions cxn-inventory)
                when (and (eql (phrase-type cxn) 'lexical) 
                          (irl:unify-irl-programs (extract-form-predicates cxn) remaining-form)
                          (setf remaining-form (set-difference remaining-form (extract-form-predicates cxn) :test #'irl:unify-irl-programs))
                          (irl:unify-irl-programs (extract-meaning-predicates cxn) gold-standard-meaning)
                          ;;we need to check if a cxn could match twice based on the meaning and discard these cases,
                          ;; if it matches multiple times, the size of the set diff will be larger than 1
                          (= 1 (- (length gold-standard-meaning)
                                  (length (set-difference gold-standard-meaning (extract-meaning-predicates cxn) :test #'irl:unify-irl-programs)))))   
                collect cxn)
          #'(lambda (x y)
              (<
               (search (third (first (extract-form-predicates x))) utterance)
               (search (third (first (extract-form-predicates y))) utterance))))))

#|
(defun diff-non-overlapping-form (observed-form matching-lex-cxns)
  "subtract all lexical forms from the gold standard"
  (let ((resulting-form observed-form)
        (lex-unit-names nil))
    (loop for lex-cxn in matching-lex-cxns
          for lex-cxn-form = (extract-form-predicates lex-cxn)
          do (let ((prev-res-form resulting-form))
               (setf resulting-form
                     (set-difference resulting-form lex-cxn-form
                                     :test #'irl:unify-irl-programs))
               (setf lex-unit-names
                     (append lex-unit-names
                             (list
                              (second
                               (find 'string
                                     (set-difference prev-res-form resulting-form
                                                     :test #'irl:unify-irl-programs)
                                     :key #'first)))))))
    (values lex-unit-names resulting-form)))
|#

#|
(defun diff-non-overlapping-meaning (gold-standard-meaning matching-lex-cxns)
    "subtract all lexical meanings (bind statements) from the gold standard"
  (let ((resulting-meaning gold-standard-meaning)
        (args nil))
    (loop for lex-cxn in matching-lex-cxns
          for lex-cxn-meaning = (first (extract-meaning-predicates lex-cxn))
          do (let ((prev-res-meaning resulting-meaning))
               ;; problem! set difference will remove both instances if there are identical meanings (e.g cylinder & cylinders)
               ;; solution: take only one e.g with find and remove by index
               ;(setf resulting-meaning (remove lex-cxn-meaning resulting-meaning :test #'equal :count 1))
               (setf resulting-meaning
                     (set-difference resulting-meaning (list lex-cxn-meaning)
                                     :test #'irl:unify-irl-programs))
               ;(setf args (append args (list (third lex-cxn-meaning))))))
               (setf args
                     (append args
                             (list
                              (third
                               (find 'bind (set-difference prev-res-meaning resulting-meaning
                                                           :test #'irl:unify-irl-programs)
                                     :key #'first)))))))
    (values args resulting-meaning)))
|#
 
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
 


(defun subunit-names-for-lex-cxns (lex-cxns)
  (loop for lex-cxn in lex-cxns
        for lex-cxn-form = (extract-form-predicates lex-cxn)
        for lex-cxn-unit-name = (second (find 'string lex-cxn-form :key #'first))
        collect lex-cxn-unit-name))

(defun subunit-blocks-for-lex-cxns (lex-cxns lex-subunit-names args th-links)
  (loop for lex-cxn in lex-cxns
        for arg in args
        for lex-cxn-unit-name in lex-subunit-names
        for th-link in th-links
        for lex-slot-lex-class = (cdr th-link)
        collect `(,lex-cxn-unit-name
                  (syn-cat (gl::lex-class ,lex-slot-lex-class))) into contributing-units
        collect `(,lex-cxn-unit-name
                  (args (,arg))
                  --) into conditional-units
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

(defun find-matching-lex-cxns-in-root (cxn-inventory root-strings)
  (remove nil (loop for remaining-form in root-strings
        for root-string = (third remaining-form)
        collect (loop for cxn in (constructions cxn-inventory)
                      when (and (eql (phrase-type cxn) 'lexical)
                                (string= (third (first (extract-form-predicates cxn))) root-string))
                      return cxn))))

(defun subtract-lex-cxn-meanings (lex-cxns gold-standard-meaning)
  (let ((lex-cxn-meanings (map 'list #'extract-meaning-predicates lex-cxns)))
    (loop for lex-cxn-meaning in lex-cxn-meanings
          do (setf gold-standard-meaning (set-difference gold-standard-meaning lex-cxn-meaning :test #'irl:unify-irl-programs)))
    gold-standard-meaning))

(defun subtract-lex-cxn-forms (lex-cxns string-predicates-in-root)
    (loop for lex-cxn in lex-cxns
          for lex-form = (extract-form-predicates lex-cxn)
          do (setf string-predicates-in-root (set-difference string-predicates-in-root lex-form :test #'irl:unify-irl-programs)))
    string-predicates-in-root)

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
