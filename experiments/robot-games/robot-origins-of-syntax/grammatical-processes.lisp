;;;; /grammatical-processes.lisp

(in-package :roos)

;; -----------------
;; + Conceptualise +
;; -----------------

(defun categorise-all-objects-all-channels (objects channels ontology)
  (loop for object in objects
        for all-categories = (loop for channel in channels
                                   for channel-categories = (find-data ontology channel)
                                   for cat/dist = (categorise object channel-categories)
                                   collect (cons channel (first cat/dist)))
        collect (cons (id object) all-categories)))

(defun discriminate-objects (topic-ids all-ids all-objects)
  "For each object in list-of-objects, this function returns a minimal set of discriminative feature-values."
  (loop for topic-id in topic-ids
        for attr-combination = (discriminate-object (rest (assoc topic-id all-objects))
                                                    (loop for id in (remove topic-id all-ids)
                                                          collect (rest (assoc id all-objects))))
        collect (cons topic-id (mapcar #'rest attr-combination))))

(defun discriminate-object (topic-features rest-features)
  "Returns the minimal amount of (attr . val) conses that
   discriminates object from the objects in list-of-objects.
   Make sure the object is not in list-of-objects, otherwise
   the functions will logically return nil."
  (loop for nr-of-attr-needed from 1 to (length topic-features)
        do
        (let ((attr-combinations (shuffle (combinations-of-length topic-features nr-of-attr-needed))))
          (loop for attr-combination in attr-combinations
                when (discriminative-combination? attr-combination rest-features)
                do (return-from discriminate-object attr-combination)))))

(defun discriminative-combination? (list-of-attributes list-of-object-attributes)
  "Returns t if the attribute combination in list-of-attributes does not occur in any of the list-of-object-attributes."
  (let ((unique t)) ;; unique until opposite is proven
    (loop for object-attributes in list-of-object-attributes
          while unique
          do
          (let ((attribute-booleans (loop for attribute in list-of-attributes
                                          collect (eql (cdr attribute)
                                                       (cdr (assoc (car attribute) object-attributes))))))
            (when (apply #'always attribute-booleans)
              (setf unique nil))))
    unique))

(defun objects-w-categories->predicates (objects-w-categories-list)
  (loop for (id . categories) in objects-w-categories-list
        append (object-w-categories->predicate id categories)))

(defun object-w-categories->predicate (object-id categories)
  (loop for (channel . category) in categories
        collect (list channel (id category) object-id)))

(define-event gr-conceptualise-finished (discriminatory-features list))
(define-event could-not-conceptualise)

(defmethod run-process (process
                        (process-label (eql 'gr-conceptualise))
                        task
                        agent)
  "Find discriminatory features for all topics in the scene"
  ;; For all objects in the scene;
  ;;   For all channels;
  ;;     Find a category
  ;; Look for combinations of categories that
  ;; make the topics unique
  (let* ((prev-process-input (input process))
         (scene (find-data prev-process-input 'scene))
         (topic-ids (find-data prev-process-input 'topic-ids))
         (all-ids (mapcar #'id (entities scene)))
         (channels (get-configuration agent :features))
         all-objects-all-categorised
         discriminatory-features
         (discriminatory? t))
    (setf all-objects-all-categorised
          (categorise-all-objects-all-channels (entities scene)
                                               channels
                                               (ontology agent)))
    (set-data (blackboard (grammar agent))
              :scene-predicates
              (objects-w-categories->predicates all-objects-all-categorised))
    (setf discriminatory-features
          (discriminate-objects topic-ids all-ids all-objects-all-categorised))
    (loop for (id . features) in discriminatory-features
          when (null features)
          do (setf discriminatory? nil))
    (if discriminatory?
      (progn
        (notify gr-conceptualise-finished discriminatory-features)
        (make-process-result 1 (list (cons 'discriminatory-features discriminatory-features))
                             :process process))
      (progn
        (notify could-not-conceptualise)
        (speak agent "I could not conceptualize the topics")
        nil))))

;; -----------
;; + Produce +
;; -----------

(defclass multiple-hypotheses-formulation-diagnostic (diagnostic)
  ((trigger :initform 'multiple-hypotheses-formulation))
  (:documentation "Diagnostic"))

(defclass multiple-hypotheses-problem (problem)
  ((problematic-cipn :accessor problematic-cipn :initarg :problematic-cipn :initform nil))
  (:documentation "Problem"))

(defclass add-cxn-or-th-link-formulation (repair)
  ((trigger :initform 'multiple-hypotheses-formulation))
  (:documentation "Repair"))

(defclass syntax-fix (fix)
  ()
  (:documentation "Fix"))

(defmethod diagnose ((diagnostic multiple-hypotheses-formulation-diagnostic)
                     process-result
                     &key trigger)
  (declare (ignorable trigger))
  (let ((cipn (find-data process-result 'cipn)))
    (when (field? (goal-test-data cipn) :number-of-hypotheses-formulation)
      (make-instance 'multiple-hypotheses-problem :problematic-cipn cipn))))

(defun objects-categories (lexical-units)
  "From a list of lexical units, returns ((obj1 cat1 cat2) (obj2 cat3))"
  (let ((objects-cats (mapcar #'get-object-category lexical-units))
        (aggregated-objects-cats nil))
    (loop for o-c in objects-cats
          do
          (if (assoc (car o-c) aggregated-objects-cats)
            (push (cdr o-c) (cdr (assoc (car o-c) aggregated-objects-cats)))
            (push (list (car o-c) (cdr o-c)) aggregated-objects-cats)))
  aggregated-objects-cats))

(defun get-object-category (lexical-unit)
  "Unit-> (obj-1 . cat-144)"
  (let* ((object (caadr (assoc 'args (cdr lexical-unit))))
         (category (lexical-unit->category lexical-unit)))
   (cons object category)))

(defun categories-connected-through-type-hierarchy (categories categories-cxn type-hierarchy &key (ordered nil))
  "Returns true if for every category there exists a path from each category to each categorie-cxn in the type-hierarchy"
  (and (= (length categories) (length categories-cxn))
       (if ordered
         ;; Requirement: categories are connected through th in that order
         (loop for cat in categories
               for cat-cxn in categories-cxn
               always (and (type-hierarchies::directed-path-p (intern (symbol-name cat) :type-hierarchies)
                                                         (intern (symbol-name cat-cxn) :type-hierarchies)
                                                         type-hierarchy)
                       ;    (< (link-weight (intern (symbol-name cat) :type-hierarchies)
                        ;                (intern (symbol-name cat-cxn) :type-hierarchies)
                        ;                type-hierarchy) 1.0)
                           ))
               
         ;; Categories are connected through th in any order
         (loop for cat-permuation in (permutations-of-length categories (length categories))
               when (categories-connected-through-type-hierarchy cat-permuation categories-cxn type-hierarchy :ordered t)
               do (return t)))))

(defun anti-unification-categorisation (cxn transient-structure-units direction cxn-inventory)
  "Calls the function for processing cxn"
  (anti-unification-categorisation-processing-cxn (get-processing-cxn cxn) transient-structure-units direction (processing-cxn-inventory cxn-inventory)))

(defun anti-unification-categorisation-processing-cxn (cxn transient-structure-units direction cxn-inventory)
  ""
  (let* ((matching-pattern (fcg::matching-pattern cxn direction))
         (a-u-results (fcg::anti-unify-origins-of-syntax matching-pattern transient-structure-units cxn-inventory :fcg-with-type-hierarchy))
        (a-u-result-lowest-score (first a-u-results))
        (resulting-pattern (first a-u-result-lowest-score))
        (resulting-th-links (fourth a-u-result-lowest-score))
        (cost (sixth a-u-result-lowest-score)))
    (when resulting-pattern
      (list resulting-pattern resulting-th-links cost))))

(defun clean-up-form-constraints (lexical-units-and-root)
  "we just want meets constraints with the units involved"
  (let* ((root (get-root lexical-units-and-root))
         (lexical-units (remove 'root lexical-units-and-root :key #'unit-name))
         (form-constraints (extract-forms (list root)))
         (relevant-meets-constraints (loop for f-c in form-constraints
                                           when (and (string= (first f-c) "MEETS")
                                                     (find (second f-c) lexical-units :key #'unit-name :test #'string=)
                                                     (find (third f-c) lexical-units :key #'unit-name :test #'string=))
                                           collect f-c))
         (ordered-relevant-meets-constraints (loop for unit in lexical-units
                                                   for unit-name = (unit-name unit)
                                                   collect (find unit-name relevant-meets-constraints :key #'second :test #'string=)))
         (new-root `(FCG:ROOT (FCG:SYN-CAT NIL)
                              (FCG:SEM-CAT NIL)
                              (FCG:FORM ,ordered-relevant-meets-constraints))))
    (append (list new-root) lexical-units)))

(defun reuse-and-adapt-existing-cxns-for-categorisation-formulation (cipn object-id categories cxn-inventory)
  "Returns nil if nothing could be done, and true if cxns could be reused and adapted.
   The side effects of these function change temp-cxn-set (and we count on that!)"
  (let* ((transient-structure (car-resulting-cfs (cipn-car cipn)))
         (direction (direction (cip cipn)))
         (lexical-units-and-root (loop for unit in (remove-grammatical-units (left-pole-structure transient-structure))
                                       when (or (eql 'root (unit-name unit))
                                                (eql object-id (lexical-unit->object-id unit)))
                                       collect unit))
         (grammatical-constructions (grammatical-cxns-with-n-units cxn-inventory (length lexical-units-and-root)))
         (a-u-results (remove nil
                              (loop for cxn in grammatical-constructions
                                    collect (anti-unification-categorisation cxn lexical-units-and-root direction cxn-inventory))))
         (best-a-u-result (first (sort a-u-results #'< :key #'third))))
    ;; assert (> 1 (length grammatical-constructions)))
    (cond ((or (not grammatical-constructions) (= (length categories) 1))
           nil)
          (t
           (loop for (source . pattern) in (second best-a-u-result)
                 do
                 (add-categories (list source pattern) (get-type-hierarchy cxn-inventory))
                 (add-link source pattern (get-type-hierarchy cxn-inventory))
                 finally (return t))))))

(defun grammatical-cxns-with-n-units (cxn-inventory n)
  "Returns the grammatical constructions with n units on the conditional part. "
    (loop for cxn in (remove-lexical-constructions (constructions cxn-inventory))
          when (= n (length (conditional-part cxn)))
          collect cxn))

(define-event formulation-repair-triggered)

(defmethod repair ((repair add-cxn-or-th-link-formulation)
                   (problem multiple-hypotheses-problem)
                   (object process-result)
                   &key trigger)
  "Repair by making a new categorisation-cxn."
  (declare (ignorable trigger))
  (let* ((agent (owner (task (process object))))
         (problematic-cipn (problematic-cipn problem))
         (lexical-units (remove-non-word-units (left-pole-structure (car-resulting-cfs (cipn-car problematic-cipn)))))
         (objects-categories (objects-categories lexical-units))
         (temp-cxn-set (copy-object (grammar agent))))
    (set-data (blackboard temp-cxn-set)
              :scene-predicates
              (get-data (blackboard (grammar agent)) :scene-predicates))
    (loop for (object . cats) in objects-categories
          when (> (length cats) 1)
          do
          (cond ;; An equivalent cxn is found: do nothing
                ((find-cxn cats temp-cxn-set 
                           :key #'(lambda (cxn)
                                    (attr-val cxn :categories))
                           :test #'(lambda (cats cats-cxn)
                                     (categories-connected-through-type-hierarchy cats cats-cxn (get-type-hierarchy temp-cxn-set) :ordered nil))))
                ;; See whether existing cxns can be adapted, using anti- and pro-unification
                ;; True is returned if side-effects change temp-cxn-set, nil otherwise  
                ((reuse-and-adapt-existing-cxns-for-categorisation-formulation problematic-cipn object cats temp-cxn-set))
                ;; Else: make a completely new cxn
                (t
                 (make-and-add-categorisation-cxns (shuffle cats) temp-cxn-set))))
    ;; Create the repair
    (notify formulation-repair-triggered)
    (make-instance 'syntax-fix
                   :repair repair
                   :problem problem
                   :restart-data temp-cxn-set)))

(defmethod handle-fix ((fix syntax-fix)
                       (repair repair)
                       (problem problem)
                       (object process-result)
                       &key &allow-other-keys)
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  ; (call-next-method)
  (push fix (fixes (problem fix)))
  (let* ((node (problematic-cipn problem))
         (initial-cfs (initial-cfs (cip node))))
      (set-data fix :fixed-cars
                (fcg-apply (processing-cxn-inventory (restart-data fix)) initial-cfs (direction (cip node))))      
      ;;(assert (find 'fcg::succeeded (statuses (get-data fix :fixed-cars))))
      (unless (find 'fcg::succeeded (statuses (get-data fix :fixed-cars)))
        (format t "Learning failed in ~a." (if (equal (direction (cip node)) '->) "formulation" "comprehension")))
      ))

(define-event gr-produce-started (meaning-network list))
(define-event gr-produce-finished (utterance list))
(define-event type-hierarchy-updated (type-hierarchy type-hierarchy))

(defmethod run-process (process
                        (process-label (eql 'gr-produce))
                        task
                        agent)
  (let* ((prev-process-input (input process))
         (discriminatory-features (find-data prev-process-input 'discriminatory-features))
         (meaning-network (loop for (id . attrs) in discriminatory-features
                                append (loop for cat in attrs
                                              collect (category->predicate cat id))))
         process-result)
    (notify gr-produce-started meaning-network)
    (multiple-value-bind (utterance cipn)
        (fcg:formulate meaning-network :cxn-inventory (grammar agent))
      (setf process-result
            (make-process-result 1 (list (cons 'utterance utterance)
                                         (cons 'applied-cxns (mapcar #'get-original-cxn (applied-constructions cipn)))
                                         (cons 'cipn cipn))
                                 :process process))
      (multiple-value-bind (new-problems new-fixes)
          (notify-learning process-result :trigger 'multiple-hypotheses-formulation)
        (declare (ignorable new-problems))
        (cond ((null new-fixes)
               (notify gr-produce-finished utterance)
               process-result)
              ((= (length new-fixes) 1)
               (let ((repaired-cipn (get-data (first new-fixes) :fixed-cars)))
                 (if (not (field? (goal-test-data repaired-cipn) :number-of-hypotheses-formulation))
                   (progn
                     (dolist (cxn (constructions (construction-inventory repaired-cipn)))
                       (unless (find-same-cxn (get-original-cxn cxn) (grammar agent))
                         (add-cxn (get-original-cxn cxn) (grammar agent))))
                     (setf (type-hierarchies::graph (get-type-hierarchy (grammar agent)))
                           (type-hierarchies::graph (get-type-hierarchy (construction-inventory repaired-cipn))))
                     (notify type-hierarchy-updated (get-type-hierarchy (grammar agent)))
                     (set-data process-result 'cipn repaired-cipn)
                     (set-data process-result 'utterance (render (car-resulting-cfs (cipn-car repaired-cipn))
                                                                 (get-configuration (construction-inventory repaired-cipn) :render-mode)))
                     (notify gr-produce-finished (get-data process-result 'utterance))
                     process-result)
                   (progn
                     (notify gr-produce-finished utterance)
                     process-result)))))))))
        

;; ---------
;; + Parse +
;; ---------

(define-event gr-parse-finished (meaning-network list))

(defmethod run-process (process
                        (process-label (eql 'gr-parse))
                        task
                        agent)
  (let* ((prev-process-input (input process))
         (utterance (find-data prev-process-input 'utterance))
         (scene (find-data prev-process-input 'scene))
         (channels (get-configuration agent :features))
         (all-objects-all-channels (categorise-all-objects-all-channels (entities scene)
                                                                        channels
                                                                        (ontology agent)))
         (scene-predicates (objects-w-categories->predicates all-objects-all-channels)))
    (set-data (blackboard (grammar agent))
              :scene-predicates
              scene-predicates)
    (multiple-value-bind (meaning cipn)
        (fcg:comprehend utterance :cxn-inventory (grammar agent))
      (notify gr-parse-finished meaning)
      (make-process-result 1 (list (cons 'applied-cxns (mapcar #'get-original-cxn (applied-constructions cipn)))
                                   (cons 'meaning meaning)
                                   (cons 'cipn cipn))
                           :process process))))

;; -------------
;; + Interpret +
;; -------------

(define-event gr-interpret-finished (topic-ids list))

(defmethod run-process (process
                        (process-label (eql 'gr-interpret))
                        task
                        agent)
  (let* ((prev-process-input (input process))
         (scene-predicates (get-data (blackboard (grammar agent)) :scene-predicates))
         (meaning (find-data prev-process-input 'meaning))
         (all-possible-topics (get-possible-topics-for-meaning-in-world meaning scene-predicates))
         (topic-ids (when (= (length all-possible-topics) 1)
                      (first all-possible-topics))))
    (notify gr-interpret-finished all-possible-topics)
    (make-process-result 1 (list (cons 'topic-ids topic-ids))
                         :process process)))

;; -------------------
;; + Hearer Learning +
;; -------------------

(defclass multiple-hypotheses-comprehension-diagnostic (diagnostic)
  ((trigger :initform 'multiple-hypotheses-comprehension))
  (:documentation "Diagnostic"))

(defclass add-cxn-or-th-link-comprehension (repair)
  ((trigger :initform 'multiple-hypotheses-comprehension))
  (:documentation "Repair"))

(defmethod diagnose ((diagnostic multiple-hypotheses-comprehension-diagnostic)
                     process-result
                     &key trigger)
  (declare (ignorable trigger))
  (let ((cipn (find-data process-result 'cipn)))
    (when (field? (goal-test-data cipn) :number-of-hypotheses-comprehension)
      (make-instance 'multiple-hypotheses-problem :problematic-cipn cipn))))

(defun all-features-in-object-p (string-cat-meaning-tuples)
  "Returns true if a string-cat-meaning tuple is consistent with object."
  (let ((all-present t))
    (loop for tuple in string-cat-meaning-tuples
          for meaning = (third tuple)
          for categorised-meaning = (fourth tuple)
          when (not (equal-entity meaning categorised-meaning))
          do (setf all-present nil))
    all-present))

(defun filter-feature-combinations (possible-feature-combinations)
  "Returns those feature-combinations that are consistent with object"
    (loop for p-f-c in possible-feature-combinations
          when (all-features-in-object-p p-f-c)
          collect p-f-c))

(defun no-shorter-element (element list)
  "Returns true if list contains no shorter element."
  (if (null list)
    t
    (let ((shortest-elt-in-list (reduce #'min (mapcar #'length list))))
      (if (<= (length element) shortest-elt-in-list)
        t
        nil))))

(defun discriminative-string-cat-meaning-tuple (list-of-tuples list-of-objects agent)
  "Returns t if the attribute combination in list-of-attributes does not occur in any of the list-of-object-attributes."
  (let ((unique t)) ;; unique until opposite is proven
    (loop for object in list-of-objects
          while unique
          do
          (let ((attribute-booleans (loop for tuple in list-of-tuples
                                          for meaning = (third tuple)
                                          for categorised-meaning = (fourth tuple)
                                          for categorised-distance = (fifth tuple)
                                          for channel = (get-channel meaning)
                                          for all-categories-of-channel = (find-data (ontology agent) channel)
                                          for cat/dist = (categorise object all-categories-of-channel)
                                          collect (when (equal-entity (first cat/dist) categorised-meaning)
                                                    (> categorised-distance (rest cat/dist))))))
            (when (apply #'always attribute-booleans)
              (setf unique nil))))
    unique))

(defun categorise-pfcs (possible-feature-combinations object agent)
  (loop for pfc in possible-feature-combinations
        collect (loop for tuple in pfc
                      for meaning = (third tuple)
                      for channel = (get-channel meaning)
                      for all-categories-of-channel = (find-data (ontology agent) channel)
                      for cat/dist = (categorise object all-categories-of-channel)
                      collect (append tuple (list (first cat/dist) (rest cat/dist))))))

(defun no-form-repetitions (possible-feature-combination)
  (let ((all-forms (mapcar (lambda (tuple)
                             (first tuple))
                           possible-feature-combination)))
    (not (duplicates? all-forms :test #'string=))))

(defun discriminate-object-using-words (object list-of-objects possible-feature-combinations agent)
  "Returns all shortest possible-feature-combinations that are discriminative for object in list-of-objects."
  (let ((pfcs-to-return nil)
        (pfcs-with-distance (categorise-pfcs (copy-object possible-feature-combinations) object agent)))
    (loop for p-f-c in (filter-feature-combinations pfcs-with-distance)
          do
          ;(when (and (no-shorter-element p-f-c pfcs-to-return) ;; THIS NO LONGER HOLDS IN THE ROBOT WORLD!
          ;           (discriminative-string-cat-meaning-tuple p-f-c list-of-objects agent))
          (when (and (no-form-repetitions p-f-c)
                     (discriminative-string-cat-meaning-tuple p-f-c list-of-objects agent))
            (push p-f-c pfcs-to-return)))
    (reverse pfcs-to-return)))

(defun select-feature-combination-by-utterance (feature-combinations list-of-strings)
  "Returns a feature-combination that is coherent with the word order in list-of-strings."
  (loop for f-c in feature-combinations
        for permutations = (permutations-of-length f-c (length f-c))
        when (find list-of-strings
                   permutations
                   :test #'equalp
                   :key #'(lambda (permutation)
                            (remove-if-not #'stringp (flatten permutation))))
        return f-c))

(defun discriminate-objects-using-words (list-of-objects larger-list-of-objects string-cat-meaning-tuples agent)
  "For each object in list-of-objects, this function returns a minimal set of discriminative feature-values using
   the words in list-of-strings in their order. It is your responsibility to ensure that this is possible!"
  (let* ((possible-feature-combinations (remove nil (sort (all-subsequences string-cat-meaning-tuples) #'utils::list-length<l)))
         (possible-feature-combinations-per-object (loop for object in list-of-objects
                                                         collect (discriminate-object-using-words object
                                                                                                  (remove object larger-list-of-objects)
                                                                                                  possible-feature-combinations
                                                                                                  agent)))
         (possible-feature-combinations-all-objects (apply #'cartesian-product possible-feature-combinations-per-object)))
    (if (= (length possible-feature-combinations-all-objects) 1)
      (first possible-feature-combinations-all-objects)
      (select-feature-combination-by-utterance possible-feature-combinations-all-objects (mapcar #'car string-cat-meaning-tuples)))))

(defun transient-structure-lexical-cxns-only (cipn)
  (labels ((only-lexical-cxns (cipn)
             (apply #'always (mapcar #'lexical-cxn-p (applied-constructions cipn)))))
    (if (only-lexical-cxns cipn)
      (car-resulting-cfs (cipn-car cipn))
      (loop for parent in (all-parents cipn)
            when (only-lexical-cxns parent)
            return (car-resulting-cfs (cipn-car parent))))))

(defun unit-meets-unit (left-unit right-unit meets-constraints)
  "returns true if left-unit meets right-unit according to the meets constraints."
  (loop with left-unit-name = (unit-name left-unit)
        with right-unit-name = (unit-name right-unit)
        for mc in meets-constraints
        when (and (equalp left-unit-name (second mc))
                  (equalp right-unit-name (third mc)))
        return t))

(defun lexical-units-for-string-cat-meaning-tuples (units string-cat-meaning-tuples)
  (labels ((find-lexical-units (units string-cat-meaning-tuples)
             (loop with current-unit = nil
                   with meets-constraints = (remove-if-not #'(lambda (constraint)
                                                               (eql (first constraint) 'meets))
                                                           (extract-forms units))
                   with lexical-units = (remove-grammatical-units units)
                   for scmt in string-cat-meaning-tuples
                   for unit = (loop for unit in lexical-units
                                    when (and (string= (lexical-unit->string unit) (first scmt))
                                              (if current-unit
                                                (unit-meets-unit current-unit unit meets-constraints)
                                                t))
                                    ;append unit into units-to-return
                                    ;and
                                    do
                                    (progn (setf current-unit unit)
                                      (return unit)))
                   when unit
                   collect unit)))
    (append (list (get-root units))
            (the-biggest #'length (list (find-lexical-units units string-cat-meaning-tuples)
                                        (find-lexical-units (reverse units) string-cat-meaning-tuples))))))

(defun reuse-and-adapt-existing-cxns-for-categorisation-comprehension (cipn categories string-cat-meaning-tuples cxn-inventory)
  "Returns nil if nothing could be done, and true if cxns could be reused and adapted.
   The side effects of these function change temp-cxn-set (and we count on that!)"
  (let* ((transient-structure (transient-structure-lexical-cxns-only cipn))
         (direction (direction (cip cipn)))
         (lexical-units-and-root (lexical-units-for-string-cat-meaning-tuples
                                   (left-pole-structure transient-structure)
                                  string-cat-meaning-tuples))
         (lexical-units-and-root-cleaned-up-form-constraints (clean-up-form-constraints lexical-units-and-root))
         (grammatical-constructions (grammatical-cxns-with-n-units cxn-inventory (length lexical-units-and-root)))
         (a-u-results (remove nil
                              (loop for cxn in grammatical-constructions
                                    collect (anti-unification-categorisation cxn lexical-units-and-root-cleaned-up-form-constraints direction cxn-inventory))))
         (best-a-u-result (first (sort a-u-results #'< :key #'third))))
    ;;(assert (> 1 (length grammatical-constructions)))
    (cond ((or (not grammatical-constructions) (= (length categories) 1))
           nil)
          (t
           (loop for (source . pattern) in (second best-a-u-result)
                 do
                 (add-categories (list source pattern) (get-type-hierarchy cxn-inventory))
                 (add-link source pattern (get-type-hierarchy cxn-inventory))
                 finally (return t))))))

(defun replace-meanings-with-prototypes (string-cat-meaning-tuples agent)
  (loop for tuple in string-cat-meaning-tuples
        for meaning = (third tuple)
        for meaning-cat = (find-category meaning agent)
        collect (list (first tuple)
                      (second tuple)
                      meaning-cat)))

(define-event comprehension-repair-triggered)
(define-event could-not-discriminate)

(defmethod repair ((repair add-cxn-or-th-link-comprehension)
                   (problem multiple-hypotheses-problem)
                   (object process-result)
                   &key trigger)
  "Repair by making a new n-gram-cxn."
  (declare (ignorable trigger))
  (let* ((problematic-cipn (problematic-cipn problem))
         (agent (owner (task (process object))))
         (topic-ids (find-data object 'observed-topic-ids))
         (scene (find-data object 'scene))
         (topic-objs (mapcar (lambda (id) (find-entity-by-id scene id)) topic-ids))
         (utterance (find-data object 'utterance))
         (lexical-units (remove-non-word-units (left-pole-structure (car-resulting-cfs (cipn-car problematic-cipn)))))
         (string-cat-meaning-tuples (get-string-cat-meaning-tuples-for-utterance utterance lexical-units))
         (string-cat-prototype-tuples (replace-meanings-with-prototypes string-cat-meaning-tuples agent))
         (grouped-string-cat-meaning-tuples (discriminate-objects-using-words topic-objs
                                                                              (entities scene)
                                                                              string-cat-prototype-tuples
                                                                              agent))
         (categories (mapcar #'(lambda (list)
                                 (loop for sublist in list
                                       collect (second sublist)))
                             grouped-string-cat-meaning-tuples))
         (temp-cxn-set (copy-object (grammar agent))))
    (set-data (blackboard temp-cxn-set)
              :scene-predicates
              (get-data (blackboard (grammar agent)) :scene-predicates))

    (if (null categories)
      (progn
        (notify could-not-discriminate)
        (speak agent "I could not find unique topics")
        (make-instance 'fix))
      (progn
        ;; Add new cxns
        (loop for cats in categories
              for string-cat-meaning-tuples in grouped-string-cat-meaning-tuples
              when (> (length cats) 1)
              do
              ;; An equivalent cxn is found: do nothing
              (cond ((find-cxn cats temp-cxn-set
                               :key #'(lambda (cxn)
                                        (attr-val cxn :categories))
                               :test #'(lambda (cats cats-cxn)
                                         (categories-connected-through-type-hierarchy cats cats-cxn (get-type-hierarchy temp-cxn-set) :ordered t))))
                    ((reuse-and-adapt-existing-cxns-for-categorisation-comprehension problematic-cipn cats string-cat-meaning-tuples temp-cxn-set))
                    ;; Else: make a completely new cxn
                    (t
                     (make-and-add-categorisation-cxns cats temp-cxn-set))))
        ;; Return the repair
        (make-instance 'syntax-fix
                       :repair repair
                       :problem problem
                       :restart-data temp-cxn-set)))))

(defmethod run-process (process
                        (process-label (eql 'gr-hearer-learning))
                        task
                        agent)
  (let* ((prev-process-input (input process))
         (success (find-data prev-process-input 'communicated-successfully))
         process-result)
    (when (hearer? agent)
      (if (not success)
        (let ((hearer-cipn (find-data prev-process-input 'cipn)))
          (setf process-result (make-process-result 1 (list (cons 'cipn hearer-cipn))
                                                    :process process))
          (multiple-value-bind (new-problems new-fixes)
              (notify-learning process-result :trigger 'multiple-hypotheses-comprehension)
            (declare (ignore new-problems))
            (if (and (listp new-fixes)
                     (eql (type-of (first new-fixes)) 'syntax-fix))
              (let ((repaired-cipn (get-data (first new-fixes) :fixed-cars)))
                (if (not (field? (goal-test-data repaired-cipn) :number-of-hypotheses-comprehension))
                  (progn
                    (dolist (cxn (constructions (construction-inventory repaired-cipn)))
                      (unless (find-same-cxn (get-original-cxn cxn) (grammar agent))
                        (add-cxn (get-original-cxn cxn) (grammar agent))))
                    (setf (type-hierarchies::graph (get-type-hierarchy (grammar agent)))
                          (type-hierarchies::graph (get-type-hierarchy (construction-inventory repaired-cipn))))
                    (notify type-hierarchy-updated (get-type-hierarchy (grammar agent)))
                    (set-data process-result 'cipn repaired-cipn)
                    process-result)
                  process-result))
              process-result)))
        (make-process-result 1 nil :process process)))))

;; -----------------
;; + Consolidation +
;; -----------------

(defun used-th-links (solution-cipn)
  (let* (;; All cip nodes that were created by the application of a non-lexical cxn
         (cip-nodes-through-cat-cxn (cons solution-cipn
                                          (remove-if #'(lambda (cipn)
                                                   (or (not (applied-constructions cipn))
                                                       (lexical-cxn-p (first (applied-constructions cipn)))))
                                                   (all-parents solution-cipn))))
         (cipn-cars (mapcar #'cipn-car cip-nodes-through-cat-cxn)))
    (mappend #'used-th-links-cipn-car cipn-cars)))

(defun competing-th-links-cipn-car (cipn-car)
  (loop with applied-cxn = (car-applied-cxn cipn-car)
        with construction-categories = (attr-val applied-cxn :categories)
        with resulting-ts = (left-pole-structure (car-resulting-cfs cipn-car))
        for binding in (car-match-bindings cipn-car)
        for cxn-unit = (find (car binding) (left-pole-structure applied-cxn) :key #'unit-name :test #'equalp)
        for ts-unit = (find (cdr binding) resulting-ts :key #'unit-name :test #'equalp)
        when (and cxn-unit ts-unit)
        ;; For each used-binding, collect competing ones
        append (loop with used-lex-cat = (lexical-unit->category ts-unit)
                     for non-used-cxn-cat in (remove (cxn-unit->category cxn-unit) construction-categories)
                     collect (cons used-lex-cat non-used-cxn-cat))))

(defun competing-th-links (solution-cipn)
  (let* (;; All cip nodes that were created by the application of a non-lexical cxn
         (cip-nodes-through-cat-cxn (cons solution-cipn
                                          (remove-if #'(lambda (cipn)
                                                   (or (not (applied-constructions cipn))
                                                       (lexical-cxn-p (first (applied-constructions cipn)))))
                                                   (all-parents solution-cipn))))
         (cipn-cars (mapcar #'cipn-car cip-nodes-through-cat-cxn))
         (competing-th-links (mappend #'competing-th-links-cipn-car cipn-cars)))
    (loop with type-hierarchy = (get-type-hierarchy (construction-inventory solution-cipn))
          for c-t-l in competing-th-links
          when (and (node-p (car c-t-l) type-hierarchy)
                    (node-p (cdr c-t-l) type-hierarchy)
                    (directed-path-p (car c-t-l) (cdr c-t-l) type-hierarchy))
          collect c-t-l)))

(defun reward-link (lex-cat gramm-cat type-hierarchy &optional delta (lower-bound 0.01))
  (let* ((current-weight (link-weight lex-cat gramm-cat type-hierarchy))
         (new-weight (- current-weight delta)))
    (set-link-weight lex-cat gramm-cat type-hierarchy (max lower-bound new-weight))))

(defun punish-link (lex-cat gramm-cat type-hierarchy &optional delta (upper-bound 1.0))
  (let* ((current-weight (link-weight lex-cat gramm-cat type-hierarchy))
         (new-weight (+ current-weight delta)))
    (set-link-weight lex-cat gramm-cat type-hierarchy (min upper-bound new-weight))))


(define-event type-hierarchy-rewarded (agent roos-agent) (th-links list))
(define-event type-hierarchy-punished (agent roos-agent) (th-links list))

(defun find-best-topic (category objects agent)
  (let* ((channel (get-channel category))
         (channel-categories (find-data (ontology agent) channel)))
    (the-smallest (lambda (obj)
                    (let ((cat/dist (categorise obj channel-categories)))
                      (abs (rest cat/dist))))
                  objects)))

(defun align-agent-lexicon (agent lexical-cxns topics success)
  (let ((li-inc (get-configuration agent :li-inc))
        (li-dec (get-configuration agent :li-dec))
        (alpha (get-configuration agent :alignment-rate))
        all-punished-cxns
        all-shifted-cats)
    (if success
      (loop for applied-cxn in lexical-cxns
            for applied-category = (find-category (attr-val applied-cxn :meaning) agent)
            for corresponding-topic = (find-best-topic applied-category topics agent)
            do (let ((punished-cxns (dec-competitor-score applied-cxn agent :delta li-dec)))
                 (inc-score applied-cxn :delta li-inc)
                 (shift-category applied-category corresponding-topic :alpha alpha)
                 (setf all-punished-cxns (append punished-cxns all-punished-cxns))
                 (push applied-category all-shifted-cats))
            finally (progn
                      (notify category-alignment (remove-duplicates all-shifted-cats :key #'id))
                      (notify lexicon-alignment
                              (mapcar #'get-original-cxn lexical-cxns)
                              (when all-punished-cxns (mapcar #'get-original-cxn all-punished-cxns)))))    
      (when (speaker? agent)
        (loop for applied-cxn in lexical-cxns
              do (dec-score applied-cxn agent :delta li-dec)
              finally (notify lexicon-alignment
                              nil
                              (mapcar #'get-original-cxn lexical-cxns)))))))

(defun align-agent (agent success process)
  (let* ((prev-process-input (input process))
         (cipn (find-data prev-process-input 'cipn))
         (lexical-cxns (remove-duplicates (all-lexical-constructions (applied-constructions cipn))
                                          :key #'name
                                          :test #'string=))
         (topic-ids (find-data prev-process-input 'observed-topic-ids))
         (scene (find-data prev-process-input 'scene))
         (topics (mapcar (lambda (id) (find-entity-by-id scene id)) topic-ids))
         (li-incf-weight (get-configuration agent :li-incf-weight))
         (li-decf-weight (get-configuration agent :li-decf-weight)))
    (if success
      (progn
        (notify alignment-started)
        (when (get-configuration agent :align-lexicon-when-learning-grammar?)
          (align-agent-lexicon agent lexical-cxns topics success))
        (loop for (lex-cat . gramm-cat) in (used-th-links cipn)
              do (reward-link lex-cat gramm-cat (get-type-hierarchy (grammar agent)) li-decf-weight)
              ;; Reward the type-hierarchy-links used
              finally (notify type-hierarchy-rewarded agent (used-th-links cipn)))
        ;; Loop through competitors and punish them
        (loop for (lex-cat . gramm-cat) in (competing-th-links cipn)
              do (punish-link lex-cat gramm-cat (get-type-hierarchy (grammar agent)) li-incf-weight)
              ;; Reward the type-hierarchy-links used
              finally (notify type-hierarchy-punished agent (competing-th-links cipn))))
      ;; Communicative Failure: 
      (when (speaker? agent)
        (notify alignment-started)
        (when (get-configuration agent :align-lexicon-when-learning-grammar?)
          (align-agent-lexicon agent lexical-cxns topics success))
        ;; If speaker, loop through the used th-links and punish them.
        (loop for (lex-cat . gramm-cat) in (used-th-links cipn)
              do (punish-link lex-cat gramm-cat (get-type-hierarchy (grammar agent)) li-incf-weight)
              finally (notify type-hierarchy-punished agent (used-th-links cipn)))))))

(defmethod run-process (process
                        (process-label (eql 'gr-consolidate))
                        task
                        agent)
  (let* ((prev-process-input (input process))
         (success (find-data prev-process-input 'communicated-successfully))
         (who-aligns? (get-configuration agent :who-aligns)))
    (case who-aligns?
      (:speaker (when (speaker? agent)
                  (align-agent agent success process)))
      (:hearer (when (hearer? agent)
                 (align-agent agent success process)))
      (:both (align-agent agent success process)))
    (make-process-result 1 nil :process process)))

;;;;;;;;;;;;;;;;
;; Goal Tests ;;
;;;;;;;;;;;;;;;;

(defun get-possible-topics-for-meaning-in-world (meaning scene-predicates)
  "Returns possible topics for meaning in scene."
  (let* ((bindings (unify (cons '== meaning) scene-predicates))
         possible-topics)
    (loop for binding in bindings
          for topic-object-ids = (mapcar #'cdr binding)
          do (push topic-object-ids possible-topics))
    (remove-duplicates possible-topics :test #'permutation-of?)))

(defmethod cip-goal-test ((node cip-node)
                          (mode (eql :single-interpretation-in-world-formulation)))
  "Checks whether the resulting meaning network has only 1 interpretation in the world."
  (let* ((scene (get-data (blackboard (construction-inventory node)) :scene-predicates))
         ;(nr-of-topics (get-data (blackboard (construction-inventory node)) :nr-of-topics))
         (utterance (render (car-resulting-cfs (cipn-car node))
                            (get-configuration (construction-inventory node) :render-mode)))
         (fresh-cxn-inventory (copy-fcg-construction-set-without-cxns (original-cxn-set (construction-inventory node)))) 
         meaning
         possible-topics)
    ;; Add applied cxns to the fresh inventory
    (dolist (cxn (applied-constructions node))
      (unless (find-cxn (get-original-cxn cxn) fresh-cxn-inventory)
        (add-cxn (get-original-cxn cxn) fresh-cxn-inventory)))
    ;; comprehend
    (setf meaning (with-disabled-monitor-notifications
                    (comprehend utterance :cxn-inventory fresh-cxn-inventory)
                    )
          ) ;; comprehend-all???
    (setf possible-topics (get-possible-topics-for-meaning-in-world meaning scene))
  (if (= (length possible-topics) 1)
    t
    (progn
      (set-data (goal-test-data node) :number-of-hypotheses-formulation (length possible-topics))
      nil))))

(defmethod cip-goal-test ((node cip-node) (mode (eql :not-more-than-two-unit-structures)))
    (> 3 (second (multiple-value-list (fcg::connected-syntactic-structure (left-pole-structure (car-resulting-cfs (cipn-car node))))))))

(defmethod cip-goal-test ((node cip-node) (mode (eql :single-interpretation-in-world-comprehension)))
  "Checks whether the resulting meaning network has only 1 interpretation in the world."
  (let* ((scene (get-data (blackboard (construction-inventory node)) :scene-predicates))
         ;(nr-of-topics (get-data (blackboard (construction-inventory node)) :nr-of-topics))
         (meaning (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node)))))
         (possible-topics (get-possible-topics-for-meaning-in-world meaning scene)))
    (if (= (length possible-topics) 1)
      t
      (progn
        (set-data (goal-test-data node) :number-of-hypotheses-comprehension (length possible-topics))
        nil))))

;;;;;;;;;;;;;;;;
;; Queue Mode ;;
;;;;;;;;;;;;;;;;

(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :backtrack-over-grammatical-cxns-only)))
  (setf (priority node)  
        (cip-priority node (get-configuration cip :priority-mode)))
  (unless (priority node)
    (error "The priority of the new node is NIL. You used priority
mode ~a. Please check why it did not calculate a priority score." (get-configuration cip :priority-mode)))
  (if (lex-cxn-in-path-of-sibling-p node)
    (queue cip)
    (setf (queue cip) (sorted-insert (queue cip) node :key #'priority :test #'>))))

(defun lex-cxn-in-path-of-sibling-p (node)
  (when (and (parent node)
             (siblings node)
             (string= (attr-val (fcg::last-applied-construction node) :cxn-type) 'lexical-cxn))
    (let ((cxns-in-sibling-paths
           (loop for sibling in (siblings node)
                 for sibling-cxn = (fcg::last-applied-construction sibling)
                 append (remove sibling-cxn
                                (remove nil
                                        (traverse-depth-first sibling :collect-fn #'(lambda (node)
                                                                              (when (fully-expanded? node)
                                                                                (fcg::last-applied-construction node)))))
                                :key #'name :test #'equalp))))
      (find (name (fcg::last-applied-construction node)) cxns-in-sibling-paths :key #'name :test #'equalp))))

;;;;;;;;;;;;;;;;;;;
;; Priority Mode ;;
;;;;;;;;;;;;;;;;;;;

(defun used-th-links-cipn-car (cipn-car)
  (loop with applied-cxn = (left-pole-structure (car-applied-cxn cipn-car))
        with resulting-ts = (left-pole-structure (car-resulting-cfs cipn-car))
        for binding in (car-match-bindings cipn-car)
        for cxn-unit = (find (car binding) applied-cxn :key #'unit-name :test #'equalp)
        for ts-unit = (find (cdr binding) resulting-ts :key #'unit-name :test #'equalp)
        when (and cxn-unit ts-unit)
        collect (cons (lexical-unit->category ts-unit) (cxn-unit->category cxn-unit))))

(defmethod cip-priority ((node cip-node) (mode (eql :depth-first-with-type-hierachy-weights)))
  (if (all-parents node)
    (let* ((base-priority (+ 1.0  (priority (first (all-parents node)))))
           (cost-of-links (mapcar #'(lambda (link)
                                      (link-weight (car link)
                                                   (cdr link)
                                                   (get-type-hierarchy (construction-inventory node))))
                                  (used-th-links-cipn-car (cipn-car node)))))
      (if cost-of-links
        (+ base-priority (sum (mapcar #'(lambda (cost) (- 1.0 cost)) cost-of-links)))
        base-priority))
    0.0))

;;;;;;;;;;;;;;;;;;
;; Cxn-supplier ;;
;;;;;;;;;;;;;;;;;;


(defclass cxn-supplier-with-label-nr-of-categories-and-score (cxn-supplier-with-ordered-labels)
  ())

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label-nr-of-categories-and-score)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-with-label-nr-of-categories-and-score
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-label-nr-of-categories-and-score
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-by-nr-of-categories-and-score node (car labels)))))))

(defun all-constructions-of-label-by-nr-of-categories-and-score (node label)
  "Returns all constructions that of label 'label', order by number of categories and score."
  (let ((cxns-of-label (copy-object
                        (loop for cxn in (constructions-for-application (construction-inventory (cip node)))
                              for cxn-label = (attr-val cxn :label)
                              when (equalp (symbol-name label) (symbol-name cxn-label))
                              collect cxn))))
    (cond ((equalp (symbol-name label) "CXN")
           (sort cxns-of-label (lambda (cxn1 cxn2)
                                 (let ((nr-of-cats-cxn1 (length (attr-val cxn1 :categories)))
                                       (nr-of-cats-cxn2 (length (attr-val cxn2 :categories)))
                                       (score-cxn1 (attr-val cxn1 :score))
                                       (score-cxn2 (attr-val cxn2 :score)))
                                   (cond ((> nr-of-cats-cxn1 nr-of-cats-cxn2) t)
                                         ((and (= nr-of-cats-cxn1 nr-of-cats-cxn2) (> score-cxn1 score-cxn2)) t)
                                         (t nil))))))
          ((equalp (symbol-name label) "LEX")
           (sort cxns-of-label (lambda (cxn1 cxn2)
                                 (let ((score-cxn1 (attr-val cxn1 :score))
                                       (score-cxn2 (attr-val cxn2 :score)))
                                   (> score-cxn1 score-cxn2)))))
          (t cxns-of-label))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-label-nr-of-categories-and-score) (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. just return the next one
         (pop (remaining-constructions cxn-supplier)))
        ((loop for child in (children node)
               thereis (cxn-applied child))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         (setf (current-label cxn-supplier) (car (remaining-labels cxn-supplier)))
         (setf (remaining-labels cxn-supplier) (cdr (remaining-labels cxn-supplier)))
         (setf (all-constructions-of-current-label cxn-supplier)
               (all-constructions-of-label-by-nr-of-categories-and-score node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))

(defclass cxn-supplier-categorisation-type-hierarchy (cxn-supplier-with-ordered-labels)
  ())

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :cxn-supplier-categorisation-type-hierarchy)))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-categorisation-type-hierarchy
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-label-nr-of-categories-and-score
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-categorisation-type-hierarchy node (car labels)))))))

(defun all-constructions-of-label-categorisation-type-hierarchy (node label)
  "Returns all constructions that of label 'label', order by number of categories and score."
  (let ((cxns-of-label (copy-object
                        (loop for cxn in (constructions-for-application (construction-inventory (cip node)))
                              for cxn-label = (attr-val cxn :label)
                              when (equalp (symbol-name label) (symbol-name cxn-label))
                              collect cxn))))
    (cond ((equalp (symbol-name label) "CXN")
           (sort cxns-of-label (lambda (cxn1 cxn2)
                                 (let ((nr-of-cats-cxn1 (length (attr-val cxn1 :categories)))
                                       (nr-of-cats-cxn2 (length (attr-val cxn2 :categories)))
                                       (score-cxn1 (attr-val cxn1 :score))
                                       (score-cxn2 (attr-val cxn2 :score)))
                                   (cond ((> nr-of-cats-cxn1 nr-of-cats-cxn2) t)
                                         ((and (= nr-of-cats-cxn1 nr-of-cats-cxn2) (> score-cxn1 score-cxn2)) t)
                                         (t nil))))))
          ((equalp (symbol-name label) "LEX")
           (sort cxns-of-label (lambda (cxn1 cxn2)
                                 (let ((score-cxn1 (attr-val cxn1 :score))
                                       (score-cxn2 (attr-val cxn2 :score)))
                                   (> score-cxn1 score-cxn2)))))
          (t cxns-of-label))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-categorisation-type-hierarchy) (node cip-node))
  (cond ;; For lexical cxns: just pop
        ((and (remaining-constructions cxn-supplier)
              (string= (current-label cxn-supplier) 'lex))
         (pop (remaining-constructions cxn-supplier)))
        ;; For grammatical cxns: return all of that length
        ((remaining-constructions cxn-supplier)
         (let* ((first-cxn (pop (remaining-constructions cxn-supplier)))
                (next-constructions (cons first-cxn (loop with first-cxn-length = (length (conditional-part (get-original-cxn first-cxn)))
                                                          for cxn in (remaining-constructions cxn-supplier)
                                                          when (= first-cxn-length (length (conditional-part (get-original-cxn cxn))))
                                                          collect cxn))))
           ;;now we need to remove the next-constructions from the list of remaining constructions
           (setf (remaining-constructions cxn-supplier)
                 (loop for cxn in (remaining-constructions cxn-supplier)
                       unless (find (name cxn) next-constructions :key #'name :test #'equalp)
                       collect cxn))
           ;;return next constructions:
           next-constructions))
        ((loop for child in (children node)
               thereis (cxn-applied child))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         (setf (current-label cxn-supplier) (car (remaining-labels cxn-supplier)))
         (setf (remaining-labels cxn-supplier) (cdr (remaining-labels cxn-supplier)))
         (setf (all-constructions-of-current-label cxn-supplier)
               (all-constructions-of-label-by-nr-of-categories-and-score node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Categorisation cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-and-add-categorisation-cxns (cats cxn-set)
         (let ((new-cat-1 (intern (symbol-name (make-const "FIRST-SLOT")) :type-hierarchies))
               (new-cat-2 (intern (symbol-name (make-const "SECOND-SLOT")) :type-hierarchies))
               (new-cat-3 (intern (symbol-name (make-const "THIRD-SLOT")) :type-hierarchies)))
           (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                      "categorisation-"
                                                      (symbol-name new-cat-1) "-"
                                                      (symbol-name new-cat-2)
                                                      "-cxn")))
                               ((?categorisation-unit
                                 (args (?x))
                                 (unit-type categorisation)
                                 (subunits (?lex-1-unit ?lex-2-unit))
                                 (footprints (categorisation-cxn)))
                                (?lex-1-unit
                                 (footprints (categorisation-cxn)))
                                (?lex-2-unit
                                 (footprints (categorisation-cxn)))
                                <-
                                (?lex-1-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-1))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-1))
                                 (footprints (NOT categorisation-cxn)))
                                (?lex-2-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-2))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-2))
                                 (footprints (NOT categorisation-cxn)))
                                (?categorisation-unit
                                 --
                                 (HASH form ((meets ?lex-1-unit ?lex-2-unit)))))
                               :cxn-inventory ,cxn-set
                               :attributes (:categories ,new-cat-1 ,new-cat-2)))

           (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                      "categorisation-"
                                                      (symbol-name new-cat-1) "-"
                                                      (symbol-name new-cat-3)
                                                      "-cxn")))
                               ((?categorisation-unit
                                 (args (?x))
                                 (unit-type categorisation)
                                 (subunits (?lex-1-unit ?lex-2-unit))
                                 (footprints (categorisation-cxn)))
                                (?lex-1-unit
                                 (footprints (categorisation-cxn)))
                                (?lex-2-unit
                                 (footprints (categorisation-cxn)))
                                <-
                                (?lex-1-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-1))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-1))
                                 (footprints (NOT categorisation-cxn)))
                                (?lex-2-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-3))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-3))
                                 (footprints (NOT categorisation-cxn)))
                                (?categorisation-unit
                                 --
                                 (HASH form ((meets ?lex-1-unit ?lex-2-unit)))))
                               :cxn-inventory ,cxn-set
                               :attributes (:categories ,new-cat-1 ,new-cat-3)))

           (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                      "categorisation-"
                                                      (symbol-name new-cat-2) "-"
                                                      (symbol-name new-cat-3)
                                                      "-cxn")))
                               ((?categorisation-unit
                                 (args (?x))
                                 (unit-type categorisation)
                                 (subunits (?lex-1-unit ?lex-2-unit))
                                 (footprints (categorisation-cxn)))
                                (?lex-1-unit
                                 (footprints (categorisation-cxn)))
                                (?lex-2-unit
                                 (footprints (categorisation-cxn)))
                                <-
                                (?lex-1-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-2))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-2))
                                 (footprints (NOT categorisation-cxn)))
                                (?lex-2-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-3))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-3))
                                 (footprints (NOT categorisation-cxn)))
                                (?categorisation-unit
                                 --
                                 (HASH form ((meets ?lex-1-unit ?lex-2-unit)))))
                               :cxn-inventory ,cxn-set
                               :attributes (:categories ,new-cat-2 ,new-cat-3)))
           
           (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                      "categorisation-"
                                                      (symbol-name new-cat-1) "-"
                                                      (symbol-name new-cat-2) "-"
                                                      (symbol-name new-cat-3)
                                                      "-cxn")))
                               ((?categorisation-unit
                                 (args (?x))
                                 (unit-type categorisation)
                                 (subunits (?lex-1-unit ?lex-2-unit ?lex-3-unit))
                                 (footprints (NOT categorisation-cxn)))
                                (?lex-1-unit
                                 (footprints (categorisation-cxn)))
                                (?lex-2-unit
                                 (footprints (categorisation-cxn)))
                                (?lex-3-unit
                                 (footprints (categorisation-cxn)))
                                <-
                                (?lex-1-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-1))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-1))
                                 (footprints (NOT categorisation-cxn)))
                                (?lex-2-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-2))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-2))
                                 (footprints (NOT categorisation-cxn)))
                                (?lex-3-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-3))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-3))
                                 (footprints (NOT categorisation-cxn)))
                                (?categorisation-unit
                                 --
                                 (HASH form ((meets ?lex-1-unit ?lex-2-unit)
                                             (meets ?lex-2-unit ?lex-3-unit)))))
                               :cxn-inventory ,cxn-set
                               :attributes (:categories ,new-cat-1 ,new-cat-2 ,new-cat-3)))

           (add-categories (list new-cat-1 new-cat-2 new-cat-3) (get-type-hierarchy cxn-set))
           (add-categories  (list (intern (symbol-name (first cats)) :type-hierarchies)
                                  (intern (symbol-name (second cats)) :type-hierarchies))
                            (get-type-hierarchy cxn-set))
           (add-link  (intern (symbol-name (first cats)) :type-hierarchies) new-cat-1 (get-type-hierarchy cxn-set) :weight 0.5)
           (add-link  (intern (symbol-name (second cats)) :type-hierarchies) new-cat-2 (get-type-hierarchy cxn-set) :weight 0.5)
           (when (= (length cats) 3)
             (add-category  (intern (symbol-name (third cats)) :type-hierarchies) (get-type-hierarchy cxn-set))
             (add-link  (intern (symbol-name (third cats)) :type-hierarchies)  new-cat-3 (get-type-hierarchy cxn-set) :weight 0.5))
           ))




;;;;;;;;;;;;;;;;;;;;
;; Expansion mode ;;
;;;;;;;;;;;;;;;;;;;;

(in-package :fcg)

(defmethod expand-cip-node ((node cip-node) (mode (eql :expand-with-multiple-cxns)))
  "When next-cxn returns a list, then apply all these cxns to the cipn."
  (loop with nodes-to-queue = nil
        with failed-nodes = nil
        with cxn-inventory = (construction-inventory node)
        for cxns = (listify (next-cxn (cxn-supplier node) node))
        when cxns
        do (let ((succeeded-cars nil)
                 (failed-cars nil))
             (dolist (cxn cxns)
               (multiple-value-bind (these-succeeded-cars these-failed-cars)
                   (fcg-apply (safe-cxn cxn (applied-constructions node))
                              (car-resulting-cfs (cipn-car node))
                              (direction (cip node)) :notify nil
                              :configuration (configuration (construction-inventory node))
                              :cxn-inventory cxn-inventory)
                 (setf succeeded-cars (append succeeded-cars these-succeeded-cars)
                       failed-cars (append failed-cars these-failed-cars))))
             (loop for car in succeeded-cars
                   do (push (cip-add-child node car)
                            nodes-to-queue)
                   when (apply-sequentially? node (car-applied-cxn car))
                   do (setf (fully-expanded? node) t) (return))
             
             (loop for car in failed-cars
                   do (push (cip-add-child node car :cxn-applied nil)
                            failed-nodes)))
        when nodes-to-queue do (return nodes-to-queue)
        while cxns
        finally (setf (fully-expanded? node) t)))

(defun unify-atom (x y bindings &key cxn-inventory)
  "unify-atom function for use with type-hierarchies"
  (cond ((eq bindings +fail+) +fail+)
	;; handle strings and interned symbols
	((equal x y) bindings)
	;; unify uninterned symbols 
	((and (symbolp x) (symbolp y)
	      (equal (symbol-name x) (symbol-name y)) bindings))
        ;; unify symbols on type-hierarchy-basis
        ((and cxn-inventory
              (type-hierarchies::get-type-hierarchy cxn-inventory)
              (symbolp x) (symbolp y)
              (type-hierarchies::node-p (intern (symbol-name x) :type-hierarchies)
                                        (type-hierarchies:get-type-hierarchy cxn-inventory))
              (type-hierarchies::node-p (intern (symbol-name y) :type-hierarchies)
                                        (type-hierarchies:get-type-hierarchy cxn-inventory))
              (type-hierarchies::directed-path-p (intern (symbol-name y) :type-hierarchies)
                                                 (intern (symbol-name x) :type-hierarchies)
                                                 (type-hierarchies:get-type-hierarchy cxn-inventory))
              ;(< (type-hierarchies:link-weight (intern (symbol-name y) :type-hierarchies)
              ;                                 (intern (symbol-name x) :type-hierarchies)
              ;                                 (type-hierarchies:get-type-hierarchy cxn-inventory))
              ;   1.0)
              bindings))
	;; unify variables
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((unify-equal x y) bindings)
	(t (values +fail+ x y))))