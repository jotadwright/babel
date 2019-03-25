(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing the grouping strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form Competitors and Meaning Competitors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod competing-cxns ((cxn fcg-construction) (cxn-inventory fcg-construction-set) (mode (eql :grouping-strategy)))
  "Competing-cxns do not exist in the grouping strategy."
  nil)

(defmethod find-same-cxn ((cxn fcg-construction) (cxn-inventory fcg-construction-set) (mode (eql :grouping-strategy)))
  "Two cxns are the same if they have the same name for lexical-cxns and the same categories for group-cxns."
  (cond ((string= (attr-val cxn :cxn-type) 'grouping-cxn)
         (find-cxn  (attr-val cxn :categories) cxn-inventory
               :key #'(lambda (cxn)
                        (attr-val cxn :categories))
               :test #'permutation-of?))
        (t
         (find-cxn cxn cxn-inventory :key #'name :test #'string=))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting Diagnostics and Repairs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-diagnostics-and-repairs ((agent syntax-agent) (mode (eql :grouping-strategy)))
  "Sets the diagnositics and repairs for the grouping-stragegy"
  (setf (diagnostics agent) (list (make-instance 'diagnose-multiple-hypotheses-formulation)
                                  (make-instance 'diagnose-multiple-hypotheses-comprehension)))
  (setf (repairs agent) (list (make-instance 'add-grouping-cxn-formulation)
                              (make-instance 'add-grouping-cxn-comprehension))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Learning Operators Used to implement the grouping strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass add-grouping-cxn-formulation (repair)
  ((trigger :initform 'multiple-hypotheses-formulation)))

(defclass add-grouping-cxn-comprehension (repair)
  ((trigger :initform 'multiple-hypotheses-comprehension)))

(defun make-and-add-grouping-cxns (cats cxn-set)
  (cond ((= 2 (length cats))
         (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                    "grouping-"
                                                    (symbol-name (first cats)) "-"
                                                    (symbol-name (second cats))
                                                    "-cxn")))
                             ((?group-unit
                               (args (?x))
                               (unit-type group)
                               (subunits (?lex-1-unit ?lex-2-unit))
                               (footprints (grouping-cxn)))
                              (?lex-1-unit
                               (footprints (grouping-cxn)))
                              (?lex-2-unit
                               (footprints (grouping-cxn)))
                              <-
                              (?lex-1-unit
                               (args (?x))
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT grouping-cxn))
                               --
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT grouping-cxn)))
                              (?lex-2-unit
                               (args (?x))
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT grouping-cxn))
                               --
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT grouping-cxn)))
                              (?group-unit
                               --
                               (HASH form ((group ?lex-1-unit ?lex-2-unit)))))
                             :cxn-inventory ,cxn-set
                             :attributes (:categories ,(first cats) ,(second cats)
                                          :cxn-type grouping-cxn))))
        ((= 3 (length cats))
         (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                    "grouping-"
                                                    (symbol-name (first cats)) "-"
                                                    (symbol-name (second cats)) "-"
                                                    (symbol-name (third cats))
                                                    "-cxn")))
                             ((?group-unit
                               (args (?x))
                               (unit-type group)
                               (subunits (?lex-1-unit ?lex-2-unit ?lex-3-unit))
                               (footprints (NOT grouping-cxn)))
                              (?lex-1-unit
                               (footprints (grouping-cxn)))
                              (?lex-2-unit
                               (footprints (grouping-cxn)))
                              (?lex-3-unit
                               (footprints (grouping-cxn)))
                              <-
                              (?lex-1-unit
                               (args (?x))
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT grouping-cxn))
                               --
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT grouping-cxn)))
                              (?lex-2-unit
                               (args (?x))
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT grouping-cxn))
                               --
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT grouping-cxn)))
                              (?lex-3-unit
                               (args (?x))
                               (syn-cat (lex-class ,(third cats)))
                               (footprints (NOT grouping-cxn))
                               --
                               (syn-cat (lex-class ,(third cats)))
                               (footprints (NOT grouping-cxn)))
                              (?group-unit
                               --
                               (HASH form ((group ?lex-1-unit ?lex-2-unit ?lex-3-unit)))))
                             :cxn-inventory ,cxn-set
                             :attributes
                             (:categories ,(first cats) ,(second cats) ,(third cats)
                              :cxn-type grouping-cxn))))
        ((= 4 (length cats))
         (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                    "grouping-"
                                                    (symbol-name (first cats)) "-"
                                                    (symbol-name (second cats)) "-"
                                                    (symbol-name (third cats)) "-"
                                                    (symbol-name (fourth cats))
                                                    "-cxn")))
                             ((?group-unit
                               (args (?x))
                               (unit-type group)
                               (subunits (?lex-1-unit ?lex-2-unit ?lex-3-unit ?lex-4-unit))
                               (footprints (NOT grouping-cxn)))
                              (?lex-1-unit
                               (footprints (grouping-cxn)))
                              (?lex-2-unit
                               (footprints (grouping-cxn)))
                              (?lex-3-unit
                               (footprints (grouping-cxn)))
                              (?lex-4-unit
                               (footprints (grouping-cxn)))
                              <-
                              (?lex-1-unit
                               (args (?x))
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT grouping-cxn))
                               --
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT grouping-cxn)))
                              (?lex-2-unit
                               (args (?x))
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT grouping-cxn))
                               --
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT grouping-cxn)))
                              (?lex-3-unit
                               (args (?x))
                               (syn-cat (lex-class ,(third cats)))
                               (footprints (NOT grouping-cxn))
                               --
                               (syn-cat (lex-class ,(third cats)))
                               (footprints (NOT grouping-cxn)))
                              (?lex-4-unit
                               (args (?x))
                               (syn-cat (lex-class ,(fourth cats)))
                               (footprints (NOT grouping-cxn))
                               --
                               (syn-cat (lex-class ,(fourth cats)))
                               (footprints (NOT grouping-cxn)))
                              (?group-unit
                               --
                               (HASH form ((group ?lex-1-unit ?lex-2-unit ?lex-3-unit ?lex-4-unit)))))
                             :cxn-inventory ,cxn-set
                             :attributes
                             (:categories ,(first cats) ,(second cats) ,(third cats) ,(fourth cats)
                              :cxn-type grouping-cxn))))))

(defmethod repair ((repair add-grouping-cxn-formulation)
                   (problem multiple-hypotheses-formulation)
                   (agent syntax-agent)
                   &key &allow-other-keys)
  "Repair by making a new grouping-cxn."
  (let* ((problematic-cipn (get-data agent :problematic-cipn))
         (lexical-units (remove-non-word-units (left-pole-structure (car-resulting-cfs (cipn-car problematic-cipn)))))
         (categories (mapcar #'cdr (objects-categories lexical-units)))
         (temp-cxn-set (copy-object (grammar agent))))
    (set-data (blackboard temp-cxn-set) :scene (get-data (blackboard (grammar agent)) :scene))
    (loop for cats in categories
          do
          (unless (find-cxn cats temp-cxn-set
                            :key #'(lambda (cxn)
                                     (cdr (assoc :categories (attributes cxn))))
                            :test #'(lambda (cats cats-cxn)
                                      (permutation-of? cats cats-cxn)))
            (make-and-add-grouping-cxns (shuffle cats) temp-cxn-set)))
      (make-instance 'syntax-fix
                     :repair repair
                     :problem problem
                     :restart-data temp-cxn-set)))

(defmethod repair ((repair add-grouping-cxn-comprehension)
                   (problem multiple-hypotheses-comprehension)
                   (agent syntax-agent)
                   &key &allow-other-keys)
  "Repair by making a new grouping-cxn."
  (let* ((problematic-cipn (get-data agent :problematic-cipn))
         (topic (get-data agent :topic))
         (scene (get-data agent :scene))
         (utterance (get-data agent :utterance))
         (lexical-units (remove-non-word-units (left-pole-structure (car-resulting-cfs (cipn-car problematic-cipn)))))
         (string-cat-meaning-tuples (get-string-cat-meaning-tuples-for-utterance utterance lexical-units))
         (grouped-string-cat-meaning-tuples (discriminate-objects-using-words (objects topic) (objects scene) string-cat-meaning-tuples))
         (categories (mapcar #'(lambda (list)
                                        (loop for sublist in list
                                              collect (second sublist)))
                            grouped-string-cat-meaning-tuples))
         (temp-cxn-set (copy-object (grammar agent))))
    (set-data (blackboard temp-cxn-set) :scene (get-data (blackboard (grammar agent)) :scene))
    (loop for cats in categories
          do
          (unless (find-cxn (shuffle cats) temp-cxn-set
                            :key #'(lambda (cxn)
                                     (cdr (assoc :categories (attributes cxn))))
                            :test #'(lambda (cats cats-cxn)
                                      (equal cats cats-cxn)))
            (make-and-add-grouping-cxns cats temp-cxn-set)))
      (make-instance 'syntax-fix
                     :repair repair
                     :problem problem
                     :restart-data temp-cxn-set)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render and de-render ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-lexicon ((word-list list) (mode (eql :grouping-strategy)))
  "Creates and returns a construction inventory with lexical constructions for word-list."
  (let* ((grammar-name (make-const "SYNTAX-GRAMMAR"))
         (cxn-inventory
          (eval `(def-fcg-constructions ,grammar-name
                   :cxn-inventory ,grammar-name
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:production-goal-tests :no-meaning-in-root :single-interpretation-in-world-formulation)
                                        (:parse-goal-tests :no-strings-in-root :single-interpretation-in-world-comprehension)
                                        (:cxn-supplier-mode . :ordered-by-label-nr-of-categories-and-score)
                                        (:render-mode . :render-string-group)
                                        (:de-render-mode . :de-render-string-group)
                                        (:create-initial-structure-mode . :root-with-redundant-meaning)
                                        (:queue-mode . :backtrack-over-grammatical-cxns-only)
                                        )))))
    (add-words cxn-inventory word-list)))

(in-package :fcg) 

(defmethod de-render ((utterance list) (mode (eql :de-render-string-group)) &key &allow-other-keys)
  "De-renders a list of strings into string and groups."
  (let* ((string-constraints (loop for string in utterance
                       for constant = (make-const string nil)
                       collect `(string ,constant ,string)))
         (groups (remove-if-not #'(lambda (s) (or (= 2 (length s))
                                                  (= 3 (length s))
                                                  (= 4 (length s))))
                            (remove nil (origins-of-syntax::all-subsequences (mapcar #'second string-constraints)))))
         (group-constraints (loop for group in groups
                                  for permutated-groups = (permutations-of-length group (length group))
                                  append (loop for p-g in permutated-groups
                                               collect `(group ,@p-g)))))
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(append string-constraints group-constraints))
                                      (syn-cat ())))
		   :right-pole '((root)))))

;; (add-element (make-html (de-render '("the" "good" "soup") :de-render-string-group)))

(defmethod render ((cfs coupled-feature-structure) (mode (eql :render-string-group)) &key &allow-other-keys)
  "Only first order grouping: groups cannot be hierarchical or nested"
  (let* (;; Extract string and group constraints form form constraints of cfs
         (form-constraints (extract-forms (left-pole-structure cfs)))
         (string-constraints (find-all 'string form-constraints :test #'equalp :key #'first))
         (group-constraints (sort (find-all "GROUP" form-constraints :test #'string= :key #'first) #'length>))
         ;; Group de lexes in the group constraints into groups
         (ordered-group-constraints (loop with ordered-group-constraints = nil
                                          for g-p in group-constraints
                                          for lexes = (shuffle (cdr g-p))
                                          do (cond (;; All lexes already covered
                                                    (find lexes ordered-group-constraints
                                                          :test #'(lambda (x y)
                                                                    (or (permutation-of? x y :test #'string=)
                                                                        (is-subset x y :test #'string=)))))
                                                   (t
                                                    (push lexes ordered-group-constraints)))
                                          finally (return ordered-group-constraints))))
         ;; Add the strings that occured in no group constraints
         (loop with flat-groups = (flatten ordered-group-constraints)
                                                           for string-constraint in string-constraints
                                                           unless (find (second string-constraint) flat-groups :test #'equalp)
                                                           do (push (list (second string-constraint)) ordered-group-constraints))
         ;; Collect the strings from the lexes
         (loop for unit-name in (flatten (shuffle ordered-group-constraints))
               collect (unit-name-in-group-constraint->string unit-name string-constraints))))
    
(defun unit-name-in-group-constraint->string (unit-name string-constraints)
  "Given string constraints and a unit-name, returns the string."
  (loop for string-constraint in string-constraints
        when (string= unit-name (second string-constraint))
        return (third string-constraint)))

#|
(render (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ((string a "the") (string b "good") (string c "soup")
                                             (group a c b) (group b a)
                                             (string x "abc") (string z "xyz")
                                             (group z x) (group f d)
                                             (string d "a") (string e "nice") (string f "plate")
                                             (group e f d)
                                             (string i "i") (string j "j"))
                                      (syn-cat ()))))
		   :right-pole '((root)))
        :render-string-group)
|#