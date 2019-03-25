(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing the categorisation strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-lexicon ((word-list list) (mode (eql :categorisation-strategy)))
  "Creates and returns a construction inventory with lexical constructions for word-list."
  (let* ((grammar-name (make-const "SYNTAX-GRAMMAR"))
         (cxn-inventory
          (eval `(def-fcg-constructions-with-type-hierarchy ,grammar-name
                   :cxn-inventory ,grammar-name
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:production-goal-tests :no-meaning-in-root :single-interpretation-in-world-formulation :not-more-than-two-unit-structures)
                                        (:parse-goal-tests :no-strings-in-root :single-interpretation-in-world-comprehension :not-more-than-two-unit-structures)
                                        (:cxn-supplier-mode . :cxn-supplier-categorisation-type-hierarchy)
                                        (:queue-mode . :backtrack-over-grammatical-cxns-only)
                                        (:priority-mode . :depth-first-with-type-hierachy-weights)
                                        (:create-initial-structure-mode . :root-with-redundant-meaning)
                                        (:node-expansion-mode . :expand-with-multiple-cxns))
                   :visualization-configurations ((:with-search-debug-data . t))))))
    (add-words cxn-inventory word-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form Competitors and Meaning Competitors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Learning Operators  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-diagnostics-and-repairs ((agent syntax-agent) (mode (eql :categorisation-strategy)))
  "Sets the diagnositics and repairs for the categorisation-stragegy"
  (setf (diagnostics agent) (list (make-instance 'diagnose-multiple-hypotheses-formulation)
                                  (make-instance 'diagnose-multiple-hypotheses-comprehension)))
  (setf (repairs agent) (list (make-instance 'add-cxn-or-th-link-comprehension)
                              (make-instance 'add-cxn-or-th-link-formulation))))

(defclass add-cxn-or-th-link-formulation (repair)
  ((trigger :initform 'multiple-hypotheses-formulation)))

(defclass add-cxn-or-th-link-comprehension (repair)
  ((trigger :initform 'multiple-hypotheses-comprehension)))

;; First, we want to make a new cxn ---


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
                 (progn
                   (add-categories (list source pattern) (get-type-hierarchy cxn-inventory))
                   ;(if (and (node-p source (get-type-hierarchy cxn-inventory))
                   ;           (node-p pattern (get-type-hierarchy cxn-inventory))
                   ;           (directed-path-p source pattern (get-type-hierarchy cxn-inventory)))
                   ;  (progn
                   ;    (set-link-weight source pattern (get-type-hierarchy cxn-inventory) (- (link-weight source pattern (get-type-hierarchy cxn-inventory)) 0.1))
                   ;    )
                     (add-link source pattern (get-type-hierarchy cxn-inventory)))
                 finally (return t))))))

(defun grammatical-cxns-with-n-units (cxn-inventory n)
  "Returns the grammatical constructions with n units on the conditional part. "
    (loop for cxn in (remove-lexical-constructions (constructions cxn-inventory))
          when (= n (length (conditional-part cxn)))
          collect cxn))

(defmethod repair ((repair add-cxn-or-th-link-formulation)
                   (problem multiple-hypotheses-formulation)
                   (agent syntax-agent)
                   &key &allow-other-keys)
  "Repair by making a new categorisation-cxn."
  (let* ((problematic-cipn (get-data agent :problematic-cipn))
         (lexical-units (remove-non-word-units (left-pole-structure (car-resulting-cfs (cipn-car problematic-cipn)))))
         (objects-categories (objects-categories lexical-units))
         (temp-cxn-set (copy-object (grammar agent))))
    (set-data (blackboard temp-cxn-set) :scene (get-data (blackboard (grammar agent)) :scene))
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
    (make-instance 'syntax-fix
                   :repair repair
                   :problem problem
                   :restart-data temp-cxn-set)))

(defun transient-structure-lexical-cxns-only (cipn)
  (labels ((only-lexical-cxns (cipn)
             (apply #'always (mapcar #'lexical-cxn-p (applied-constructions cipn)))))
    (if (only-lexical-cxns cipn)
      (car-resulting-cfs (cipn-car cipn))
      (loop for parent in (all-parents cipn)
            when (only-lexical-cxns parent)
            return (car-resulting-cfs (cipn-car parent))))))

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

(defun unit-meets-unit (left-unit right-unit meets-constraints)
  "returns true if left-unit meets right-unit according to the meets constraints."
  (loop with left-unit-name = (unit-name left-unit)
        with right-unit-name = (unit-name right-unit)
        for mc in meets-constraints
        when (and (equalp left-unit-name (second mc))
                  (equalp right-unit-name (third mc)))
        return t))

(defmethod repair ((repair add-cxn-or-th-link-comprehension)
                   (problem multiple-hypotheses-comprehension)
                   (agent syntax-agent)
                   &key &allow-other-keys)
  "Repair by making a new n-gram-cxn."
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
                   :restart-data temp-cxn-set)))

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
                           ;(< (link-weight (intern (symbol-name cat) :type-hierarchies)
                           ;             (intern (symbol-name cat-cxn) :type-hierarchies)
                           ;             type-hierarchy) 1.0)
                           ))
               
         ;; Categories are connected through th in any order
         (loop for cat-permuation in (permutations-of-length categories (length categories))
               when (categories-connected-through-type-hierarchy cat-permuation categories-cxn type-hierarchy :ordered t)
               do (return t)))))

#|
(defun make-and-add-categorisation-cxns (cats cxn-set)
  (cond ((= 2 (length cats))
         (let ((new-cat-1 (intern (symbol-name (make-const "SLOT-1of2")) :type-hierarchies))
               (new-cat-2 (intern (symbol-name (make-const "SLOT-2of2")) :type-hierarchies)))
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
           ;; Add categories and links in the type-hierarchy
           (add-categories (list (intern (symbol-name (first cats)) :type-hierarchies)
                                 (intern (symbol-name (second cats)) :type-hierarchies)
                                 new-cat-1 new-cat-2) (get-type-hierarchy cxn-set))
           (add-link  (intern (symbol-name (first cats)) :type-hierarchies) new-cat-1 (get-type-hierarchy cxn-set) :weight 0.5)
           (add-link  (intern (symbol-name (second cats)) :type-hierarchies) new-cat-2 (get-type-hierarchy cxn-set) :weight 0.5)))       
        ((= 3 (length cats))
         (let ((new-cat-1 (intern (symbol-name (make-const "SLOT-1of3")) :type-hierarchies))
               (new-cat-2 (intern (symbol-name (make-const "SLOT-2of3")) :type-hierarchies))
               (new-cat-3 (intern (symbol-name (make-const "SLOT-3of3")) :type-hierarchies)))
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
           ;; Add categories and links in the type-hierarchy
           (add-categories (list (intern (symbol-name (first cats)) :type-hierarchies)
                                 (intern (symbol-name (second cats)) :type-hierarchies)
                                 (intern (symbol-name (third cats)) :type-hierarchies)
                             new-cat-1 new-cat-2 new-cat-3)
                           (get-type-hierarchy cxn-set))
           (add-link  (intern (symbol-name (first cats)) :type-hierarchies) new-cat-1  (get-type-hierarchy cxn-set) :weight 0.5)
           (add-link  (intern (symbol-name (second cats)) :type-hierarchies) new-cat-2 (get-type-hierarchy cxn-set) :weight 0.5)
           (add-link  (intern (symbol-name (third cats)) :type-hierarchies) new-cat-3 (get-type-hierarchy cxn-set) :weight 0.5))) 
        
        ((= 4 (length cats))
         (let ((new-cat-1 (intern (symbol-name (make-const "SLOT-1of4")) :type-hierarchies))
               (new-cat-2 (intern (symbol-name (make-const "SLOT-2of4")) :type-hierarchies))
               (new-cat-3 (intern (symbol-name (make-const "SLOT-3of4")) :type-hierarchies))
               (new-cat-4 (intern (symbol-name (make-const "SLOT-4of4")) :type-hierarchies)))
           (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                      "categorisation-"
                                                      (symbol-name new-cat-1) "-"
                                                      (symbol-name new-cat-2) "-"
                                                      (symbol-name new-cat-3) "-"
                                                      (symbol-name new-cat-4)
                                                      "-cxn")))
                               ((?categorisation-unit
                                 (args (?x))
                                 (unit-type categorisation)
                                 (subunits (?lex-1-unit ?lex-2-unit ?lex-3-unit ?lex-4-unit))
                                 (footprints (NOT categorisation-cxn)))
                                (?lex-1-unit
                                 (footprints (categorisation-cxn)))
                                (?lex-2-unit
                                 (footprints (categorisation-cxn)))
                                (?lex-3-unit
                                 (footprints (categorisation-cxn)))
                                (?lex-4-unit
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
                                (?lex-4-unit
                                 (args (?x))
                                 (syn-cat (lex-class ,new-cat-4))
                                 (footprints (NOT categorisation-cxn))
                                 --
                                 (syn-cat (lex-class ,new-cat-4))
                                 (footprints (NOT categorisation-cxn)))
                                (?categorisation-unit
                                 --
                                 (HASH form ((meets ?lex-1-unit ?lex-2-unit)
                                             (meets ?lex-2-unit ?lex-3-unit)
                                             (meets ?lex-3-unit ?lex-4-unit)))))
                               :cxn-inventory ,cxn-set
                               :attributes (:categories ,new-cat-1 ,new-cat-2 ,new-cat-3 ,new-cat-4)))
                 ;; Add categories and links in the type-hierarchy
                 (add-categories (list (intern (symbol-name (first cats)) :type-hierarchies)
                                       (intern (symbol-name (second cats)) :type-hierarchies)
                                       (intern (symbol-name (third cats)) :type-hierarchies)
                                       (intern (symbol-name (fourth cats)) :type-hierarchies)
                             new-cat-1 new-cat-2 new-cat-3 new-cat-4)
                           (get-type-hierarchy cxn-set))
           (add-link  (intern (symbol-name (first cats)) :type-hierarchies) new-cat-1 (get-type-hierarchy cxn-set) :weight 0.5)
           (add-link  (intern (symbol-name (second cats)) :type-hierarchies) new-cat-2 (get-type-hierarchy cxn-set) :weight 0.5)
           (add-link  (intern (symbol-name (third cats)) :type-hierarchies)  new-cat-3 (get-type-hierarchy cxn-set) :weight 0.5)
           (add-link  (intern (symbol-name (fourth cats)) :type-hierarchies) new-cat-4 (get-type-hierarchy cxn-set) :weight 0.5)))))

|#

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

(defmethod find-same-cxn ((cxn fcg-construction) (cxn-inventory fcg-construction-set) (mode (eql :categorisation-strategy)))
  "Two cxns are the same if they have the same name."
  (find-cxn cxn cxn-inventory :key #'name :test #'string=))

(defun used-th-links (solution-cipn)
  (let* (;; All cip nodes that were created by the application of a non-lexical cxn
         (cip-nodes-through-cat-cxn (cons solution-cipn
                                          (remove-if #'(lambda (cipn)
                                                   (or (not (applied-constructions cipn))
                                                       (lexical-cxn-p (first (applied-constructions cipn)))))
                                                   (all-parents solution-cipn))))
         (cipn-cars (mapcar #'cipn-car cip-nodes-through-cat-cxn)))
    (mappend #'used-th-links-cipn-car cipn-cars)))

(defun used-th-links-cipn-car (cipn-car)
  (loop with applied-cxn = (left-pole-structure (car-applied-cxn cipn-car))
        with resulting-ts = (left-pole-structure (car-resulting-cfs cipn-car))
        for binding in (car-match-bindings cipn-car)
        for cxn-unit = (find (car binding) applied-cxn :key #'unit-name :test #'equalp)
        for ts-unit = (find (cdr binding) resulting-ts :key #'unit-name :test #'equalp)
        when (and cxn-unit ts-unit)
        collect (cons (lexical-unit->category ts-unit) (cxn-unit->category cxn-unit))))

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

(defmethod cip-priority ((node cip-node) (mode (eql :depth-first-with-type-hierachy-weights)))
  (if (all-parents node)
    (let* ((base-priority (+ 1.0  (priority (first (all-parents node)))))
           (cost-of-links (mapcar #'(lambda (link)
                                      (link-weight (car link)
                                                   (cdr link)
                                                   (get-type-hierarchy (construction-inventory node))))
                                  (used-th-links-cipn-car (cipn-car node)))))
      (if cost-of-links
        (+ base-priority (/ (sum (mapcar #'(lambda (cost) (- 1.0 cost)) cost-of-links))
                            (length (used-th-links-cipn-car (cipn-car node)))))
        base-priority))
    0.0))











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
         'cxn-supplier-categorisation-type-hierarchy
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
    (if (and (equalp (symbol-name label) "CXN")
             (equal (direction (cip node)) '->)) 
      (sort cxns-of-label (lambda (cxn1 cxn2)
                            (let ((nr-of-cats-cxn1 (length (attr-val cxn1 :categories)))
                                  (nr-of-cats-cxn2 (length (attr-val cxn2 :categories)))
                                  (score-cxn1 (attr-val cxn1 :score))
                                  (score-cxn2 (attr-val cxn2 :score)))
                              (cond ((> nr-of-cats-cxn1 nr-of-cats-cxn2) t)
                                    ((and (= nr-of-cats-cxn1 nr-of-cats-cxn2) (> score-cxn1 score-cxn2)) t)
                                    (t nil)))))
      cxns-of-label)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-categorisation-type-hierarchy) (node cip-node))
  (cond ;; For lexical cxns: just pop
        ((and (remaining-constructions cxn-supplier)
              (equalp (current-label cxn-supplier) 'lex))
         (pop (remaining-constructions cxn-supplier)))

        ;; For speaker cxns:  all of that length
        ((and (remaining-constructions cxn-supplier) (equal (direction (cip node)) '->))
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
        
        ;; For the hearer: no help
        ((and (remaining-constructions cxn-supplier) (equal (direction (cip node)) '<-))
         (let* ((next-constructions (remaining-constructions cxn-supplier)))
           ;;now we need to remove the next-constructions from the list of remaining constructions
           (setf (remaining-constructions cxn-supplier) nil)
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
               (all-constructions-of-label-categorisation-type-hierarchy node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))


















 
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