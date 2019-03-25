(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing the basic marking strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form Competitors and Meaning Competitors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod competing-cxns ((cxn fcg-construction) (cxn-inventory fcg-construction-set) (mode (eql :marker-strategy)))
  "Competing-cxns have the same category or marker as cxn."
  ;;If there are remaining redundant features in the CIPN, look up if
  ;;they have markers in the grammar that have not been used in this
  ;;game and if so, punish these >> currently only working for the SPEAKER

  (let* ((final-ts (left-pole-structure (car-resulting-cfs (cipn-car (get-data (blackboard cxn-inventory) :cipn)))))
         (referent-marked-by-cxn (loop for unit in final-ts
                                       for strings = (extract-string unit)
                                       when (find (attr-val cxn :marker) strings :key #'third :test #'string=)
                                       do (return (first (unit-feature-value unit 'args)))))
         (redundant-meanings (unit-feature-value
                              (get-root (left-pole-structure (car-resulting-cfs (cipn-car (get-data (blackboard cxn-inventory) :cipn)))))
                              'fcg::redundant-meaning))

         (unexpressed-redundant-features-with-same-ref
          (loop for meaning-pred in redundant-meanings
                when (eq (second meaning-pred) referent-marked-by-cxn)
                collect meaning-pred)))
    
    (when unexpressed-redundant-features-with-same-ref
      (let ((possible-competing-markers (loop for marker-cxn in (markers cxn-inventory)
                                              when (find (attr-val marker-cxn :category)
                                                         unexpressed-redundant-features-with-same-ref :key #'first)
                                              collect marker-cxn)))
        (loop for possible-competitor in possible-competing-markers
              unless (or (eq (attr-val possible-competitor :category)
                             (attr-val cxn :category)) ;;niet alleen van deze maar alle die toepasten (of filteren op obj-id)
                         (string= (attr-val possible-competitor :marker)
                                  (attr-val cxn :marker)))
              collect possible-competitor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Learning Operators Used to implement the default Marking Strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass add-marker-cxn-formulation (repair)
  ((trigger :initform 'multiple-hypotheses-formulation)))

(defclass adopt-marker-cxn-comprehension (repair)
  ((trigger :initform 'multiple-hypotheses-comprehension)))

(defun marker (cxn) (attr-val cxn :marker))
(defsetf marker (cxn) (marker) `(setf (attr-val ,cxn :marker) ,marker))

(defun invent-marker-cxn (redundant-meaning cxn-set)
  (let* ((meaning (first redundant-meaning))
         (marker (string-append "-" (loop for word in (lexicon cxn-set)
                                          when (eql (attr-val word :meaning) meaning)
                                          return (attr-val word :form))))
         (referent-var (make-var 'ref))
         (marker-unit-name (make-symbol (upcase (string-append "?" marker "-unit"))))
         
         (cxn-name (make-symbol (upcase (string-append marker  "-marker-cxn")))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((root
                          (footprints (,cxn-name)))
                         (,marker-unit-name
                          (args (,referent-var))
                          (unit-type marker))
                         (?lex-unit
                          (subunits (,marker-unit-name))
                          (footprints (marker-cxn)))
                         <-
                         (root
                          (footprints (not ,cxn-name))
                          --
                          (footprints (not ,cxn-name)))
                         (,marker-unit-name
                          (HASH fcg::redundant-meaning ,(list (list meaning
                                                               referent-var)))
                          --
                          (HASH form ((string ,marker-unit-name ,marker))))
                         (?lex-unit
                          (args (,referent-var))
                          (unit-type word)
                          (footprints (not marker-cxn))
                          --
                          (unit-type word)
                          (footprints (not marker-cxn))
                          (HASH form ((meets ?lex-unit ,marker-unit-name)))))
                        :cxn-inventory ,cxn-set
                        :disable-automatic-footprints t
                        :attributes (:category ,meaning
                                     :marker ,marker)))))

(defun adopt-marker-cxn (marker cxn-set)
  (let ((meaning (second (loop for word in (lexicon cxn-set)
                                when (string= (attr-val word :form) (subseq marker 1))
                                return (list (attr-val word :form)
                                             (attr-val word :meaning)))))
        (marker-unit-name (make-symbol (upcase (string-append "?" marker "-unit"))))
        (referent-var (make-var 'ref))
        (cxn-name (make-symbol (upcase (string-append marker  "-marker-cxn")))))
    (assert meaning)
    (eval `(def-fcg-cxn ,cxn-name
                        ((root
                          (footprints (,cxn-name)))
                         (,marker-unit-name
                          (args (,referent-var))
                          (unit-type marker))
                         (?lex-unit
                          (subunits (,marker-unit-name))
                          (footprints (marker-cxn)))
                         <-
                         (root
                          (footprints (not ,cxn-name))
                          --
                          (footprints (not ,cxn-name)))
                         (?lex-unit
                          (args (,referent-var))
                          (unit-type word)
                          (footprints (not marker-cxn))
                          --
                          (unit-type word)
                          (footprints (not marker-cxn))
                          (HASH form ((meets ?lex-unit ,marker-unit-name))))
                         (,marker-unit-name
                          (HASH fcg::redundant-meaning ,(list (list meaning
                                                               referent-var)))
                          --
                          (HASH form ((string ,marker-unit-name ,marker)))))
                        :cxn-inventory ,cxn-set
                        :disable-automatic-footprints t
                        :attributes (:category ,meaning
                                     :marker ,marker)))))
        
(defun add-recursive-marker-cxn (cxn-set)
  (eval `(def-fcg-cxn generic-marker-cxn
                      ((?marker-unit
                        (args (?x))
                        (unit-type marker)
                        (footprints (marker-cxn)))
                       (?unit-to-be-marked
                        (subunits (?marker-unit))
                        (footprints (recursive-marker-cxn marker-cxn)))
                       <-
                       (?already-marked-unit
                        (args (?x))
                        (unit-type word)
                        (footprints (marker-cxn))
                        (subunits (?existing-marker-unit))
                        --
                        (unit-type word)
                        (subunits (?existing-marker-unit))
                        (footprints (marker-cxn)))
                       (?existing-marker-unit
                        (unit-type marker)
                        --
                        (unit-type marker)
                        (form ((string ?existing-marker-unit ?marker-string))))
                       (?unit-to-be-marked
                        (args (?x))
                        (unit-type word)
                        (footprints (NOT recursive-marker-cxn))
                        --
                        (unit-type word)
                        (footprints (NOT recursive-marker-cxn)))
                       (?marker-unit
                        --
                        (HASH form ((string ?marker-unit ?marker-string)
                                    (meets ?unit-to-be-marked ?marker-unit)))))
                      :cxn-inventory ,cxn-set
                      :disable-automatic-footprints t
                      :attributes (:score 2.0)))) ;;always highest score!

(defmethod repair ((repair add-marker-cxn-formulation)
                   (problem multiple-hypotheses-formulation)
                   (agent syntax-agent)
                   &key &allow-other-keys)
  "Repair by making a new n-gram-cxn."
  (let* ((problematic-cipn (get-data agent :problematic-cipn))
         (redundant-meaning-predicates (unit-feature-value
                                        (get-root (left-pole-structure (car-resulting-cfs (cipn-car problematic-cipn))))
                                        'fcg::redundant-meaning))
         (temp-cxn-set (copy-object (grammar agent))))

    (assert redundant-meaning-predicates)
    
    (set-data (blackboard temp-cxn-set) :scene (get-data (blackboard (grammar agent)) :scene))
    
    (unless (find-cxn 'generic-marker-cxn temp-cxn-set)
      (add-recursive-marker-cxn temp-cxn-set))

    (loop with chosen-preds = nil
          for meaning-predicate in redundant-meaning-predicates ;;one per object
          unless (and (find (second meaning-predicate) chosen-preds :key #'second)
                      (find (first meaning-predicate) chosen-preds :key #'first))
          do (invent-marker-cxn meaning-predicate temp-cxn-set)
          (push meaning-predicate chosen-preds))
    
    (make-instance 'syntax-fix
                     :repair repair
                     :problem problem
                     :restart-data temp-cxn-set)))

(defmethod repair ((repair adopt-marker-cxn-comprehension)
                   (problem multiple-hypotheses-comprehension)
                   (agent syntax-agent)
                   &key &allow-other-keys)
  "Repair by making a new n-gram-cxn."
  (let* ((problematic-cipn (get-data agent :problematic-cipn))
         (unprocessed-markers (get-strings (get-root (left-pole-structure (car-resulting-cfs (cipn-car problematic-cipn))))))
         (temp-cxn-set (copy-object (grammar agent))))
    
    (set-data (blackboard temp-cxn-set) :scene (get-data (blackboard (grammar agent)) :scene))

    (unless (find-cxn 'generic-marker-cxn temp-cxn-set)
      (add-recursive-marker-cxn temp-cxn-set))
    
    (loop for marker in (remove-duplicates unprocessed-markers :test #'string=)
          do (adopt-marker-cxn marker temp-cxn-set))
   
    (make-instance 'syntax-fix
                   :repair repair
                   :problem problem
                   :restart-data temp-cxn-set)))

(defun objects-categories (lexical-units)
  "From a list of lexical units, returns ((obj1 cat1 cat2)(obj2 cat 3))"
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

(defmethod set-diagnostics-and-repairs ((agent syntax-agent) (mode (eql :marker-strategy)))
  "Sets the diagnositics and repairs for the n-gram-stragegy"
  (setf (diagnostics agent) (list (make-instance 'diagnose-multiple-hypotheses-formulation)
                                  (make-instance 'diagnose-multiple-hypotheses-comprehension)))
  (setf (repairs agent) (list (make-instance 'add-marker-cxn-formulation)
                              (make-instance 'adopt-marker-cxn-comprehension))))


;(export '(fcg::redundant-meaning))

(defmethod initialize-lexicon ((word-list list) (mode (eql :marker-strategy)))
  "Creates and returns a construction inventory with lexical constructions for word-list."
  (let* ((grammar-name (make-const "SYNTAX-GRAMMAR"))
         (cxn-inventory
          (eval `(def-fcg-constructions ,grammar-name
                   :cxn-inventory ,grammar-name
                   :feature-types ((args sequence)
                                   (form set-of-predicates)
                                   (meaning set-of-predicates)
                                   (fcg::redundant-meaning set-of-predicates)
                                   (subunits set)
                                   (footprints set))
                   :fcg-configurations ((:production-goal-tests :no-meaning-in-root :single-interpretation-in-world-formulation)
                                        (:parse-goal-tests  :single-interpretation-in-world-comprehension :no-strings-in-root)
                                        (:cxn-supplier-mode . :ordered-by-label-and-score)
                                        (:parse-order lex cxn)
                                        (:production-order lex cxn)
                                        (:queue-mode . :backtrack-over-grammatical-cxns-only)
                                        (:create-initial-structure-mode . :root-with-redundant-meaning))))))
    (add-words cxn-inventory word-list)))

(defmethod find-same-cxn ((cxn fcg-construction) (cxn-inventory fcg-construction-set) (mode (eql :marker-strategy)))
  "Two cxns are the same if they have the same name."
  (find-cxn cxn cxn-inventory :key #'name :test #'string=))
