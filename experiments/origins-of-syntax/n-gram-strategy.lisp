(in-package :origins-of-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementing the n-gram strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Form Competitors and Meaning Competitors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod competing-cxns ((cxn fcg-construction) (cxn-inventory fcg-construction-set) (mode (eql :n-gram-strategy)))
  "Competing-cxns have the same categories as cxn, but in a different order."
  (loop with cxn-categories = (attr-val cxn :categories)
        for test-cxn in (remove (name cxn) (constructions cxn-inventory) :key #'name :test #'equalp)
        for test-cxn-categories = (attr-val test-cxn :categories)
        when (permutation-of? test-cxn-categories cxn-categories :key #'symbol-name :test #'equalp)
        collect test-cxn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Learning Operators Used to implement the N-gram Strategy ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass add-ngram-cxn-formulation (repair)
  ((trigger :initform 'multiple-hypotheses-formulation)))

(defclass add-ngram-cxn-comprehension (repair)
  ((trigger :initform 'multiple-hypotheses-comprehension)))

(defun make-and-add-n-gram-cxns (cats cxn-set)
  (cond ((= 2 (length cats))
         (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                    "n-gram-"
                                                    (symbol-name (first cats)) "-"
                                                    (symbol-name (second cats))
                                                    "-cxn")))
                             ((?n-gram-unit
                               (args (?x))
                               (unit-type n-gram)
                               (subunits (?lex-1-unit ?lex-2-unit))
                               (footprints (n-gram-cxn)))
                              (?lex-1-unit
                               (footprints (n-gram-cxn)))
                              (?lex-2-unit
                               (footprints (n-gram-cxn)))
                              <-
                              (?lex-1-unit
                               (args (?x))
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT n-gram-cxn))
                               --
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT n-gram-cxn)))
                              (?lex-2-unit
                               (args (?x))
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT n-gram-cxn))
                               --
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT n-gram-cxn)))
                              (?n-gram-unit
                               --
                               (HASH form ((meets ?lex-1-unit ?lex-2-unit)))))
                             :cxn-inventory ,cxn-set
                             :attributes (:categories ,(first cats) ,(second cats)))))
        ((= 3 (length cats))
         (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                    "n-gram-"
                                                    (symbol-name (first cats)) "-"
                                                    (symbol-name (second cats)) "-"
                                                    (symbol-name (third cats))
                                                    "-cxn")))
                             ((?n-gram-unit
                               (args (?x))
                               (unit-type n-gram)
                               (subunits (?lex-1-unit ?lex-2-unit ?lex-3-unit))
                               (footprints (NOT n-gram-cxn)))
                              (?lex-1-unit
                               (footprints (n-gram-cxn)))
                              (?lex-2-unit
                               (footprints (n-gram-cxn)))
                              (?lex-3-unit
                               (footprints (n-gram-cxn)))
                              <-
                              (?lex-1-unit
                               (args (?x))
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT n-gram-cxn))
                               --
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT n-gram-cxn)))
                              (?lex-2-unit
                               (args (?x))
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT n-gram-cxn))
                               --
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT n-gram-cxn)))
                              (?lex-3-unit
                               (args (?x))
                               (syn-cat (lex-class ,(third cats)))
                               (footprints (NOT n-gram-cxn))
                               --
                               (syn-cat (lex-class ,(third cats)))
                               (footprints (NOT n-gram-cxn)))
                              (?n-gram-unit
                               --
                               (HASH form ((meets ?lex-1-unit ?lex-2-unit)
                                           (meets ?lex-2-unit ?lex-3-unit)))))
                             :cxn-inventory ,cxn-set
                             :attributes (:categories ,(first cats) ,(second cats) ,(third cats)))))
         ((= 4 (length cats))
         (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append
                                                    "n-gram-"
                                                    (symbol-name (first cats)) "-"
                                                    (symbol-name (second cats)) "-"
                                                    (symbol-name (third cats)) "-"
                                                    (symbol-name (fourth cats))
                                                    "-cxn")))
                             ((?n-gram-unit
                               (args (?x))
                               (unit-type n-gram)
                               (subunits (?lex-1-unit ?lex-2-unit ?lex-3-unit ?lex-4-unit))
                               (footprints (NOT n-gram-cxn)))
                              (?lex-1-unit
                               (footprints (n-gram-cxn)))
                              (?lex-2-unit
                               (footprints (n-gram-cxn)))
                              (?lex-3-unit
                               (footprints (n-gram-cxn)))
                              (?lex-4-unit
                               (footprints (n-gram-cxn)))
                              <-
                              (?lex-1-unit
                               (args (?x))
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT n-gram-cxn))
                               --
                               (syn-cat (lex-class ,(first cats)))
                               (footprints (NOT n-gram-cxn)))
                              (?lex-2-unit
                               (args (?x))
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT n-gram-cxn))
                               --
                               (syn-cat (lex-class ,(second cats)))
                               (footprints (NOT n-gram-cxn)))
                              (?lex-3-unit
                               (args (?x))
                               (syn-cat (lex-class ,(third cats)))
                               (footprints (NOT n-gram-cxn))
                               --
                               (syn-cat (lex-class ,(third cats)))
                               (footprints (NOT n-gram-cxn)))
                              (?lex-4-unit
                               (args (?x))
                               (syn-cat (lex-class ,(fourth cats)))
                               (footprints (NOT n-gram-cxn))
                               --
                               (syn-cat (lex-class ,(fourth cats)))
                               (footprints (NOT n-gram-cxn)))
                              (?n-gram-unit
                               --
                               (HASH form ((meets ?lex-1-unit ?lex-2-unit)
                                           (meets ?lex-2-unit ?lex-3-unit)
                                           (meets ?lex-3-unit ?lex-4-unit)))))
                             :cxn-inventory ,cxn-set
                             :attributes (:categories ,(first cats) ,(second cats) ,(third cats) ,(fourth cats)))))))

(defmethod repair ((repair add-ngram-cxn-formulation)
                   (problem multiple-hypotheses-formulation)
                   (agent syntax-agent)
                   &key &allow-other-keys)
  "Repair by making a new n-gram-cxn."
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
            (make-and-add-n-gram-cxns (shuffle cats) temp-cxn-set)))
      (make-instance 'syntax-fix
                     :repair repair
                     :problem problem
                     :restart-data temp-cxn-set)))

(defmethod repair ((repair add-ngram-cxn-comprehension)
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
    (loop for cats in categories
          do
          (unless (find-cxn cats temp-cxn-set
                            :key #'(lambda (cxn)
                                     (cdr (assoc :categories (attributes cxn))))
                            :test #'(lambda (cats cats-cxn)
                                      (equal cats cats-cxn)))
            (make-and-add-n-gram-cxns cats temp-cxn-set)))
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

(defmethod set-diagnostics-and-repairs ((agent syntax-agent) (mode (eql :n-gram-strategy)))
  "Sets the diagnositics and repairs for the n-gram-stragegy"
  (setf (diagnostics agent) (list (make-instance 'diagnose-multiple-hypotheses-formulation)
                                  (make-instance 'diagnose-multiple-hypotheses-comprehension)))
  (setf (repairs agent) (list (make-instance 'add-ngram-cxn-formulation)
                              (make-instance 'add-ngram-cxn-comprehension))))

(defmethod initialize-lexicon ((word-list list) (mode (eql :n-gram-strategy)))
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
                                        (:queue-mode . :backtrack-over-grammatical-cxns-only)
                                        (:create-initial-structure-mode . :root-with-redundant-meaning))))))
    (add-words cxn-inventory word-list)))

(defmethod find-same-cxn ((cxn fcg-construction) (cxn-inventory fcg-construction-set) (mode (eql :n-gram-strategy)))
  "Two cxns are the same if they have the same name."
  (find-cxn cxn cxn-inventory :key #'name :test #'string=))
