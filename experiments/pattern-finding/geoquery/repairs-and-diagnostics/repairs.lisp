(in-package :pf-for-sql)


;;------------;;
;; HOLOPHRASE ;;
;;------------;;

(defclass add-holophrase (repair) 
  ((trigger :initform 'fcg::new-node)))

(defun make-cxn-name (form-string)
  (let* ((parts (uiop:split-string form-string :separator " "))
         (string-name (format nil "~a" (first parts))))
    (pop parts)
    (loop for part in parts
          do (setf string-name (format nil "~a-~a" string-name part)))
    (setf string-name (format nil "~a-cxn" string-name))
    string-name))

;(make-cxn-name "give me the cities in usa")

(defun find-equivalent-cxn (form-string meaning-predicates cxn-inventory)
  (loop for cxn in (constructions cxn-inventory)
        when (and (string= (attr-val cxn :sequence) form-string)
                  (equalp (attr-val cxn :meaning) meaning-predicates))
          return cxn))

(defun learn-holophrase (form-string meaning-predicates agent-grammar)
  "Learning a holophrastic cxn ; takes as argument a form (a question in natural language) and a meaning (a predicate network)."  
    (let* ((cxn-name (make-symbol (make-cxn-name form-string)))
           (cxn-cat (make-symbol (format nil "~a-cat" (make-cxn-name form-string))))
           (holistic-cxn (make-instance 'fcg-construction
                        :name cxn-name
                        :contributing-part (list (make-instance 'contributing-unit
                                                                :name '?holistic-unit
                                                                :unit-structure `((category ,cxn-cat)
                                                                                  (form-args (?left-1 ?right-1))
                                                                                  (meaning-args (irl:get-target-var meaning)))))
                        :conditional-part (list (make-instance 'conditional-unit
                                                               :name '?holistic-unit
                                                               :formulation-lock `((HASH meaning ,meaning-predicates))
                                                               :comprehension-lock `((HASH form ((sequence ,form-string ?left-1 ?right-1))))))
                        :attributes `((:cxn-cat . ,cxn-cat) (:sequence . ,form-string) (:meaning ,@meaning-predicates))
                        :description "A geo construction"
                        :cxn-inventory agent-grammar)))
      ;(unless (find-equivalent-cxn form-string meaning-predicates agent-grammar)
      (add-cxn holistic-cxn agent-grammar)
      (add-category cxn-cat agent-grammar)))

;(learn-holophrase "name all the lakes of us" '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND COLUMN ?COLUMN-2 LAKE_NAME) (BIND CONCEPT ?ALIAS-0 LAKEALIAS0) (BIND TABLE ?TABLE-0 LAKE)))

;(learn-holophrase "what is capital of iowa" '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-3) (DOT ?COLUMN-2 ?ALIAS-0 ?COLUMN-4) (EQUALS ?FILTER-0 ?COLUMN-2 ?COMPARATOR-0) (WHERE ?FILTER-1 ?FILTER-0) (AS ?FILTER-2 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-3 ?FILTER-2) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-3 ?FILTER-1) (BIND COLUMN ?COLUMN-4 STATE_NAME) (BIND COLUMN ?COLUMN-3 CAPITAL) (BIND CONCEPT ?COMPARATOR-0 iowa) (BIND CONCEPT ?ALIAS-0 STATEALIAS0) (BIND TABLE ?TABLE-0 STATE)))

;(comprehend "what is capital of iowa" :construction-inventory *fcg-constructions*)

;;------------;;
;; ITEM-BASED ;;
;;------------;;

;; Problem class: demo-unknown-words inherits from problem
(defclass demo-unknown-words (problem)
  ())

;; Diagnostic class: demo-diagnose-unknown-words inherits from diagnostic
(defclass demo-diagnose-unknown-words (diagnostic)
  ((trigger :initform 'new-node)))

;; Repair class: demo-add-lexical-cxn inherits from repair
(defclass demo-add-lexical-cxn (repair) 
  ((trigger :initform 'new-node)))

; Second repair : make an item based cxn. For the moment, they are of one slot only

(defun make-item-based-cxn (form-string meaning-predicates form-sequence-1 form-sequence-2)
  "based on meaning and form, makes an item-based cxn" ;; single-slot for now, to be continued
  (let* ((item-based-name (make-symbol (make-cxn-name form-string)))
        (cxn-cat (make-symbol (format nil "~a-cat" (make-cxn-name form-string))))
        (slot-cxn-cat (make-symbol (format nil "~a-slot-1-cat" (make-cxn-name form-string))))
        (item-based-cxn (make-instance 'fcg-construction
                                       :name item-based-name
                                       :contributing-part (list (make-instance 'contributing-unit
                                                                               :name '?item-based-unit
                                                                               :unit-structure `((category ,cxn-cat)
                                                                                                 (form-args (?left-1 ?right-2))
                                                                                                 (meaning-args ,(irl:get-target-var meaning-predicates))
                                                                                                 (subunits (?slot-1)))))
                                       :conditional-part (list (make-instance 'conditional-unit
                                                                              :name '?item-based-unit
                                                                              :formulation-lock `((HASH meaning ,meaning-predicates)) ;item-based-meaning
                                                                              :comprehension-lock `((HASH form ((sequence ,form-sequence-1 ?left-1 ?right-1)(sequence ,form-sequence-2 ?left-2 ?right-2)))))
                                                               (make-instance 'conditional-unit
                                                                              :name '?slot-1
                                                                              :formulation-lock `((meaning-args ,(irl:get-open-vars meaning-predicates)) (category ,slot-cxn-cat))
                                                                              :comprehension-lock `((form-args (?right-1 ?left-2)) (category ,slot-cxn-cat))))
                                       :attributes `((:cxn-cat . ,cxn-cat)
                                                     (:sequence . ,form-string)
                                                     (:meaning ,@meaning-predicates)
                                                     (:repair . holistic->item-based)
                                                     (:score . 0.5)
                                                     (:label . cxn))
                                       :description "A geo construction"
                                       :cxn-inventory *fcg-constructions*)))
    (add-cxn item-based-cxn *fcg-constructions*)
    (add-category cxn-cat *fcg-constructions*)
    (add-category slot-cxn-cat *fcg-constructions*)))

; (make-item-based-cxn "give-me-the-slot-1-in-usa-cxn-1" '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND CONCEPT ?ALIAS-0 CITYALIAS0) (BIND TABLE ?TABLE-0 CITY)) "give me the " " in usa")

; (clear *fcg-constructions*)

(defun tokenize-form-string (form-string)
  "A function to tokenize the form-string"
  (let ((form-tokens (split-string form-string " ")))
    form-tokens))

