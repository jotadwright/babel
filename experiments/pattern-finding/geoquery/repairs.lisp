(in-package :pf-for-sql)

#|(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction))|#

#|(def-fcg-constructions experiment-parameters
  :feature-types ((form sequence)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set))
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :all-cxns) 
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                     ;  (:hash-mode . :hash-sequence-meaning)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network)))|#


;; First repair : learning a holophrastic contruction ;;

(defun make-cxn-name (form-string)
  (let* ((parts (uiop:split-string form-string :separator " "))
         (string-name (format nil "~a" (first parts))))
    (pop parts)
    (loop for part in parts
          do (setf string-name (format nil "~a-~a" string-name part)))
    (setf string-name (format nil "~a-cxn" string-name))
    string-name))

;(make-cxn-name "give me the cities in usa")

(defun learn-holophrase (form-string meaning-predicates)
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
                        :cxn-inventory *fcg-constructions*)))
      (add-cxn holistic-cxn *fcg-constructions*)
      (add-category cxn-cat *fcg-constructions*)))

;(learn-holophrase "what is capital of iowa" '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-3) (DOT ?COLUMN-2 ?ALIAS-0 ?COLUMN-4) (EQUALS ?FILTER-0 ?COLUMN-2 ?COMPARATOR-0) (WHERE ?FILTER-1 ?FILTER-0) (AS ?FILTER-2 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-3 ?FILTER-2) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-3 ?FILTER-1) (BIND COLUMN ?COLUMN-4 STATE_NAME) (BIND COLUMN ?COLUMN-3 CAPITAL) (BIND CONCEPT ?COMPARATOR-0 iowa) (BIND CONCEPT ?ALIAS-0 STATEALIAS0) (BIND TABLE ?TABLE-0 STATE)))

;(comprehend "what is capital of iowa" :construction-inventory *fcg-constructions*)

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

