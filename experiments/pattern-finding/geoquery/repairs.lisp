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
    (let* ((cxn-name (make-cxn-name form-string))
           (cxn-cat (format nil "~a-cat" cxn-name))
           (holistic-cxn (def-fcg-cxn cxn-name
                                      ((?holistic-unit
                                        (category cxn-cat)
                                        (meaning-args (irl:get-target-var meaning))
                                        (form-args (?left-1 ?right-1)))
                                       <--
                                       (?holistic-unit
                                        (HASH meaning (meaning-predicates))
                                        --
                                        (HASH form ((sequence form-string ?left-1 ?right-1)))))))
                                       
      holistic-cxn)))

;(learn-holophrase "what is capital of iowa" '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-3) (DOT ?COLUMN-2 ?ALIAS-0 ?COLUMN-4) (EQUALS ?FILTER-0 ?COLUMN-2 ?COMPARATOR-0) (WHERE ?FILTER-1 ?FILTER-0) (AS ?FILTER-2 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-3 ?FILTER-2) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-3 ?FILTER-1) (BIND COLUMN ?COLUMN-4 STATE_NAME) (BIND COLUMN ?COLUMN-3 CAPITAL) (BIND CONCEPT ?COMPARATOR-0 iowa) (BIND CONCEPT ?ALIAS-0 STATEALIAS0) (BIND TABLE ?TABLE-0 STATE)))

;(comprehend "what is capital of iowa" :construction-inventory *fcg-constructions*)

(defun learn-item-based-cxn (form-string meaning-predicates))

(defun tokenize-form-string (form-string)
  "A function to tokenize the form-string"
  (let ((form-tokens (split-string form-string " ")))
    form-tokens))

