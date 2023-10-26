(ql:quickload :pf-for-sql)
(in-package :pf-for-sql)

(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction))

(defparameter *corpus-sample*
  (list
   '((:form . "give me the cities in usa")
     (:meaning . ((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND COLUMN ?COLUMN-2 CITY_NAME) (BIND CONCEPT ?ALIAS-0 CITYALIAS0) (BIND TABLE ?TABLE-0 CITY) (?result-1))))
   '((:form . "give me all the states of usa")
     (:meaning . ((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND COLUMN ?COLUMN-2 STATE_NAME) (BIND CONCEPT ?ALIAS-0 STATEALIAS0) (BIND TABLE ?TABLE-0 STATE))))
   '((:form . "give me the states in usa")
     (:meaning . ((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND COLUMN ?COLUMN-2 STATE_NAME) (BIND CONCEPT ?ALIAS-0 STATEALIAS0) (BIND TABLE ?TABLE-0 STATE))))
   '((:form . "give me all the lakes in usa")
     (:meaning . ((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND COLUMN ?COLUMN-2 LAKE_NAME) (BIND CONCEPT ?ALIAS-0 LAKEALIAS0) (BIND TABLE ?TABLE-0 LAKE))))
   '((:form . "give me all the cities in usa")
     (:meaning . ((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND COLUMN ?COLUMN-2 CITY_NAME) (BIND CONCEPT ?ALIAS-0 CITYALIAS0) (BIND TABLE ?TABLE-0 CITY))))))

#|(loop for example in *corpus-sample*
      do (print (cdr (assoc ':form example))))|#

;; tests sur un extrait du fichier
(defparameter *test* "{\"utterance\":\"what is capital of iowa\", \"meaning\":\"((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-3) (DOT ?COLUMN-2 ?ALIAS-0 ?COLUMN-4) (EQUALS ?FILTER-0 ?COLUMN-2 ?COMPARATOR-0) (WHERE ?FILTER-1 ?FILTER-0) (AS ?FILTER-2 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-3 ?FILTER-2) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-3 ?FILTER-1) (BIND COLUMN ?COLUMN-4 STATE_NAME) (BIND COLUMN ?COLUMN-3 CAPITAL) (BIND CONCEPT ?COMPARATOR-0 iowa) (BIND CONCEPT ?ALIAS-0 STATEALIAS0) (BIND TABLE ?TABLE-0 STATE))\", \"len\": 12}" )

(defparameter *corpus* "/Users/ajouglar/babel/systems/postmodern-parser/data/geography-for-pf.jsonl")

(defun remove-last-character (str)
  (subseq str 0 (- (length str) 1)))

(with-open-file (stream *corpus* :direction :input :external-format :utf-8 :element-type :default)
  (loop for line = (read-line stream nil nil)
        while line
        do (let*
               ((processed-line (remove-last-character line))
                (data (com.inuoe.jzon:parse processed-line))
                (utterance (gethash "utterance" data)))
             (with-input-from-string (meaning (gethash "meaning" data))
               (learn-holophrase utterance (read meaning))))))

;(add-element (make-html (constructions-list *fcg-constructions*)))

(comprehend "give me the cities in usa")

(loop for example in *corpus-sample*
      do (learn-holophrase (cdr (assoc ':form example)) (cdr (assoc ':meaning example))))

(find-cxn "give me the cities in usa" *fcg-constructions*)
(learn-holophrase "give me the cities in usa" '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2) (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-1 ?FILTER-0) (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1) (BIND COLUMN ?COLUMN-2 CITY_NAME) (BIND CONCEPT ?ALIAS-0 CITYALIAS0) (BIND TABLE ?TABLE-0 CITY)))




;; ------------------------------------------------------------------------------------- ;;
;; ------------------------------------------------------------------------------------- ;;

(def-fcg-constructions fcg-constructions
  :feature-types ((form set-of-predicates :handle-regex-sequences)
                  (meaning set-of-predicates)
                  (form-args sequence)
                  (meaning-args sequence)
                  (subunits set)
                  (footprints set))
  :hashed t
  :fcg-configurations (;; to activate heuristic search
                       (:construction-inventory-processor-mode . :heuristic-search) ;; use dedicated cip
                       (:node-expansion-mode . :full-expansion) ;; always fully expands node immediately
                       (:cxn-supplier-mode . :hashed) ;; use hashing
                       ;; for using heuristics
                       (:search-algorithm . :best-first) ;; :depth-first, :breadth-first
                       (:heuristics :nr-of-applied-cxns :nr-of-units-matched) ;; list of heuristic functions (modes of #'apply-heuristic)
                       (:heuristic-value-mode . :sum-heuristics-and-parent) ;; how to use results of heuristic functions for scoring a node
                      ; (:hash-mode . :hash-string-meaning)
                       (:de-render-mode . :de-render-sequence)
                       (:render-mode . :render-sequences)
                       (:category-linking-mode . :neighbours)
                       (:parse-goal-tests :no-applicable-cxns :connected-semantic-network)))

(progn 
(def-fcg-cxn give-me-the-cities-in-usa-cxn-1
             ((?holistic-unit
               (category give-me-the-cities-in-usa-cxn-cat-1)
               (meaning-args (?RESULT-0))
               (form-args (?left-1 ?right-1)))
              <-
              (?holistic-unit
               (HASH meaning ((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2)
                              (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0)
                              (FROM ?FILTER-1 ?FILTER-0)
                              (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1)
                              (BIND COLUMN ?COLUMN-2 CITY_NAME)
                              (BIND CONCEPT ?ALIAS-0 CITYALIAS0)
                              (BIND TABLE ?TABLE-0 CITY)))
               --
               (HASH form ((sequence "give me the cities in usa" ?left-1 ?right-1))))))

(def-fcg-cxn give-me-the-slot-1-in-usa-cxn-1
             ((?item-based-unit
               (category give-me-the-slot-1-in-usa-cxn-cat-1)
               (meaning-args (?RESULT-0))
               (form-args (?left-1 ?right-02))
               (subunits (?slot-1)))
              <-
              (?item-based-unit
               (HASH meaning ((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-2)
                              (AS ?FILTER-0 ?TABLE-0 ?ALIAS-0)
                              (FROM ?FILTER-1 ?FILTER-0)
                              (SELECT ?RESULT-0 ?COLUMN-1 ?FILTER-1)
                              (BIND CONCEPT ?ALIAS-0 CITYALIAS0)
                              (BIND TABLE ?TABLE-0 CITY)))
               --
               (HASH form ((sequence "give me the " ?left-1 ?right-1)
                           (sequence " in usa" ?left-2 ?right-2))))
              (?slot-1
               (category give-me-the-slot-1-in-usa-slot-1-cat-1)
               (meaning-args (?TABLE-0))
               --
               (form-args (?right-1 ?left-2))
               (category give-me-the-slot-1-in-usa-slot-1-cat-1)
               )))

(def-fcg-cxn states-cxn-1
             ((?holistic-unit
               (category states-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?COLUMN-2)))
              <-
              (?holistic-unit
               (HASH meaning ((BIND TABLE ?TABLE-0 ?state)))
               --
               (HASH form ((sequence "states" ?left-1 ?right-1))))))

(def-fcg-cxn cities-cxn-1
             ((?holistic-unit
               (category cities-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?COLUMN-2)))
              <-
              (?holistic-unit
               (HASH meaning ((BIND TABLE ?TABLE-0 ?city)))
               --
               (HASH form ((sequence "cities" ?left-1 ?right-1))))))

(def-fcg-cxn lakes-cxn-1
             ((?holistic-unit
               (category lakes-cxn-cat-1)
               (form-args (?left-1 ?right-1))
               (meaning-args (?COLUMN-2)))
              <-
              (?holistic-unit
               (HASH meaning ((BIND TABLE ?TABLE-0 ?lake)))
               --
               (HASH form ((sequence "lakes" ?left-1 ?right-1))))))

(add-categories '(give-me-the-cities-in-usa-cxn-cat-1
                  give-me-the-slot-1-in-usa-cxn-cat-1
                  give-me-the-slot-1-in-usa-slot-1-cat-1
                  states-cxn-cat-1
                  cities-cxn-cat-1
                  lakes-cxn-cat-1)
                *fcg-constructions*)

(progn
  (add-link 'states-cxn-cat-1 'give-me-the-slot-1-in-usa-slot-1-cat-1 *fcg-constructions*)
  (add-link 'cities-cxn-cat-1 'give-me-the-slot-1-in-usa-slot-1-cat-1 *fcg-constructions*)
  (add-link 'lakes-cxn-cat-1 'give-me-the-slot-1-in-usa-slot-1-cat-1 *fcg-constructions*)))

;; (comprehend-all "give me the cities in usa")
;; (comprehend-all "give me the states in usa")



