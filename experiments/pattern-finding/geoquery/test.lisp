(ql:quickload :fcg)
(in-package :fcg)

(ql:quickload :pattern-finding)
(in-package :pf)

;; activate monitors
(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))

(defun remove-cxns-learned-at (experiment at)
  (let ((learned-at-cxns
         (find-all-if #'(lambda (cxn)
                          (string= (format nil "@~a" at)
                                   (attr-val cxn :learned-at)))
                      (constructions (grammar (learner experiment))))))
    (loop with grammar = (grammar (learner experiment))
          for cxn in learned-at-cxns
          for alter-ego-cxn = (alter-ego-cxn cxn grammar)
          do (delete-cxn (name cxn) grammar :key #'name)
             (delete-cxn (name alter-ego-cxn) grammar :key #'name))))

(defun setup-test-case ()
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (let* ((*experiment*
          (make-instance 'pattern-finding-experiment
                         :entries '((:mode . :testing))))
         (*cxn-inventory* (grammar (first (agents *experiment*)))))
    (values *experiment* *cxn-inventory*)))

;; for old fcg
(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:number-of-epochs . 5)
                              (:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:corpus-directory . ,(babel-pathname :directory '("systems" "postmodern-parser" "data")))
                              (:corpus-file . ,(make-pathname :name "geography-for-pf" :type "jsonl"))))))

;; for new fcg
(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:number-of-epochs . 5)
                              (:anti-unification-mode . :heuristic)
                              (:partial-analysis-mode . :heuristic)
                              (:allow-cxns-with-no-strings . nil)
                              (:repair-recursively . nil)
                              (:max-nr-of-nodes . 2000)
                              (:corpus-directory . ,(babel-pathname :directory '("systems" "postmodern-parser" "data")))
                              (:corpus-file . ,(make-pathname :name "geography-for-pf" :type "jsonl"))))))
(run-interaction *experiment*)

(run-series *experiment* 100)

(run-series *experiment* (length (corpus *experiment*)))

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
(add-element (make-html *cxn-inventory* :sort-by-type-and-score t))
(add-element (make-html (categorial-network *cxn-inventory*)))

;tests 
(defparameter *sorted-reorganized-data* (reorganize-by-id *reorganized-data*))
(fcg::print-anti-unification-results
 (anti-unify-predicate-network
  ;; what is the largest state bordering texas
  (fresh-variables '((DOT ?COLUMN-1 ?ALIAS-1 ?COLUMN-9)
    (DOT ?COLUMN-2 ?ALIAS-2 ?COLUMN-9)
    (DOT ?COLUMN-3 ?ALIAS-0 ?COLUMN-11)
    (DOT ?COLUMN-4 ?ALIAS-3 ?COLUMN-11)
    (DOT ?COLUMN-5 ?ALIAS-3 ?COLUMN-10)
    (DOT ?COLUMN-6 ?ALIAS-1 ?COLUMN-11)
    (DOT ?COLUMN-7 ?ALIAS-0 ?COLUMN-10)
    (DOT ?COLUMN-8 ?ALIAS-2 ?COLUMN-11)
    (MAX ?AGGREGATOR-0 ?COLUMN-5)
    (AS ?FILTER-0 ?TABLE-1 ?ALIAS-3)
    (AS ?FILTER-1 ?TABLE-0 ?ALIAS-2)
    (EQUALS ?FILTER-2 ?COLUMN-8 ?COMPARATOR-0)
    (WHERE ?FILTER-3 ?FILTER-2)
    (FROM ?FILTER-4 ?FILTER-1)
    (IN ?FILTER-5 ?COLUMN-4 ?RESULT-3)
    (WHERE ?FILTER-6 ?FILTER-5)
    (FROM ?FILTER-7 ?FILTER-0)
    (EQUALS ?FILTER-8 ?COLUMN-7 ?RESULT-2)
    (AS ?FILTER-9 ?TABLE-0 ?ALIAS-1)
    (EQUALS ?FILTER-10 ?COLUMN-6 ?COMPARATOR-0)
    (WHERE ?FILTER-11 ?FILTER-10)
    (FROM ?FILTER-12 ?FILTER-9)
    (IN ?FILTER-13 ?COLUMN-3 ?RESULT-1)
    (AND ?FILTER-14 ?FILTER-8 ?FILTER-13)
    (WHERE ?FILTER-15 ?FILTER-14)
    (AS ?FILTER-16 ?TABLE-1 ?ALIAS-0)
    (FROM ?FILTER-17 ?FILTER-16)
    (SELECT ?RESULT-0 ?COLUMN-3 ?FILTER-17 ?FILTER-15)
    (SELECT ?RESULT-1 ?COLUMN-1 ?FILTER-12 ?FILTER-11)
    (SELECT ?RESULT-2 ?AGGREGATOR-0 ?FILTER-7 ?FILTER-6)
    (SELECT ?RESULT-3 ?COLUMN-2 ?FILTER-4 ?FILTER-3)
    (BIND COLUMN ?COLUMN-11 STATE_NAME)
    (BIND COLUMN ?COLUMN-10 AREA)
    (BIND COLUMN ?COLUMN-9 BORDER)
    (BIND CONCEPT ?COMPARATOR-0 texas)
    (BIND CONCEPT ?ALIAS-0 STATEALIAS0)
    (BIND CONCEPT ?ALIAS-1 BORDER_INFOALIAS1)
    (BIND CONCEPT ?ALIAS-2 BORDER_INFOALIAS0)
    (BIND CONCEPT ?ALIAS-3 STATEALIAS1)
    (BIND TABLE ?TABLE-0 BORDER_INFO)
    (BIND TABLE ?TABLE-1 STATE)))
  ;; what is the largest state bordering arkansas
  (fresh-variables '((DOT ?COLUMN-1 ?ALIAS-1 ?COLUMN-9)
    (DOT ?COLUMN-2 ?ALIAS-2 ?COLUMN-9)
    (DOT ?COLUMN-3 ?ALIAS-0 ?COLUMN-11)
    (DOT ?COLUMN-4 ?ALIAS-3 ?COLUMN-11)
    (DOT ?COLUMN-5 ?ALIAS-3 ?COLUMN-10)
    (DOT ?COLUMN-6 ?ALIAS-1 ?COLUMN-11)
    (DOT ?COLUMN-7 ?ALIAS-0 ?COLUMN-10)
    (DOT ?COLUMN-8 ?ALIAS-2 ?COLUMN-11)
    (MAX ?AGGREGATOR-0 ?COLUMN-5)
    (AS ?FILTER-0 ?TABLE-1 ?ALIAS-3)
    (AS ?FILTER-1 ?TABLE-0 ?ALIAS-2)
    (EQUALS ?FILTER-2 ?COLUMN-8 ?COMPARATOR-0)
    (WHERE ?FILTER-3 ?FILTER-2)
    (FROM ?FILTER-4 ?FILTER-1)
    (IN ?FILTER-5 ?COLUMN-4 ?RESULT-3)
    (WHERE ?FILTER-6 ?FILTER-5)
    (FROM ?FILTER-7 ?FILTER-0)
    (EQUALS ?FILTER-8 ?COLUMN-7 ?RESULT-2)
    (AS ?FILTER-9 ?TABLE-0 ?ALIAS-1)
    (EQUALS ?FILTER-10 ?COLUMN-6 ?COMPARATOR-0)
    (WHERE ?FILTER-11 ?FILTER-10)
    (FROM ?FILTER-12 ?FILTER-9)
    (IN ?FILTER-13 ?COLUMN-3 ?RESULT-1)
    (AND ?FILTER-14 ?FILTER-8 ?FILTER-13)
    (WHERE ?FILTER-15 ?FILTER-14)
    (AS ?FILTER-16 ?TABLE-1 ?ALIAS-0)
    (FROM ?FILTER-17 ?FILTER-16)
    (SELECT ?RESULT-0 ?COLUMN-3 ?FILTER-17 ?FILTER-15)
    (SELECT ?RESULT-1 ?COLUMN-1 ?FILTER-12 ?FILTER-11)
    (SELECT ?RESULT-2 ?AGGREGATOR-0 ?FILTER-7 ?FILTER-6)
    (SELECT ?RESULT-3 ?COLUMN-2 ?FILTER-4 ?FILTER-3)
    (BIND COLUMN ?COLUMN-11 STATE_NAME)
    (BIND COLUMN ?COLUMN-10 AREA)
    (BIND COLUMN ?COLUMN-9 BORDER)
    (BIND CONCEPT ?COMPARATOR-0 arkansas)
    (BIND CONCEPT ?ALIAS-0 STATEALIAS0)
    (BIND CONCEPT ?ALIAS-1 BORDER_INFOALIAS1)
    (BIND CONCEPT ?ALIAS-2 BORDER_INFOALIAS0)
    (BIND CONCEPT ?ALIAS-3 STATEALIAS1)
    (BIND TABLE ?TABLE-0 BORDER_INFO)
    (BIND TABLE ?TABLE-1 STATE)))))


(fcg::print-anti-unification-results
 (fcg::anti-unify-predicate-network
  ;; how long is the colorado river
  (fresh-variables '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-3) (DOT ?COLUMN-2 ?ALIAS-0 ?COLUMN-4) (EQUALS ?FILTER-0 ?COLUMN-2 ?COMPARATOR-0) (WHERE ?FILTER-1 ?FILTER-0) (AS ?FILTER-2 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-3 ?FILTER-2) (DISTINCT ?AGGREGATOR-0 ?COLUMN-1) (SELECT ?RESULT-0 ?AGGREGATOR-0 ?FILTER-3 ?FILTER-1) (BIND COLUMN ?COLUMN-4 RIVER-NAME) (BIND COLUMN ?COLUMN-3 LENGTH) (BIND CONCEPT ?COMPARATOR-0 colorado) (BIND CONCEPT ?ALIAS-0 RIVERALIAS0) (BIND TABLE ?TABLE-0 RIVER)))
  ;; how long is the delaware river
  (fresh-variables '((DOT ?COLUMN-1 ?ALIAS-0 ?COLUMN-3) (DOT ?COLUMN-2 ?ALIAS-0 ?COLUMN-4) (EQUALS ?FILTER-0 ?COLUMN-2 ?COMPARATOR-0) (WHERE ?FILTER-1 ?FILTER-0) (AS ?FILTER-2 ?TABLE-0 ?ALIAS-0) (FROM ?FILTER-3 ?FILTER-2) (DISTINCT ?AGGREGATOR-0 ?COLUMN-1) (SELECT ?RESULT-0 ?AGGREGATOR-0 ?FILTER-3 ?FILTER-1) (BIND COLUMN ?COLUMN-4 RIVER-NAME) (BIND COLUMN ?COLUMN-3 LENGTH) (BIND CONCEPT ?COMPARATOR-0 delaware) (BIND CONCEPT ?ALIAS-0 RIVERALIAS0) (BIND TABLE ?TABLE-0 RIVER)))))
