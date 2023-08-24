(ql:quickload :pattern-finding)
(in-package :pf)

;; activate monitors
(progn
  (deactivate-all-monitors)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))

(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  ;(activate-monitor show-type-hierarchy-after-n-interactions)
  )

;; make experiment
;; data = all ingredient lists of all recipes
(progn
  (wi::reset)
  (notify reset-monitors)
  (reset-id-counters)
  (defparameter *experiment*
    (make-instance 'pattern-finding-experiment
                   :entries `((:comprehend-all-n . 2)
                              (:shuffle-data-p . nil)
                              (:number-of-epochs . 5)
                              (:learner-cxn-supplier . :hashed-scored-labeled) ;; do NOT ignore cxns with negative score...
                              (:corpus-directory . ,(babel-pathname :directory '("experiments" "grammar-learning"
                                                                                 "cooking" "data")))
                              (:corpus-file . ,(make-pathname :name "benchmark-ingredients-cleaned" :type "jsonl"))))))

(length (corpus *experiment*))

(run-series *experiment* (length (corpus *experiment*)))

(defparameter *cxn-inventory* (grammar (first (agents *experiment*))))
(add-element (make-html *cxn-inventory* :sort-by-type-and-score t))
(add-element (make-html (categorial-network *cxn-inventory*)))