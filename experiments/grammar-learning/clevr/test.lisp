(ql:quickload :grammar-learning)
(in-package :grammar-learning)


(setf *raise-errors* t)
;; full logging except trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

;; minimal logging after 100 interactions with type hierarchy
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction))

;; minimal logging after 100 interactions
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor export-categorial-network-evolution-to-jsonl)
  (activate-monitor export-type-hierarchy-to-json))

;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

;; full logging except trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  ;(activate-monitor export-categorial-network-evolution-to-jsonl)
  ;(activate-monitor show-type-hierarchy-after-n-interactions)
  ;(activate-monitor trace-interactions-in-wi)
  )

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (eval `(make-instance 'grammar-learning-experiment
                   :entries '((:determine-interacting-agents-mode . :corpus-learner)
                         (:observation-sample-mode . :debug)
                         (:meaning-representation . :irl)
                         (:de-render-mode . :de-render-string-meets-no-punct)
                         (:corpus-files-root . ,(merge-pathnames
                                     (make-pathname :directory '(:relative "clevr-grammar-learning"))
                                     cl-user:*babel-corpora*))
                         (:corpus-data-file . ,(make-pathname :directory '(:relative "train")
                                                   :name "stage-1" :type "jsonl")))))))
                              

;(cl-store:store (grammar (first (agents *experiment*))) (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "cxn-inventory-train-random" :type "store"))

;(add-element (make-html (categorial-network (grammar (first (agents *experiment*)))) :weights t))
;(add-element (make-html (grammar (first (agents *experiment*)))))

;(defparameter *th* (categorial-network (grammar (first (interacting-agents *experiment*)))))

;;; test single interaction
;(run-interaction *experiment*)

;;; test series of interactions
;(run-series *experiment* (length (question-data *experiment*)))

;(run-series *experiment* 1000)
; 83 has issue



#|
ISSUES:

I1: order of variables in args may be different, but cxn can still apply, the resulting meaning network is not connected! See observation #... => make it a set!
I2: there aren't that many holophrases to learn from, add the repairs that learn from a combination of cxns
I3: in general: the substitution repair learns something different than the others, namely the filters are not included, what is the least general generalisation of two differing observations?

 
TODO:
- fix AMR functions

- debug issue in failing processing!
- create testcases for new repairs
- add new repairs that don't only start from holophrases, but also from minimally differing combinations of cxns

|#