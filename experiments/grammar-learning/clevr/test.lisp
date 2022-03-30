(ql:quickload :grammar-learning)
(in-package :grammar-learning)



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
#|

QUESTIONS
---------

NOTES
------
use original-cxn everywhere, except in fcg apply (uses processing-cxn)

NOOIT AVERAGEN OVER SERIES! Error bars gebruiken bijv 75 25= indicatie van variantie
 
ISSUES
------

TODO
----
- for holistic to item-based, use fcg-apply for all holistic cxns, then create item-based cxn from whatever is in root
- get them all, then sort by longest, then look for conflicts between them:
 eg. utterance: what is the shape of the yellow object?
 matched: shape, yellow object, object, shape of the yellow
 sort: shape of the yellow, yellow object, object, shape
 number of collisions = 3:
   shape of the yellow vs yellow object
   shape of the yellow vs shape
   yellow object vs object
   
 try: shape of the yellow, object ok, => remove all collisions from Q, create new Q with alternatives, break symmetry by doing set diff
      shape of the yellow, ok
      object: ok
      uncovered = length of what is left in root = what is the = 3
 try: yellow object, shape

example:
utterance: what is the color of the sphere
step 1: find all matching cxns:
        what is the color of the, what is the, color of the cube
step 2: try longest first:
        what is the color of the : uncovered = 1, collisions: what is the, color of the cube
step 3: try collisions (while collisions: try)
        what is the + color of the cube: uncovered = 0
 
- for item-based to holistic cxn, use fcg-apply for all holistic cxns, then the item-based cxns (must be at least one!), then create the missing holistic cxns for whatever is left in the root if it's continuous
- fix monitor names
- reverse exported jsonl graph

|#
