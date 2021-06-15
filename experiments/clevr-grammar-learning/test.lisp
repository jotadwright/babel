(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

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
  (activate-monitor display-metrics)
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
  (activate-monitor print-a-dot-for-each-interaction))

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (make-instance 'clevr-grammar-learning-experiment
                   :entries '((:observation-sample-mode . :debug) ;; train, debug, evaluation, development
                              (:determine-interacting-agents-mode . :corpus-learner)
                              (:remove-cxn-on-lower-bound . nil)
                              (:learner-th-connected-mode . :neighbours))))) ;; :neighbours or :path-exists

;(cl-store:store (grammar (first (agents *experiment*))) (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "cxn-inventory-train-random" :type "store"))

;(add-element (make-html (get-type-hierarchy (grammar (first (agents *experiment*)))) :weights t))
;(add-element (make-html (grammar (first (agents *experiment*)))))

;(defparameter *th* (get-type-hierarchy (grammar (first (interacting-agents *experiment*)))))

;;; test single interaction
;(run-interaction *experiment*)

;;; test series of interactions
;(run-series *experiment* (length (question-data *experiment*)))

;(run-series *experiment* 100)

;

;(formulate '((get-context ?source-1) (filter ?target-2 ?target-1 ?color-6) (unique ?target-object-1 ?target-2) (bind shape-category ?shape-8 thing) (bind attribute-category ?attribute-2 shape) (filter ?target-1 ?source-1 ?shape-8) (bind color-category ?color-6 blue) (query ?target-3 ?target-object-1 ?attribute-2)) :gold-standard-utterance "What shape is the blue object?" :cxn-inventory (grammar (first (interacting-agents *experiment*))))


#|

QUESTIONS
---------
- hoe werkt de hashfunctie? waar zit de functie die gebruikt w bij opzoeken?
- 

 
NOTES
------
OVERAL ORIGINAL behalve in FCG apply!

 
ISSUES
------



TODO
----

1. monitors
-- dot for each interaction: geef letters aan repairs en print de letter v d repair ipv een X
- fix plots voor serie 1!
- cxns per type OK
- repairs
- th links / comm success
- dot for each interaction: geef letters aan repairs en print de letter v d repair ipv een X
- stage 1 en dan stage 2 met dezelfde grammatica
- stage 2 loopt vast!

|#
