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

;(run-series *experiment* 400)


#|

QUESTIONS
---------

 
NOTES
------
OVERAL ORIGINAL behalve in FCG apply!

NOOIT AVERAGEN OVER SERIES! Error bars gebruiken bijv 75 25= indicatie van variantie
 
ISSUES
------



TODO
----
- add series number to exported grammar! (currently overwritten!)

- 10 series
- run parallel batch

- stage 1 en dan stage 2 met dezelfde grammatica
- stage 2 loopt vast!


DONE
----
-2 epochs: plak de data 2x aan elkaar (geshuffeld)



|#
