(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

;; full logging
(progn
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

(deactivate-all-monitors)


;; full logging except trace-fcg
(progn
  (activate-monitor display-metrics)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

(deactivate-all-monitors)


;; minimal logging after 100 interactions
(progn
  (activate-monitor display-metrics)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor print-a-dot-for-each-interaction))
 

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (make-instance 'clevr-grammar-learning-experiment
                   :entries '((:observation-sample-mode . :sequential) ;; random or sequential
                              (:determine-interacting-agents-mode . :corpus-learner)
                              (:learner-th-connected-mode . :neighbours))))) ;; :neighbours or :path-exists

(progn
  (defparameter *type-hierarchy* (get-type-hierarchy (grammar (first (interacting-agents *experiment*)))))
  (add-element (make-html *type-hierarchy*)))
;;; test single interaction
;(run-interaction *experiment*)


;;; test series of interactions
;(run-series *experiment* 200)




#|
ISSUES
------
integrity check werkt alleen voor holophrases
|#


#|
TODO
----
- maak monitor die de integriteit checkt: is het totaal aantal cxns gelijk aan het interactienummer - het totaal aantal successes? OK voor holophrases tot 10K
- repairs individueel testen
- in alle repairs zit een lijn die de type hierarchy reset in handle fix, verwijder deze
- score cxns na interaction in :after method, willen we een upper bound?
- lengte opnemen in hash key? op basis van aantal meets constraints

|#
