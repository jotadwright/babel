(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

(deactivate-all-monitors)


;; full logging except trace-fcg
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))




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
                   :entries '((:observation-sample-mode . :random) ;; random or sequential
                              (:determine-interacting-agents-mode . :corpus-learner)
                              (:learner-th-connected-mode . :neighbours))))) ;; :neighbours or :path-exists
;(all-constructions-of-current-label 
;(add-element (make-html (get-type-hierarchy (grammar (first (interacting-agents *experiment*))))))
;;; test single interaction
;(run-interaction *experiment*)


;;; test series of interactions
;(run-series *experiment* 100)




#|
ISSUES
------
integrity check werkt alleen voor holophrases
duplicate item-based cxns want add th links is niet actief, test eens met andere th mode
|#


#|
TODO
----
- maak monitor die de integriteit checkt: is het totaal aantal cxns gelijk aan het interactienummer - het totaal aantal successes? OK voor holophrases tot 10K
- repairs individueel testen
- vervang (constructions cipn door all-constructions --> waar zit deze functie?
- in alle repairs zit een lijn die de type hierarchy reset in handle fix, verwijder deze
- score cxns na interaction in :after method, willen we een upper bound?
- lengte opnemen in hash key? op basis van aantal meets constraints
- maak eens een repair monitor (zie Jens)


|#
