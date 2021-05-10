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
  ;(activate-monitor display-metrics)
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
;(run-series *experiment* 5)




#|
ISSUES
------
- type hierarchy werkt niet, test eens de 6 eerste sequentieel, een gemaakte item-based past niet toe.
- hij werkt wel binnen de repair, maar wordt nadien gewist, test eens een repair, met de debugger zie je de th links, maar eens de interactie voorbij is zijn ze weg!

|#


#|
TODO
----
- maak monitor die de integriteit checkt: is het totaal aantal cxns gelijk aan het interactienummer - het totaal aantal successes? OK
- repairs individueel testen

|#
