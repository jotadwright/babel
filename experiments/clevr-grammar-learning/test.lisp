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
                   :entries '((:observation-sample-mode . :sequential) ;; random or sequential
                              (:determine-interacting-agents-mode . :corpus-learner)
                              (:learner-th-connected-mode . :path-exists))))) ;; :neighbours or :path-exists

 
;(add-element (make-html (get-type-hierarchy (grammar (first (interacting-agents *experiment*)))) :weights t))

;;; test single interaction
;(run-interaction *experiment*)


;;; test series of interactions
;(run-series *experiment* 10000)



#|
ISSUES
------

lexical-> item based maakt duplicate item-based cxns, er is geen check om te kijken of er al een bestaat, dan moet eigenlijk add-th-links al toegepast hebben
substitution repair maakt ook duplicates, bijv.
are there any cubes? --> holophrase
are there any spheres --> are there any x, spheres, cubes
new observation: are there any things?
==> hier had eigenlijk item-based -> lexical moeten toepassen, maar deze skipt
    dan komt hij in substitution, en maakt hij een nieuwe item-based die al bestaat!
|#


#|
TODO
----
- repairs individueel testen, nadat je die add-cxn condition hebt ingevoegd
- logica in lexical to item-based nakijken, dubbels gewoon skippen uit veiligheid, zie diff-non-overlapping-meaning functie in utils
- score cxns na interaction in :after method, willen we een upper bound? belangrijk als je wereld verandert, je zal niet dezelfde score halen!
- maak eens een repair monitor (zie Jens)
- constructiesoortmonitor invoegen: punishment toevoegen
- check handle fix! fix cxns en th-links moeten doorgegeven worden
- th links moeten niet meer in twee richtingen, mag in een richting
- visualisation configurations van th aanpassen: clustering weergeven
- cxn gewichten mogen vanaf 0
- herschrijven van functie die de volgorde van variabelen in cxns bepaalt


--
verwijder de unique identifier
zoek de cxn op basis van naam voordat je ze aanmaakt! 
(let* ((cxn-name (name cxn))
       ...
(unless (find cxn-name (constructions original-cxn-inventory) :key #'name :test #'eql))
 (add-cxn ...



|#
