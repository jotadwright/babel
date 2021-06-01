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
                              (:remove-cxn-on-lower-bound . t)
                              (:learner-th-connected-mode . :neighbours))))) ;; :neighbours or :path-exists

;(add-element (make-html (get-type-hierarchy (grammar (first (interacting-agents *experiment*)))) :weights t))
;(add-element (make-html (grammar (first (interacting-agents *experiment*)))))

;(defparameter *th* (get-type-hierarchy (grammar (first (interacting-agents *experiment*)))))

;;; test single interaction
;(run-interaction *experiment*)

;;; test series of interactions
;(run-series *experiment* 100)

#|
ISSUES
------
OVERAL ORIGINAL behalve in FCG apply!|#
#|
TODO
----
- lexical cxns hashen
- constructiesoortmonitor invoegen, zie code jens
- repair monitor (zie Jens): welke repair heeft toegepast in een interactie?

- repairs individueel testen, nadat je die add-cxn condition hebt ingevoegd
- logica in lexical to item-based nakijken, dubbels gewoon skippen uit veiligheid, zie diff-non-overlapping-meaning functie in utils
- constructiesoortmonitor invoegen: 
- check handle fix! fix cxns en th-links moeten doorgegeven worden
- th links moeten niet meer in twee richtingen, mag in een richting
 ok voor subst
 ok voor add lex
 nok voor addition


--
verwijder de unique identifier
zoek de cxn op basis van naam voordat je ze aanmaakt! 
(let* ((cxn-name (name cxn))
       ...
(unless (find cxn-name (constructions original-cxn-inventory) :key #'name :test #'eql))
 (add-cxn ...



|#

#| ABSTRACT CXNS:
PISTE 1
-------
als er twee opeenvolgende slots zijn, maak er een slot van, bijvoorbeeld:
Is there a X? "is there a cube?"
Is there a X Y? "is there a large cube?"
--> Is there a X? + X--> Y Z cxn "large cube" (determined noun phrase)

PISTE 2
-------
laat ook langere chunks toe bij diffs, niet enkel single lex items!
bijv: how many large cubes are there? vs how many small spheres are there?
==> large cubes cxn
==> small spheres cxn
==> how many x are there?

dan als we large of small leren krijgen we X cubes, x spheres, en X Y.

==> zo zou je ook X or Y kunnen leren!
|#
