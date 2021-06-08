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
NOTES
------
OVERAL ORIGINAL behalve in FCG apply!

 
ISSUES
------
- niets weggooien bij lateral inhibition = OK, maar grammar size berekening moet 0-cxns eruitlaten, visualisatie ook (skip-0 keyword maken)
- sorting in make-html werkt niet, maar waarom???


TODO
----

1. monitors
- cxns per type
- repairs
- th links / comm success

2. test production!

3. unit tests
- testcase per repair, ook production

4. item based based repairs updaten en terug invoegen

5. dingen om te dubbelchecken
- logica in lexical to item-based nakijken, dubbels gewoon skippen uit veiligheid, zie diff-non-overlapping-meaning functie in utils



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
