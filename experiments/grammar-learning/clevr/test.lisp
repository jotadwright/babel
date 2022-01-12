(ql:quickload :grammar-learning)
(in-package :grammar-learning)

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
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor export-type-hierarchy-evolution-to-jsonl)
  (activate-monitor export-type-hierarchy-to-json))

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (make-instance 'grammar-learning-experiment
                   :entries '((:observation-sample-mode . :sort-length-ascending))))) ;; train, debug, evaluation, development

;(cl-store:store (grammar (first (agents *experiment*))) (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "cxn-inventory-train-random" :type "store"))

;(add-element (make-html (get-type-hierarchy (grammar (first (agents *experiment*)))) :weights t))
;(add-element (make-html (grammar (first (agents *experiment*)))))

;(defparameter *th* (get-type-hierarchy (grammar (first (interacting-agents *experiment*)))))

;;; test single interaction
;(run-interaction *experiment*)

;;; test series of interactions
;(run-series *experiment* (length (question-data *experiment*)))

;(run-series *experiment*  100)
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
- fix namen van monitors
- average over series van aantal nodes en edges in TH
- voorbeeld van TH visualisatie


- add th links: weg met die comprehend, gwn links maken, niet checken of de indirecte bestaat


- stage 1 en dan stage 2 met dezelfde grammatica
- stage 2 loopt vast! assertion error, dus repairs failen en comprehension failt


1 epoch
test set (uiteindelijke metric)

DONE
----
- 2 epochs: plak de data 2x aan elkaar (geshuffeld)
- maak een export monitor voor categorial networks die om de n interacties een timestep toevoegt met de volledige graph, in JSON formaat
- maak het een optie om een grammatica mee te geven waarop hij verder leert (zonder in evaluation mode te zitten)


|#
