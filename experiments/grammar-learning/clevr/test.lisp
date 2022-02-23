(ql:quickload :grammar-learning)
(in-package :grammar-learning)



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

;(add-element (make-html (get-type-hierarchy (grammar (first (agents *experiment*)))) :weights t))
;(add-element (make-html (grammar (first (agents *experiment*)))))

;(defparameter *th* (categorial-network (grammar (first (interacting-agents *experiment*)))))

;;; test single interaction
;(run-interaction *experiment*)

;;; test series of interactions
;(run-series *experiment* (length (question-data *experiment*)))

;(run-series *experiment*  41)
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
- reverse exported jsonl graph
- 


- add th links: weg met die comprehend, gwn links maken, niet checken of de indirecte bestaat


- stage 1 en dan stage 2 met dezelfde grammatica
- stage 2 loopt vast! assertion error, dus repairs failen en comprehension failt


|#
