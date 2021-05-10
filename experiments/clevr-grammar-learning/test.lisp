(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

(progn
  (activate-monitor display-metrics)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

(deactivate-all-monitors)

(progn
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

(deactivate-all-monitors)

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


;;; test single interaction
;(run-interaction *experiment*)


;;; test series of interactions
;(run-series *experiment* 50)
;; modulo 100
;(with-activated-monitor 


#|
problems:

score loopt een iteratie achter!

|#



#|
TODO
----
- maak monitor die de integriteit checkt: is het totaal aantal cxns gelijk aan het interactienummer - het totaal aantal successes? OK


|#
