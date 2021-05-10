(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

(progn
  ;(activate-monitor trace-fcg)
  ;(activate-monitor trace-irl)
  (activate-monitor display-metrics)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))

;(deactivate-all-monitors)

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
;(run-series *experiment* 200)


#|
problems:

score loopt een iteratie achter!

|#



#|
TODO
----
- maak monitor die de integriteit checkt: is het totaal aantal cxns gelijk aan het interactienummer - het totaal aantal successes?


|#
