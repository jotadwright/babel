(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)


(progn
  (activate-monitor trace-fcg)
  (activate-monitor trace-irl)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))

(deactivate-all-monitors)

(progn
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor display-metrics))


(defparameter *experiment*
  (make-instance 'clevr-grammar-learning-experiment
                 :entries '((:learner-cxn-supplier . :ordered-by-label-and-score)
                            (:observation-sample-mode . :random) ;; random first or all
                            (:determine-interacting-agents-mode . :tutor-learner)
                            (:learner-th-connected-mode . :path-exists)))) ;; :neighbours or :path-exists
                             
;;; test single interaction
(run-interaction *experiment*)


;;; test series of interactions
;(progn
  ;(wi::reset)
(run-series *experiment* 500)

;; show the type hierarchy
(let* ((cxn-inventory (grammar (learner *experiment*)))
       (th (get-type-hierarchy cxn-inventory)))
  (add-element (make-html th :weights? t)))