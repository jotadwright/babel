(ql:quickload :clevr-grammar-learning)
(in-package :clevr-grammar-learning)

;(hash
(progn
  ;(activate-monitor trace-fcg)
  ;(activate-monitor trace-irl)
  ;(activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor trace-interactions-in-wi))

;(deactivate-all-monitors)

(progn
  (activate-monitor print-a-dot-for-each-interaction)
  ;(activate-monitor display-metrics))
  (activate-monitor gl::trace-grammar-learning-verbose)) ;todo: export this monitor


(defparameter *experiment*
  (make-instance 'clevr-grammar-learning-experiment
                 :entries '((:observation-sample-mode . :random) ;; random first or all
                            (:determine-interacting-agents-mode . :corpus-learner)
                            (:learner-th-connected-mode . :neighbours)))) ;; :neighbours or :path-exists
                             
;;; test single interaction
;(run-interaction *experiment*)


;;; test series of interactions
;(progn
  ;(wi::reset)
;(run-series *experiment* 200)

(comprehend "How big is the rubber cube?" :cxn-inventory (grammar (first (interacting-agents *experiment*))) :gold-standard-meaning '((CLEVR-WORLD:GET-CONTEXT #:?SOURCE-153736) (CLEVR-WORLD:FILTER #:?TARGET-418573 #:?TARGET-418574 #:?COLOR-84382) (CLEVR-WORLD:UNIQUE #:?TARGET-OBJECT-40658 #:?TARGET-418575) (UTILS:BIND CLEVR-WORLD:SHAPE-CATEGORY #:?SHAPE-164583 CLEVR-WORLD:CUBE) (UTILS:BIND CLEVR-WORLD:ATTRIBUTE-CATEGORY #:?ATTRIBUTE-60248 CLEVR-WORLD:MATERIAL) (CLEVR-WORLD:FILTER #:?TARGET-418576 #:?SOURCE-153737 #:?SHAPE-164584) (UTILS:BIND CLEVR-WORLD:COLOR-CATEGORY #:?COLOR-84383 CLEVR-WORLD:BLUE) (CLEVR-WORLD:QUERY #:?TARGET-418577 #:?TARGET-OBJECT-40659 #:?ATTRIBUTE-60249)))


;(fcg::consolidate-repairs
(grammar (first (interacting-agents *experiment*)))