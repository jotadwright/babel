(in-package :experiment-framework)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; This file declares all web monitors of the crs conventionality package   ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event routine-conceptualisation-started (topic crs-conventionality::crs-conventionality-entity-set) (agent crs-conventionality::crs-conventionality-agent) (scene crs-conventionality::crs-conventionality-entity-set))

(define-event meta-conceptualisation-started (topic crs-conventionality::crs-conventionality-entity-set) (agent crs-conventionality::crs-conventionality-agent) (scene crs-conventionality::crs-conventionality-entity-set))

(define-event routine-conceptualisation-finished (cip fcg:construction-inventory-processor) (solution-nodes cons) (agent crs-conventionality::crs-conventionality-agent))

(define-event meta-conceptualisation-finished (fix meta-layer-learning:fix) (agent crs-conventionality::crs-conventionality-agent))

(define-event routine-interpretation-started (agent crs-conventionality::crs-conventionality-agent) (scene crs-conventionality::crs-conventionality-entity-set))

(define-event routine-interpretation-finished (agent crs-conventionality::crs-conventionality-agent) (scene crs-conventionality::crs-conventionality-entity-set))

(define-event-handler (trace-interaction interaction-started)
  (add-element '((hr)))
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "Interaction: ~a" interaction-number)))
  (add-element '((hr)))
  (add-element '((hr)))
  (add-element `((h3) ,(format nil "Interacting agents: ~a (speaker) and ~a (hearer)"
                               (utils:id (speaker interaction))
                               (utils:id (hearer interaction)))))
  (add-element (make-html (speaker interaction)))
  (add-element (make-html (hearer interaction)))
  (add-element `((h3) ,(format nil "Scene: ~a"
                               (utils:id (crs-conventionality::scene interaction)))))
  (add-element (make-html (crs-conventionality::scene interaction)))
  (add-element `((h3) ,(format nil "Topic: ~a"
                               (utils:id (crs-conventionality::topic interaction)))))
  (add-element (make-html (crs-conventionality::topic interaction))))


(define-event-handler (trace-interaction routine-conceptualisation-started)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "Start conceptualisation" )))
  (add-element `((p) ,(format nil "Agent ~a is conceptualising topic ~a"
                               (utils:id agent)
                               (utils:id topic))))
  (add-element (make-html agent :expand-initially t))
  (add-element (make-html topic))
  )

(define-event-handler (trace-interaction routine-conceptualisation-finished)
  (add-element `((h2) ,(format nil "Routine conceptualisation finished" )))
  (if (fcg:succeeded-nodes cip)
    (progn
      (add-element `((h2) ,(format nil "Solution found:" )))
      (add-element (make-html (fcg::render (fcg::car-resulting-cfs (fcg:cipn-car (first solution-nodes)))
                                           (get-configuration (crs-conventionality::grammar agent) :render-mode))))
      (add-element '((hr))))
    (add-element `((h5) ,(format nil "--> No solution in routine conceptualisation" )))))

(define-event-handler (trace-interaction meta-conceptualisation-started)
  (add-element `((h2) ,(format nil "Start invention" ))))

(define-event-handler (trace-interaction meta-conceptualisation-finished)
  (add-element `((p) ,(format nil "Invented the following form:" )))
  (add-element (make-html (fcg:render (fcg::car-resulting-cfs (first (get-data (blackboard fix) 'fcg::fixed-cars)))
                           (get-configuration (crs-conventionality::grammar agent) :render-mode))))
  (add-element '((hr)))
  )


(define-event-handler (trace-interaction routine-interpretation-started)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "Start interpretation" )))
  (add-element `((p) ,(format nil "Agent ~a is interpreting form ~a"
                               (utils:id agent)
                               (first (utterance agent)))))
  (add-element (make-html agent :expand-initially t))
 )


(define-event-handler (trace-interaction routine-interpretation-finished)
  (add-element `((h2) ,(format nil "Interpretation finished" )))
  (add-element '((hr)))
  )


;(run-interaction *naming-game-canonical*)

;(activate-monitor trace-interaction)

