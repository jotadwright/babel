(in-package :experiment-framework)

;;
;; !!! WARNING : events are currently all defined in :experiment-framework (TODO)
;;


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
  (add-element `((h1 :style "background-color: black; color:white; padding: 5px; margin-top: 50px")
                 ,(format nil "Interaction ~a"
                               interaction-number)))
  (add-element `((b) ,(format nil "Speaker: ")))
  (add-element (make-html (speaker interaction)))
  (add-element `((br)))
  (add-element `((b) ,(format nil "Hearer: ")))
  (add-element (make-html (hearer interaction)))
  (add-element `((br)))
  (add-element `((b) ,(format nil "Scene: ")))
  (add-element (make-html (crs-conventionality::scene interaction)))
  (add-element `((br)))
  (add-element `((b) ,(format nil "Topic: ")))
  (add-element (make-html (crs-conventionality::topic interaction))))


(define-event-handler (trace-interaction routine-conceptualisation-started)
  (add-element `((h2 :style "background-color: LightGray; padding: 5px;") ,(format nil "Conceptualisation")))
  (add-element `((p) ,(format nil "Speaker conceptualises the topic.")))
  
  (add-element `((h3) ,(format nil "Routine conceptualisation")))
  )

(define-event-handler (trace-interaction routine-conceptualisation-finished)

  (if (fcg:succeeded-nodes cip)
    (progn
      (add-element `((b :style "background-color: green; color: white; padding: 5px;")
                     ,(format nil "Routine conceptualisation succeeded" )))
      (add-element `((br :style "margin: 20px")))
      (add-element `((a) ,(format nil "Speaker uttered: ")))
      (add-element `((b) ,(format nil "\"~a\"" (first (fcg::render (fcg::car-resulting-cfs (fcg:cipn-car (first solution-nodes)))
                                           (get-configuration (crs-conventionality::grammar agent) :render-mode))))))
      (add-element '((hr :style "border-top: 1px dashed; background-color:transparent;"))))


    (progn
      (add-element `((b :style "background-color: red; color: white; padding: 5px;")
                     ,(format nil "Routine conceptualisation failed" )))
      (add-element `((br :style "margin: 20px"))))))

(define-event-handler (trace-interaction meta-conceptualisation-started)
  (add-element `((h3) ,(format nil "Meta layer conceptualisation"))))

(Define-event-handler (trace-interaction meta-conceptualisation-finished)
  (add-element `((b) ,(format nil "Diagnosed problem: ")))
  (add-element (make-html (meta-layer-learning::problem fix)))
  (add-element `((br)))
  (add-element `((b) ,(format nil "Fix issued by following repair: ")))
  (add-element (make-html (meta-layer-learning::issued-by fix)))
  (add-element `((br)))
  (add-element `((b) ,(format nil "Learned construction: ")))
  (add-element (make-html (meta-layer-learning::restart-data fix)))
  
  (add-element `((a) ,(format nil "Speaker uttered:" )))
  (add-element `((b) ,(format nil "\"~a\"" (first (fcg:render (fcg::car-resulting-cfs (first (get-data (blackboard fix) 'fcg::fixed-cars)))
                           (get-configuration (crs-conventionality::grammar agent) :render-mode))))))
  (add-element '((hr :style "border-top: 1px dashed; background-color:transparent;")))
  )


(define-event-handler (trace-interaction routine-interpretation-started)
  (add-element `((h2 :style "background-color: LightGray; padding: 5px;") ,(format nil "Interpretation" )))
  (add-element `((p) ,(format nil "Hearer tries to interpret \"~a\"."
                               (first (utterance agent)))))
 )


(define-event-handler (trace-interaction routine-interpretation-finished)
  (add-element `((h2) ,(format nil "Interpretation finished" )))
  (add-element '((hr :style "border-top: 1px dashed; background-color:transparent;")))

  (if (fcg:succeeded-nodes cip)
    (progn
      (add-element `((b :style "background-color: green; color: white; padding: 5px;")
                     ,(format nil "Routine interpretation succeeded" )))
      (add-element `((br :style "margin: 20px"))))
    (progn
      (add-element `((b :style "background-color: red; color: white; padding: 5px;")
                     ,(format nil "Routine conceptualisation failed" )))
      (add-element `((br :style "margin: 20px"))))))


