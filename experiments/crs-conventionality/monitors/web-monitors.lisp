(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                ;;
;;  Declares all web monitors of the crs conventionality package  ;;
;;                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Interaction ;;
;;;;;;;;;;;;;;;;;
(define-event interaction-started (experiment crs-conventionality-experiment)
                                  (interaction crs-conventionality-interaction)
                                  (interaction-number number))

(define-event-handler (trace-interaction interaction-started)
  (add-element `((h1 :style "background-color: black; color:white; padding: 5px; margin-top: 50px")
                 ,(format nil "Interaction ~a"
                               interaction-number)))
  
  (add-element `((div)
                   ((table :class "two-col")
                    ((tbody)
                     ((tr)
                      ((td) "speaker")
                      ((td) ,(make-html (speaker interaction))))
                     ((tr)
                      ((td) "hearer")
                      ((td) ((div :id ,(mkstr (make-id 'subtree-id)))
                             ,(make-html (hearer interaction)))))
                     ((tr)
                      ((td) "scene")
                      ((td) ((div :id ,(mkstr (make-id 'subtree-id)))
                             ,(make-html (scene interaction)))))
                     ((tr)
                      ((td) "topic")
                      ((td) ((div :id ,(mkstr (make-id 'subtree-id)))
                             ,(make-html (topic interaction))))))))))


;; Routine Conceptualisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-event routine-conceptualisation-started (topic crs-conventionality-entity-set)
                                                (agent crs-conventionality-agent)
                                                (scene crs-conventionality-entity-set))

(define-event-handler (trace-interaction routine-conceptualisation-started)
  (add-element `((h2 :style "background-color: LightGray; padding: 5px;")
                 ,(format nil "Conceptualisation")))
  
  (add-element `((h3) ,(format nil "Routine conceptualisation"))))


(define-event routine-conceptualisation-finished (cip fcg:construction-inventory-processor)
                                                 (solution-nodes cons)
                                                 (agent crs-conventionality-agent))

(define-event-handler (trace-interaction routine-conceptualisation-finished)

  (if (fcg:succeeded-nodes cip)
    (progn
      (add-element `((b :style "background-color: green; color: white; padding: 5px;")
                     ,(format nil "Routine conceptualisation completed" )))
      (add-element `((br :style "margin: 20px")))
      (add-element `((a) ,(format nil "Speaker uttered: ")))
      (add-element `((b) ,(format nil "\"~a\"" (first (fcg::render (fcg::car-resulting-cfs (fcg:cipn-car (first solution-nodes)))
                                           (get-configuration (grammar agent) :render-mode))))))
      (add-element '((hr :style "border-top: 1px dashed; background-color:transparent;"))))


    (progn
      (add-element `((b :style "background-color: red; color: white; padding: 5px;")
                     ,(format nil "Routine conceptualisation failed" )))
      (add-element `((br :style "margin: 20px"))))))


;; Meta Conceptualisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event meta-conceptualisation-started (topic crs-conventionality-entity-set)
                                             (agent crs-conventionality-agent)
                                             (scene crs-conventionality-entity-set))

(define-event-handler (trace-interaction meta-conceptualisation-started)
  (add-element `((h3) ,(format nil "Meta layer conceptualisation"))))


(define-event meta-conceptualisation-finished (fix meta-layer-learning:fix)
                                              (agent crs-conventionality-agent))

(define-event-handler (trace-interaction meta-conceptualisation-finished)
  (add-element `((div)
                 ((table :class "two-col")
                  ((tbody)
                   ((tr)
                    ((td) "Diagnosed problem:")
                    ((td) ,(make-html(meta-layer-learning::problem fix))))
                   ((tr)
                    ((td) "Fix issued by following repair:")
                    ((td) ,(make-html (meta-layer-learning::issued-by fix))))
                   ((tr)
                    ((td) "Learned construction:")
                    ((td) ,(make-html (meta-layer-learning::restart-data fix))))))))

  (add-element `((p :style "display:inline") ,(format nil "Speaker uttered: " )))
  (add-element `((b) ,(format nil "\"~a\"" (first (fcg:render (fcg::car-resulting-cfs (first (get-data (blackboard fix) 'fcg::fixed-cars)))
                           (get-configuration (grammar agent) :render-mode))))))
  (add-element '((hr :style "border-top: 1px dashed; background-color:transparent;"))))


;; Routine Interpretation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-event routine-interpretation-started (agent crs-conventionality-agent)
                                             (scene crs-conventionality-entity-set))


(define-event-handler (trace-interaction routine-interpretation-started)
  (add-element `((h2 :style "background-color: LightGray; padding: 5px;")
                 ,(format nil "Interpretation" )))
  (add-element `((p) ,(format nil "Hearer tries to interpret \"~a\"."
                               (first (utterance agent))))))


(define-event routine-interpretation-finished (agent crs-conventionality-agent)
                                              (scene crs-conventionality-entity-set))

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


;; Alignment ;;
;;;;;;;;;;;;;;;
(define-event alignment-finished (speaker crs-conventionality-agent) (hearer crs-conventionality-agent))

(define-event-handler (trace-interaction alignment-finished)
  (add-element `((h2 :style "background-color: LightGray; padding: 5px;") ,(format nil "Alignment")))
  ; TODO: add punished and rewarded cxns to interface
  )


;; Adoption finished ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define-event adoption-finished (cxn fcg::fcg-construction))

(define-event-handler (trace-interaction adoption-finished)
  (add-element `((a) ,(format nil "Hearer adopted: ")))
  (add-element (make-html cxn)))


;; Evaluate IRL program ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-monitor trace-irl-crs)

(define-event-handler (trace-irl-crs irl::evaluate-irl-program-started)
  nil)


(define-event-handler (trace-irl-crs irl::evaluate-irl-program-finished)
  nil)


;; FCG ;;
;;;;;;;;;

(define-monitor trace-fcg-crs)

(define-event-handler (trace-fcg-crs cxn-deleted)
  nil)


(define-event-handler (trace-fcg-crs fcg::cip-node-expanded)
  (add-element `((h4) "HALLO"))
  (add-element `((table :class "two-col")
                 ((tbody)
                  ,(make-tr-for-cip-tree-fcg-light cipn "expansion" 
                                     :hide-subtree-with-duplicates nil)
                  ,(make-tr-for-cip-tree-fcg-light (top-node (cip cipn)) "new tree" )
                  ,(make-tr-for-cip-queue-fcg-light (cip cipn) "new queue" )))))

(define-event-handler (trace-fcg-crs fcg::fcg-apply-w-n-solutions-started)
  (add-element (make-html (fcg::original-cxn-set construction-inventory))))


(define-event-handler (trace-fcg-crs fcg::fcg-apply-w-n-solutions-finished)
  (add-element `((div)
                 ((table :class "two-col")
                  ((tbody)
                   ,(fcg::make-tr-for-cip-tree-fcg-light (fcg::top-node cip) "application process"))))))