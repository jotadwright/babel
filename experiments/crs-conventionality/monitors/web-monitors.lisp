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
  (cond ((eq 'speaker (experiment-framework::discourse-role agent))
         (add-element `((h2 :style "background-color: LightGray; padding: 5px;")
                        ,(format nil "Conceptualisation")))
         (add-element `((p) "The speaker tries to conceptualise the topic."))
         (add-element `((h3) ,(format nil "Routine conceptualisation"))))
        ((eq 'hearer (experiment-framework::discourse-role agent))
         (add-element `((h2 :style "background-color: LightGray; padding: 5px;")
                        ,(format nil "Conventionality check")))
         (add-element `((p) "We check if the hearer would have conceptualised the same utterance."))
         )))

(define-event routine-conceptualisation-finished (cip fcg:construction-inventory-processor)
                                                 (solution-nodes cons)
                                                 (agent crs-conventionality-agent))

(define-event-handler (trace-interaction routine-conceptualisation-finished)
  (if (fcg:succeeded-nodes cip)
    (progn
      (add-element `((b :style "color: green;")
                     ,(format nil "Routine conceptualisation succeeded" )))
      (add-element `((br :style "margin: 20px")))
      (add-element `((p :style "display: inline;") ,(format nil "Speaker uttered: ")))
      (add-element `((b) ,(format nil "\"~a\"" (first (fcg::render (fcg::car-resulting-cfs (fcg:cipn-car (first solution-nodes)))
                                           (get-configuration (grammar agent) :render-mode)))))))


    (progn
      (add-element `((b :style "color: red;")
                     ,(format nil "Routine conceptualisation failed" )))
      (add-element `((br :style "margin: 20px"))))))


;; Meta Conceptualisation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event meta-conceptualisation-started (topic crs-conventionality-entity-set)
                                             (agent crs-conventionality-agent)
                                             (scene crs-conventionality-entity-set))

(define-event-handler (trace-interaction meta-conceptualisation-started)
  (add-element '((hr :style "border-top: 1px dashed; background-color:transparent;")))
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

  (add-element '((hr :style "border-top: 1px dashed; background-color:transparent;")))
  
  (add-element `((p :style "display:inline") ,(format nil "Speaker uttered: " )))
  (add-element `((b) ,(format nil "\"~a\"" (first (fcg:render (fcg::car-resulting-cfs (first (get-data (blackboard fix) 'fcg::fixed-cars)))
                           (get-configuration (grammar agent) :render-mode)))))))


;; Routine Interpretation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-event routine-interpretation-started (agent crs-conventionality-agent)
                                             (scene crs-conventionality-entity-set))


(define-event-handler (trace-interaction routine-interpretation-started)
  (add-element `((h2 :style "background-color: LightGray; padding: 5px;")
                 ,(format nil "Interpretation" )))
  (add-element `((p) ,(format nil "Hearer tries to interpret \"~a\"."
                               (first (utterance agent))))))


(define-event routine-interpretation-finished (cip fcg:construction-inventory-processor)
                                              (agent crs-conventionality-agent)
                                              (scene crs-conventionality-entity-set))

(define-event-handler (trace-interaction routine-interpretation-finished)
  (if (fcg:succeeded-nodes cip)
    (progn
      (add-element `((b :style "color: green;")
                     ,(format nil "Routine interpretation succeeded" )))
      (add-element `((br :style "margin: 20px"))))
    (progn
      (add-element `((b :style "color: red;")
                     ,(format nil "Routine interpretation failed" )))
      (add-element `((br :style "margin: 20px"))))))


;; Alignment ;;
;;;;;;;;;;;;;;;
(define-event alignment-started (speaker crs-conventionality-agent) (hearer crs-conventionality-agent))

(define-event-handler (trace-interaction alignment-started)
  (add-element `((h2 :style "background-color: LightGray; padding: 5px;") ,(format nil "Alignment"))))

(define-event alignment-finished (speaker crs-conventionality-agent)
                                 (hearer crs-conventionality-agent)
                                 (interaction crs-conventionality-interaction))

(define-event-handler (trace-interaction alignment-finished)
  (let ((applied-cxn-speaker (first (applied-constructions speaker)))
        (applied-cxn-hearer (first (applied-constructions hearer))))
    (if (communicated-successfully interaction)
      (progn
        (add-element `((p) ,(format nil "The speaker increased the score of the following construction by ~a:" (learning-rate speaker))))
        (add-element (make-html (original-cxn applied-cxn-speaker) :expand-initially nil))
        (add-element `((p) ,(format nil "The hearer increased the score of the following construction by ~a:" (learning-rate speaker))))
        (add-element (make-html (original-cxn applied-cxn-hearer) :expand-initially nil)))
      (progn
        (add-element `((p) ,(format nil "The speaker decreased the score of the following construction by ~a:" (learning-rate speaker))))
        (add-element (make-html (original-cxn applied-cxn-speaker) :expand-initially nil))))))


;; Adoption finished ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define-event adoption-finished (cxn fcg::fcg-construction))

(define-event-handler (trace-interaction adoption-finished)
  (add-element `((p) ,(format nil "The hearer adopted the following construction: ")))
  (add-element (make-html cxn :expand-initially nil)))


;; Determine Coherence-finished ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event determine-coherence-finished (speaker crs-conventionality-agent)
                                           (hearer crs-conventionality-agent))

(define-event-handler (trace-interaction determine-coherence-finished)
  (cond ((eq (utterance speaker) (utterance hearer))
         (add-element `((b :style "color: green;")
                        ,(format nil "Listener would have uttered \"~a\" as well." (first (utterance hearer))))))
        (else
         (add-element `((b :style "color: green;")
                        ,(format nil "Listener would have uttered \"~a\" instead of \"~a\"." (first (utterance hearer)) (first (utterance speaker))))))))


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