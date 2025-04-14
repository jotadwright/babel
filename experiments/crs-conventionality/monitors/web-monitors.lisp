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


(define-event interaction-finished (experiment t)
                                   (interaction t)
                                   (interaction-number number))

(define-event-handler (trace-interaction interaction-finished)
  (if (communicated-successfully interaction)
    (add-element `((h2 :style "background-color: HoneyDew; opacity:0.6; color: green; padding: 5px;")
                   "Interaction succeeded"))
    (add-element `((h2 :style "background-color: LavenderBlush; color: red; padding: 5px;")
                   "Interaction failed"))))


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
        (add-element `((p) ,(format nil "The speaker increased the score of the following construction by ~a:"
                                    (compute-score-increase (attr-val applied-cxn-speaker :score) (learning-rate speaker)))))
        (add-element (make-html (original-cxn applied-cxn-speaker) :expand-initially nil))
        (add-element `((p) ,(format nil "The hearer increased the score of the following construction by ~a:"
                                    (compute-score-increase (attr-val applied-cxn-hearer :score) (learning-rate hearer)))))
        (add-element (make-html (original-cxn applied-cxn-hearer) :expand-initially nil)))
      (progn
        (add-element `((p) ,(format nil "The speaker decreased the score of the following construction by ~a:"
                                    (compute-score-decrease (attr-val applied-cxn-speaker :score) (learning-rate speaker)))))
        (add-element (make-html (original-cxn applied-cxn-speaker) :expand-initially nil))))))

(defun compute-score-increase (score learning-rate)
  (- score (/ (- score learning-rate)
              (- 1 learning-rate))))

(defun compute-score-decrease (score learning-rate)
  (- (/ score (- 1 learning-rate))
     score))


;; Adoption finished ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define-event adoption-finished (cxn fcg::fcg-construction) (invention t))

(define-event-handler (trace-interaction adoption-finished)
  (when invention
    (add-element `((h2 :style "background-color: LightGray; padding: 5px;") ,(format nil "Adoption"))))
  (add-element `((p) ,(format nil "The hearer adopted the following construction: ")))
  (add-element (make-html cxn :expand-initially nil)))


;; Determine Coherence-finished ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event determine-coherence-finished (speaker crs-conventionality-agent)
                                           (hearer crs-conventionality-agent))

(define-event-handler (trace-interaction determine-coherence-finished)
  (cond ((equal (utterance speaker) (conceptualised-utterance hearer))
         (add-element `((b :style "color: green;")
                        ,(format nil "Hearer would have uttered \"~a\" as well." (first (conceptualised-utterance hearer))))))
         ((not (conceptualised-utterance hearer))
          (add-element `((b :style "color: red;")
                         ,(format nil "Hearer would not have uttered \"~a\"." (first (utterance speaker))))))
        (t
         (add-element `((b :style "color: red;")
                        ,(format nil "Hearer would have uttered \"~a\" instead of \"~a\"." (first (conceptualised-utterance hearer)) (first (utterance speaker))))))))

;; Introduce New Agents ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event introduce-new-agents-finished (population crs-conventionality-population)
                                            (new-agents list))

(define-event-handler (trace-interaction introduce-new-agents-finished)
  (add-element `((h1 :style "background-color: DarkBlue; color:white; padding: 5px; margin-top: 50px")
                 ,(format nil "Population changed")))
  (add-element `((p) ,(format nil "Introduced ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}." (mapcar #'id  new-agents))))
  (add-social-network (agents population)))


;; Replace Agents ;;
;;;;;;;;;;;;;;;;;;;;

(define-event replace-agents-finished (population crs-conventionality-population)
                                      (replaced-agents list)
                                      (new-agents list))

(define-event-handler (trace-interaction replace-agents-finished)
  (add-element `((h1 :style "background-color: DarkBlue; color:white; padding: 5px; margin-top: 50px")
                 ,(format nil "Population changed")))
  (let ((pairs (loop for replaced-agent in (mapcar #'id  replaced-agents)
                     for new-agent in (mapcar #'id  new-agents)
                     collect (format nil "~a by ~a" replaced-agent new-agent))))
    (add-element `((p) ,(format nil "Replaced ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}." pairs))))
  (add-social-network (agents population)))


(defun add-social-network (agents)
  (let* ((filename (format nil "population-graph-~a" (make-time-stamp)))
         (filepath (format nil "~a.svg" filename)))
    (population-network->graphviz agents :name filename :make-image t :open-image nil :use-labels? t)
    (add-element `((img :src ,filepath :height "500")))))


;; Evaluate IRL program ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-monitor trace-irl-crs)

(define-event-handler (trace-irl-crs irl::evaluate-irl-program-started)
  nil)


(define-event-handler (trace-irl-crs irl::evaluate-irl-program-finished)
  nil)


(define-event-handler (trace-irl-crs irl::chunk-composer-finished)
  (add-element `((div)
                 ((table :class "two-col")
                  ((tbody)
                   ((tr)
                    ((td) "Composition process: ")
                    ((td) ,(irl::make-collapsed-html-for-composition-process irl::composer)))))))
  
  (when (and (irl::solutions irl::composer)
              (not (length= (irl::solutions irl::composer) irl::solutions)))
    (add-element '((h3) "All composer solutions so far"))
    (irl::composer-solutions->html (irl::solutions irl::composer))))


;; FCG ;;
;;;;;;;;;

(define-monitor trace-fcg-crs)

(define-event-handler (trace-fcg-crs cxn-deleted)
  nil)

(define-event-handler (trace-fcg-crs fcg::fcg-apply-w-n-solutions-started)
  (add-element (make-html (fcg::original-cxn-set construction-inventory))))


(define-event-handler (trace-fcg-crs fcg::fcg-apply-w-n-solutions-finished)
  (add-element `((div)
                 ((table :class "two-col")
                  ((tbody)
                   ,(fcg::make-tr-for-cip-tree-fcg-light (fcg::top-node cip) "application process"))))))


