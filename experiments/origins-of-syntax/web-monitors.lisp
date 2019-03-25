(in-package :origins-of-syntax)

;;;;;;;;;;;;
;; Events ;;
;;;;;;;;;;;;

(define-event scene-perceived (scene syntax-scene))

(define-event topic-selected (topic syntax-topic))

(define-event conceptualization-finished (topic syntax-topic) (discriminatory-features-objects list) (meaning-predicates list)
  (redundant-meaning-predicates list))

(define-event utterance-passed (utterance list))

(define-event interpretation-finished (possible-topics t))

(define-event learning-hearer-started)

(define-event learning-speaker-started)

(define-event finishing-interaction (interaction interaction))

(define-event starting-interaction (experiment syntax-experiment) (interaction interaction))

(define-event alignment-started (agent syntax-agent))

(define-event construction-rewarded (agent syntax-agent) (cxn fcg-construction))

(define-event type-hierarchy-rewarded (agent syntax-agent) (th-links list))

(define-event type-hierarchy-punished (agent syntax-agent) (th-links list))

(define-event construction-punished (agent syntax-agent) (cxn fcg-construction))

(define-event type-hierarchy-updated (type-hierarchy type-hierarchy))

;;;;;;;;;;;;;;;;;;;;;;;
;; Trace-Interaction ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-event-handler (trace-interaction starting-interaction)
  ;;(clear-page)
  (add-element '((hr)))
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction))))
  (add-element '((hr)))
  (add-element '((h2) "Participants:"))
  (add-element `((ul)
                 ((li) ,(format nil "Speaker: agent ~a"
                               (id (speaker experiment))))
                 ((li) ,(format nil "Hearer agent ~a"
                               (id (hearer experiment))))))
  ;(add-element '((hr)))
  )

(define-event-handler (trace-interaction scene-perceived)
  (add-element '((h2) "Scene:"))
  (add-element (make-html scene :expand-initially t))
  ;(add-element '((hr)))
  )

(define-event-handler (trace-interaction topic-selected)
  (add-element '((h2) "Topic:"))
  (add-element (make-html topic :expand-initially t))
  ;(add-element '((hr)))
  )

(define-event-handler (trace-interaction conceptualization-finished)
  (add-element '((h2) "Discriminatory Features"))
  (add-element (make-html topic :expand-initially t
                          :discriminatory-features (mapcar #'(lambda (obj) (mapcar #'car obj)) discriminatory-features-objects)))
  (add-element '((h2) "Conceptualized Meaning"))
  (add-element (predicate-network->svg meaning-predicates :only-variables nil)))

(define-event-handler (trace-interaction learning-speaker-started)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "The speaker needs to invent."))))

(define-event-handler (trace-interaction utterance-passed)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "Passing Utterance \"~{~a ~}\" from Speaker to Hearer" utterance))))

(define-event-handler (trace-interaction interpretation-finished)
  (add-element '((h2) "Interpretation: Possible Topics"))
  (if (listp possible-topics)
    (loop for topic in possible-topics
          do
          (add-element (make-html topic :expand-initially t)))
    (add-element `((p) ,possible-topics))))

(define-event-handler (trace-interaction learning-hearer-started)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "The hearer learns."))))

(define-event-handler (trace-interaction alignment-started)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "The ~a Aligns." (discourse-role agent)))))

(define-event-handler (trace-interaction construction-rewarded)
  (add-element '((p) "Rewarded: "))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction type-hierarchy-rewarded)
  (add-element '((p) "Rewarded: "))
  (loop with th = (make-instance 'type-hierarchy)
        for (lex-cat . gramm-cat) in th-links
        do
        (add-categories (list lex-cat gramm-cat) th)
        (add-link lex-cat gramm-cat th :weight (link-weight lex-cat gramm-cat (get-type-hierarchy (grammar agent))))
        finally (add-element (make-html th :weights? t))))

(define-event-handler (trace-interaction type-hierarchy-punished)
  (add-element '((p) "Punished: "))
  (loop with th = (make-instance 'type-hierarchy)
        for (lex-cat . gramm-cat) in th-links
        do
        (add-categories (list lex-cat gramm-cat) th)
        (add-link lex-cat gramm-cat th :weight (link-weight lex-cat gramm-cat (get-type-hierarchy (grammar agent))))
        finally (add-element (make-html th :weights? t))))

(define-event-handler (trace-interaction construction-punished)
  (add-element '((p) "Punished: "))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction finishing-interaction)
  (add-element
   `((h2) "Interaction "
     ,(if (communicated-successfully interaction)
        `((b :style "color:green") "succeeded")
        `((b :style "color:red") "failed")))))

(define-event-handler (trace-interaction type-hierarchy-updated)
 (add-element '((p) "Updated Type Hierarchy: "))
 (add-element (make-html type-hierarchy :weights? t)))

