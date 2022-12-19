(in-package :naming-game)

(define-monitor trace-experiment-wi
                :documentation "Traces some important information about the experiment in the web-interface")

(define-monitor trace-interaction-wi)

(define-event-handler (trace-interaction-wi interaction-started)
  (let* ((interacting-agents (interacting-agents interaction))
         (speaker (first interacting-agents))
         (hearer (second interacting-agents)))
    (add-element
     `((h1) ,(format nil "Interaction ~a"
                     (interaction-number interaction))))
    (add-element
     `((h2) ,(format nil "~a is the speaker"
                     (downcase (mkstr (id speaker))))))
    (add-element
     `((h2) ,(format nil "~a is the hearer"
                     (downcase (mkstr (id hearer))))))
    ))

(define-event-handler (trace-interaction-wi interaction-finished)
  (add-element
   `((h2) "Interaction "
     ,(if (communicated-successfully interaction)
        `((b :style "color:green") "succeeded")
        `((b :style "color:red") "failed"))))
  (add-element '((hr))))

(define-event conceptualisation-finished (speaker naming-game-agent))
  
(define-event-handler (trace-interaction-wi conceptualisation-finished)
  (add-element
   `((h2)
     ,(format nil "Speaker chooses the topic ~a"
              (topic speaker))))
  (add-element
   `((h2)
     ,(format nil "The speaker produces utterance '~a' to refer to the topic." (utterance speaker)))
  ))

(define-event parsing-finished (hearer naming-game-agent))

(define-event-handler (trace-interaction-wi parsing-finished)
  (if (applied-cxn hearer)
    (add-element '((h2) "The learner parsed the utterance."))
    (add-element
       '((h2) "The learner could not parse the utterance."))))

(define-event interpretation-finished (hearer naming-game-agent))

(define-event-handler (trace-interaction-wi interpretation-finished)
  (if (pointed-object hearer)
    (add-element
       `((h2) ,(format nil "The hearer interpreted the utterance and points to: ~a"
                       (downcase (mkstr (topic hearer))))))
    (add-element
     `((h2) ,(format nil "The hearer could not interpret the utterance.")))))

(define-event adoptation-finished (hearer naming-game-agent))

(define-event-handler (trace-interaction-wi adoptation-finished)
  (add-element
   `((h2) ,(format nil "The hearer adopted the word ~a for ~a" (cdr (assoc :form (attributes (applied-cxn hearer)))) (cdr (assoc :meaning (attributes (applied-cxn hearer))))))))

(define-event align-finished)

(define-event-handler (trace-interaction-wi align-finished)
  (add-element
   `((h2) ,(format nil "The agents performed alignment"))))

(defmethod print-vocabulary ((agent agent))
  "prints the lexicon of agent in a nice and readable way"
  (loop for voc-item in (constructions (lexicon agent))
        for voc-attributes = (attributes voc-item)
        for score = (cdr (assoc :score voc-attributes))
        for meaning = (cdr (assoc :meaning voc-attributes))
        for form = (cdr (assoc :form voc-attributes))
        do (when (> score 0)
             (add-element `((h3) ,(format nil "form = ~a, meaning = ~a, score = ~a~%" form meaning score))))))

(define-event-handler (trace-experiment-wi run-series-finished)
  (add-element
   `((h3) ,(format nil "The vocabulary of one agent (agent-1) at the end of the experiment is:")))
  (print-vocabulary (first (agents experiment))))


  
  