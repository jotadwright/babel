(in-package :naming-game)

(define-monitor trace-experiment
                :documentation "Traces some important information about the experiment in the web-interface")


(define-event-handler (trace-interaction interaction-started)
  (let* ((interacting-agents (interacting-agents interaction))
         (speaker (first interacting-agents))
         (hearer (second interacting-agents)))
    (add-element '((hr)))
    (add-element
     `((h1) ,(format nil "Interaction ~a"
                     (interaction-number interaction))))
    (add-element
     `((h2) ,(format nil "~a is the speaker"
                     (downcase (mkstr (id speaker))))))
    (add-element
     `((h2) ,(format nil "~a is the hearer"
                     (downcase (mkstr (id hearer))))))
    (add-element '((hr)))))

(define-event-handler (trace-interaction interaction-finished)
  (add-element '((hr)))
  (add-element
   `((h2) "Interaction "
     ,(if (communicated-successfully interaction)
        `((b :style "color:green") "succeeded")
        `((b :style "color:red") "failed")))))

(define-event conceptualisation-finished (speaker naming-game-agent))
  
(define-event-handler (trace-interaction conceptualisation-finished)
  (add-element
   `((h2)
     ,(format nil "Speaker chooses the topic ~a"
              (topic speaker))))
  (add-element
   `((h2)
     ,(format t "The speaker produces utterance '~a' to refer to the topic." (utterance speaker)))
  ))

(define-event parsing-finished (hearer naming-game-agent))

(define-event-handler (trace-interaction parsing-finished)
  (if (applied-voc hearer)
    (add-element '((h2) "The learner parsed the utterance."))
    (add-element
       '((h2) "The learner could not parse the utterance."))))

(define-event interpretation-finished (hearer naming-game-agent))

(define-event-handler (trace-interaction interpretation-finished)
  (if (pointed-object hearer)
    (add-element
       `((h2) ,(format nil "The hearer interpreted the utterance and points to: ~a"
                       (downcase (mkstr (topic hearer))))))
    (add-element
     `((h2) ,(format nil "The hearer could not interpret the utterance.")))))

(define-event adoptation-finished (hearer naming-game-agent))

(define-event-handler (trace-interaction adoptation-finished)
  (add-element
   `((h2) ,(format nil "The hearer adopted the word ~a for ~a" (form (applied-voc hearer)) (meaning (applied-voc hearer))))))

(define-event align-finished)

(define-event-handler (trace-interaction align-finished)
  (add-element
   `((h2) ,(format nil "The agents performed alignment"))))

(defmethod print-vocabulary ((agent agent))
  "prints the lexicon of agent in a nice and readable way"
  (loop with counter = 0
        for voc-item in (lexicon agent)
        do (incf counter)
           (add-element `((h3) ,(format nil "form = ~a, meaning = ~a, score = ~a~%" (form voc-item)(meaning voc-item)(score voc-item))))))

(define-event-handler (trace-experiment run-series-finished)
  (add-element
   `((h3) ,(format nil "The words used by all the agents at the end of the experiment are:")))
  (print-vocabulary (first (agents experiment))))


  
  