(in-package :mwm)

(define-monitor trace-interaction-in-web-interface) ; tiiwi

(define-event-handler (trace-interaction-in-web-interface interaction-started)
  (add-element '((hr)))
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction))))
  (add-element '((hr))))

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (if (communicated-successfully interaction)
                                 "succeeded" "failed")))))

(define-event-handler (trace-interaction-in-web-interface context-generated)
  (add-element '((h2) "Current context:"))
  (add-element (make-html context :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-finished)
  (add-element '((h2) "Speaker found discriminating categories:"))
  (loop for category in discriminating-categories
        do (add-element (make-html category :expand-initially t))))

(define-event-handler (trace-interaction-in-web-interface production-finished)
  (add-element '((h2) "Speaker tries to produce:"))
  (loop for lex in applied-lex
        do (add-element (make-html lex :expand-initially t))))

(define-event-handler (trace-interaction-in-web-interface re-entrance-finished)
  (if success 
    (add-element '((h2) "Speaker re-entrance succeeded"))
    (add-element '((h2) "Speaker re-entrance failed"))))

(define-event-handler (trace-interaction-in-web-interface invention-finished)
  (add-element '((h2) "Speaker added new categories:"))
  (loop for cat in new-categories
        do (add-element (make-html cat :expand-initially t)))
  (add-element '((h2) "Speaker added new word:"))
  (add-element (make-html new-lex :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface interpretation-finished)
  (add-element '((h2) "Hearer interpreted the utterance"))
  (add-element (make-html topic :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface adoption-finished)
  (add-element '((h2) "Hearer adopted a new word:"))
  (add-element (make-html new-lex :expand-initially t)))