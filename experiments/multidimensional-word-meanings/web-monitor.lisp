(in-package :mwm)

(define-monitor trace-interaction-in-web-interface) ; tiiwi

(define-event-handler (trace-interaction-in-web-interface interaction-started)
  (add-element '((hr)))
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction))))
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "Agent ~a is the speaker"
                               (id (speaker interaction)))))
  (add-element `((h2) ,(format nil "Agent ~a is the hearer"
                               (id (hearer interaction))))))

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (if (communicated-successfully interaction)
                                 "succeeded" "failed")))))

(define-event-handler (trace-interaction-in-web-interface context-generated)
  (add-element '((h2) "Current context:"))
  (add-element (make-html context :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-finished)
  (if discriminating-categories
    (progn (add-element '((h2) "Speaker found discriminating categories:"))
      (loop for category in discriminating-categories
            do (add-element (make-html category :expand-initially t))))
    (add-element '((h2) "Speaker did not find any discriminating categories"))))

(define-event-handler (trace-interaction-in-web-interface production-finished)
  (if applied-lex
    (progn (add-element '((h2) "Speaker produces:"))
      (loop for lex in applied-lex
            do (add-element (make-html lex :expand-initially t))))
    (add-element '((h2) "Speaker could not produce an utterance"))))

(define-event-handler (trace-interaction-in-web-interface re-entrance-finished)
  (if success 
    (add-element '((h2) "Speaker re-entrance succeeded"))
    (add-element '((h2) "Speaker re-entrance failed"))))

(define-event-handler (trace-interaction-in-web-interface invention-finished)
  (when re-used-categories
    (add-element '((h2) "Speaker re-uses categories:"))
    (loop for cat in re-used-categories
          do (add-element (make-html cat :expand-initially t))))
  (when new-categories
    (add-element '((h2) "Speaker added new categories:"))
    (loop for cat in new-categories
          do (add-element (make-html cat :expand-initially t))))
  (add-element '((h2) "Speaker added new word:"))
  (add-element (make-html new-lex :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface interpretation-finished)
  (if topic
    (progn (add-element '((h2) "Hearer interpreted the utterance"))
      (add-element (make-html topic :expand-initially t)))
    (add-element '((h2) "Hearer could not interpret the utterance"))))

(define-event-handler (trace-interaction-in-web-interface adoption-finished)
  (add-element `((h2) ,(format nil "Hearer adopted a new word: ~a" (form new-lex))))
  (when re-used-categories
    (add-element '((h2) "The hearer re-used categories:"))
    (loop for cat in re-used-categories
          do (add-element (make-html cat :expand-initially t))))
  (when new-categories
    (add-element '((h2) "The hearer made new categories:"))
    (loop for cat in new-categories
          do (add-element (make-html cat :expand-initially t))))
  (make-html new-lex :expand-initially t))

(define-event-handler (trace-interaction-in-web-interface alignment-started)
  (add-element `((h2) ,(format nil "The ~a started alignment"
                               (downcase (mkstr (discourse-role agent)))))))

(define-event-handler (trace-interaction-in-web-interface competitor-punished)
  (add-element '((h2) "The agent punished a form competitor:"))
  (add-element (make-html competitor :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface lex-channels-entrenched)
  (when channels
    (add-element `((h2) ,(format nil "The agent entrenched the following channels for ~a"
                                 (id lex))))
    (add-element `((h2) ,(format nil "~{~a~^, ~}" channels)))))

(define-event-handler (trace-interaction-in-web-interface lex-channels-eroded)
  (when channels
    (add-element `((h2) ,(format nil "The agent eroded the following channels for ~a"
                                 (id lex))))
    (add-element `((h2) ,(format nil "~{~a~^, ~}" channels)))))

(define-event-handler (trace-interaction-in-web-interface meaning-extended)
  (add-element `((h2) ,(format nil "The agent extended the meaning for ~a with the following channels:"
                               (id lex))))
  (add-element `((h2) ,(format nil "~{~a~^, ~}" (mapcar (compose #'channel #'car) categories)))))

(define-event-handler (trace-interaction-in-web-interface channel-removed)
  (add-element `((h2) ,(format nil "Channel ~a is removed from the meaning of ~a"
                               (channel channel) (form lex)))))

(define-event-handler (trace-interaction-in-web-interface lex-removed)
  (add-element `((h2) ,(format nil "The word ~a is removed from the lexicon"
                               (form lex)))))