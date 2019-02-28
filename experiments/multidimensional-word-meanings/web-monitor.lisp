(in-package :mwm)

;; ---------
;; + TIIWI +
;; ---------

(define-monitor trace-interaction-in-web-interface)

(define-event-handler (trace-interaction-in-web-interface interaction-started)
  (add-element '((hr)))
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction))))
  (add-element `((h2) ,(format nil "The ~a is the speaker"
                               (downcase (mkstr (id (speaker interaction)))))))
  (add-element `((h2) ,(format nil "The ~a is the hearer"
                               (downcase (mkstr (id (hearer interaction)))))))
  (add-element '((hr))))

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element '((hr)))
  (add-element `((h2) "Interaction "
                 ,(if (communicated-successfully interaction)
                    `((b :style "color:green") "succeeded")
                    `((b :style "color:red") "failed")))))

(define-event-handler (trace-interaction-in-web-interface context-determined)
  (add-element
   `((table)
     ((tr)
      ((th) "CLEVR context"))
     ((tr)
      ((td) ,(make-html context :expand-initially t)))))
  ;; this works because there is no noise for the moment!
  (add-element
   `((table)
     ((tr)
      ((th) "MWM context"))
     ((tr)
      ((td) ,(make-html (clevr->mwm context) :expand-initially t))))))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-finished)
  (add-element `((h2) ,(format nil "The topic is ~a" (id (topic agent)))))
  (if (tutorp agent)
    (if (discriminative-set agent)
      (progn (add-element '((h2) "Tutor found discriminating attributes:"))
        (add-element `((h3) ((i) ,(format nil "~{~a~^, ~}" (discriminative-set agent))))))
      (add-element '((h2) "Tutor did not find discriminating attributes.")))
    ;; to do; learner side
    ))

(define-event-handler (trace-interaction-in-web-interface production-finished)
  (if (tutorp agent)
    (if (utterance agent)
      (progn (add-element '((h2) "Tutor produced an utterance:"))
        (add-element `((h3) ((i) ,(format nil "~{\"~a\"~^, ~}" (utterance agent))))))
      (add-element '((h2) "Tutor could not produce an utterance.")))
    ;; to do; learner side
    ))

(defun cxn->s-dot (cxn &optional highlight-green highlight-red)
  (let ((form (attr-val cxn :form))
        (meaning (attr-val cxn :meaning))
        (graph '(((s-dot::margin "0")
                  (s-dot::rankdir "LR")) s-dot::graph)))
    (push
     `(s-dot::node
       ((s-dot::id ,(format nil "~a" form))
        (s-dot::label ,form)
        (s-dot::fontcolor "#AA0000")))
     graph)
    (loop for (attr value certainty) in meaning
          for node-content = `((s-dot::id ,(mkdotstr (downcase (mkstr attr))))
                               (s-dot::label ,(if (listp value)
                                                (format nil "~a~%(~{~,2f~^, ~})"
                                                        (downcase (mkstr attr))
                                                        value)
                                                (format nil "~a~%(~,2f)"
                                                        (downcase (mkstr attr))
                                                        value))))
          when (member attr highlight-green)
          do (setf node-content
                   (append node-content
                           '((s-dot::style "filled")
                             (s-dot::fillcolor "#AAFFAA"))))
          when (member attr highlight-red)
          do (setf node-content
                   (append node-content
                           '((s-dot::style "filled")
                             (s-dot::fillcolor "#AA0000"))))
          do (push
              `(s-dot::node ,node-content)
              graph))
    (loop for (attr value certainty) in meaning
          do (push
              `(s-dot::edge
                ((s-dot::from ,(format nil "~a" form))
                 (s-dot::to ,(mkdotstr (downcase (mkstr attr))))
                 (s-dot::label ,(format nil "~,2f" certainty))))
              graph))
    (reverse graph)))

(defun meaning->s-dot (meaning)
  (let ((graph '(((s-dot::margin "0")
                  (s-dot::rankdir "LR")) s-dot::graph)))
    (push
     `(s-dot::node
       ((s-dot::id ,"root")
        (s-dot::label "")))
     graph)
    (loop for (attr value certainty) in meaning
          for node-content = `((s-dot::id ,(mkdotstr (downcase (mkstr attr))))
                               (s-dot::label ,(if (listp value)
                                                (format nil "~a~%(~{~,2f~^, ~})"
                                                        (downcase (mkstr attr))
                                                        value)
                                                (format nil "~a~%(~,2f)"
                                                        (downcase (mkstr attr))
                                                        value))))                                
          do (push
              `(s-dot::node ,node-content)
              graph))
    (loop for (attr value certainty) in meaning
          do (push
              `(s-dot::edge
                ((s-dot::from ,"root")
                 (s-dot::to ,(mkdotstr (downcase (mkstr attr))))
                 (s-dot::label ,(format nil "~,2f" certainty))))
              graph))
    (reverse graph)))

(define-event-handler (trace-interaction-in-web-interface parsing-finished)
  (if (learnerp agent)
    (if (parsed-meaning agent)
      (progn (add-element '((h2) "Learner parsed the utterance:"))
        (add-element `((div) ,(s-dot->svg
                               (meaning->s-dot (parsed-meaning agent))))))
      (add-element '((h2) "Learner could not parse the utterance.")))
    ;; to do; tutor side
    ))

(define-event-handler (trace-interaction-in-web-interface interpretation-finished)
  (if (learnerp agent)
    (if (topic agent)
      (progn (add-element '((h2) "Learner interpreted the utterance:"))
        (add-element (make-html (topic agent) :expand-initially t)))
      (add-element '((h2) "Learner could not interpret the utterance.")))
    ;; to do; tutor side
    ))

(define-event-handler (trace-interaction-in-web-interface alignment-started)
  (add-element '((hr)))
  (add-element '((h2) "Alignment started"))
  (add-element '((hr))))

(define-event-handler (trace-interaction-in-web-interface adopting-words)
  (add-element '((h2) "Learner will adopt new words:"))
  (add-element `((h3) ((i) ,(format nil "~{\"~a\"~^, ~}" words)))))

(define-event-handler (trace-interaction-in-web-interface aligning-words)
  (add-element '((h2) "Learner will align known words:"))
  (add-element `((h3) ((i) ,(format nil "~{\"~a\"~^, ~}" words)))))

(define-event-handler (trace-interaction-in-web-interface new-cxn-added)
  (add-element '((h2) "A new construction was created:"))
  (add-element `((div) ,(s-dot->svg
                         (cxn->s-dot cxn)))))

(define-event-handler (trace-interaction-in-web-interface attr-value-shifted)
  (if (listp new-value)
    (add-element `((h2) ,(format nil "The protoypical value for ~a in the word \"~a\" was shifted from ~{~,2f~^, ~} to ~{~,2f~^, ~}"
                               attr (attr-val cxn :form) old-value new-value)))
    (add-element `((h2) ,(format nil "The protoypical value for ~a in the word \"~a\" was shifted from ~,2f to ~,2f"
                                 attr (attr-val cxn :form) old-value new-value)))))

(define-event-handler (trace-interaction-in-web-interface scores-updated)
  (add-element `((h2) ,(format nil "Attributes rewarded and punished for \"~a\"" (attr-val cxn :form))))
  (add-element `((div) ,(s-dot->svg
                         (cxn->s-dot cxn
                                     rewarded-attrs
                                     punished-attrs)))))

(define-event-handler (trace-interaction-in-web-interface attr-removed)
  ; cxn, attr
  (add-element `((h2) ,(format nil "Removed attribute ~a from word \"~a\""
                               attr (attr-val cxn :form)))))

(define-event-handler (trace-interaction-in-web-interface cxn-removed)
  ; cxn
  (add-element `((h2) ,(format nil "Removed word \"~a\""
                               (attr-val cxn :form)))))