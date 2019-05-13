(in-package :mwm)

;; -----------------
;; + Printing dots +
;; -----------------

(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (if (= (mod (interaction-number interaction)
                (get-configuration experiment :dot-interval)) 0)
    (format t ". (~a)~%" (interaction-number interaction))
    (format t ".")))


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
      ((td) ,(make-html (clevr-context (speaker experiment)) :expand-initially t)))))
  (if (get-configuration experiment :perceptual-deviation)
    (progn (add-element `((table) ((tr) ((th) "MWM context (speaker)"))
                          ((tr) ((td) ,(make-html (context (speaker experiment)))))))
      (add-element `((table) ((tr) ((th) "MWM context (hearer)"))
                          ((tr) ((td) ,(make-html (context (hearer experiment))))))))
    (add-element `((table) ((tr) ((th) "MWM context"))
                          ((tr) ((td) ,(make-html (context (speaker experiment)))))))))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-finished)
  (add-element `((h2) ,(format nil "The topic is ~a" (id (topic agent)))))
  (if (tutorp agent)
    (case (get-configuration agent :tutor-lexicon)
      (:symbolic
       (progn (add-element '((h2) "Tutor found discriminating attributes:"))
         (add-element `((h3) ((i) ,(format nil "~{~a~^, ~}" (discriminative-set agent)))))))
      (:continuous
       (progn (add-element '((h2) "Tutor conceptualised the topic:"))
         (add-element `((h3) ((i) ,(format nil "~{~a~^, ~}" (mapcar #'name (applied-cxns agent))))))))
      (otherwise (add-element '((h2) "Tutor did not find discriminating attributes."))))
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

(defgeneric category->s-dot-node (category)
  (:documentation "How to display a category as an s-dot node"))

(defmethod category->s-dot-node ((category min-max-category))
  `((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
    (s-dot::label ,(format nil "~a~%(~,2f - ~,2f)"
                           (downcase (mkstr (attribute category)))
                           (lower-bound category)
                           (upper-bound category)))))

(defmethod category->s-dot-node ((category prototype-category))
  (let* ((variance (/ (M2 category) (nr-samples category)))
         (stdev (sqrt variance))
         (lower-bound (max (- (prototype category) stdev) 0.0))
         (upper-bound (min (+ (prototype category) stdev) 1.0)))
    `((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
      (s-dot::label ,(format nil "~a (~,2f)~%(~,2f - ~,2f)"
                             (downcase (mkstr (attribute category)))
                             (prototype category)
                             lower-bound
                             upper-bound)))))

(defmethod category->s-dot-node ((category prototype-min-max-category))
  `((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
    (s-dot::label ,(format nil "~a (~,2f)~%(~,2f - ~,2f)"
                           (downcase (mkstr (attribute category)))
                           (prototype category)
                           (lower-bound category)
                           (upper-bound category)))))

#|
(defmethod category->s-dot-node ((category parabola-category))
  `((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
    (s-dot::label ,(format nil "~a~%~,2f~%(~,2f - ~,2f)"
                           (downcase (mkstr (attribute category)))
                           (centre category)
                           (lower-bound category)
                           (upper-bound category)))))
|#

(defmethod category->s-dot-node ((category exponential-category))
  `((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
    (s-dot::label ,(format nil "~a (~,2f)~%(~,2f - ~,2f)"
                           (downcase (mkstr (attribute category)))
                           (prototype category)
                           (left-sigma category)
                           (right-sigma category)))))

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
    (loop for (category . certainty) in meaning
          for node-content = (category->s-dot-node category)
          when (member (attribute category) highlight-green)
          do (setf node-content
                   (append node-content
                           '((s-dot::style "filled")
                             (s-dot::fillcolor "#AAFFAA"))))
          when (member (attribute category) highlight-red)
          do (setf node-content
                   (append node-content
                           '((s-dot::style "filled")
                             (s-dot::fillcolor "#AA0000"))))
          when (> certainty 0.0)
          do (push
              `(s-dot::node ,node-content)
              graph))
    (loop for (category . certainty) in meaning
          when (> certainty 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,(format nil "~a" form))
                 (s-dot::to ,(mkdotstr (downcase (mkstr (attribute category)))))
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
    (loop for (category . certainty) in meaning
          for node-content = (category->s-dot-node category)
          when (> certainty 0.0)
          do (push
              `(s-dot::node ,node-content)
              graph))
    (loop for (category . certainty) in meaning
          when (> certainty 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,"root")
                 (s-dot::to ,(mkdotstr (downcase (mkstr (attribute category)))))
                 (s-dot::label ,(format nil "~,2f" certainty))))
              graph))
    (reverse graph)))

(define-event-handler (trace-interaction-in-web-interface parsing-finished)
  (if (parsed-meaning agent)
    (progn (add-element '((h2) "Agent parsed the utterance:"))
      (add-element `((div) ,(s-dot->svg
                             (meaning->s-dot (parsed-meaning agent))))))
    (add-element '((h2) "Agent could not parse the utterance."))))

(define-event-handler (trace-interaction-in-web-interface interpretation-finished)
  (if (topic agent)
    (progn (add-element '((h2) "Agent interpreted the utterance:"))
      (add-element (make-html (topic agent) :expand-initially t)))
    (add-element '((h2) "Agent could not interpret the utterance."))))

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

(define-event-handler (trace-interaction-in-web-interface re-introduced-meaning)
  (loop for attr in attrs
        do (add-element `((h2) ,(format nil "Re-introduced attribute ~a to word \"~a\""
                                        attr (attr-val cxn :form))))))

(define-event-handler (trace-interaction-in-web-interface cxn-removed)
  ; cxn
  (add-element `((h2) ,(format nil "Removed word \"~a\""
                               (attr-val cxn :form)))))