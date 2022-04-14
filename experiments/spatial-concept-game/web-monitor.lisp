(in-package :spatial-concepts)

;;;; Show lexicon in web interface
(defun display-lexicon (agent)
  (loop for concept in (lexicon agent)
        do (add-element
            `((div)
              ,(s-dot->svg
                (concept->s-dot concept))))
        do (add-element '((hr))))
  (length (lexicon agent)))

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
  (add-element
   `((h1) ,(format nil "Interaction ~a"
                   (interaction-number interaction))))
  (add-element
   `((h2) ,(format nil "The ~a is the speaker"
                   (downcase (mkstr (id (speaker interaction)))))))
  (add-element
   `((h2) ,(format nil "The ~a is the hearer"
                   (downcase (mkstr (id (hearer interaction)))))))
  (add-element '((hr))))


(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element '((hr)))
  (add-element
   `((h2) "Interaction "
     ,(if (communicated-successfully interaction)
        `((b :style "color:green") "succeeded")
        `((b :style "color:red") "failed")))))


(define-event-handler (trace-interaction-in-web-interface context-determined)
  (add-element
   `((table)
     ((tr) ((th) "CLEVR context"))
     ((tr) ((td) ,(make-html (get-data (speaker experiment) 'tutor-context)
                             :expand-initially t)))))
  (add-element
   `((table)
     ((tr) ((th) "MWM context"))
     ((tr) ((td) ,(make-html (get-data (speaker experiment) 'context)))))))


(define-event-handler (trace-interaction-in-web-interface conceptualisation-finished)
  (add-element
   `((h2)
     ,(format nil "The topic is ~a (~a)"
              (id (find-data agent 'topic))
              (id (find-data agent 'tutor-topic)))))
  (cond
   ;; tutor
   ((and (tutorp agent) (find-data agent 'tutor-conceptualisation))
    (add-element
     '((h2) "Tutor points to object:"))
    (add-element
     `((h3) ((i) ,(format nil "~a"
                          (id (pointed-object agent)))))))
   ;; learner
   ((and (learnerp agent) (find-data agent 'applied-concept))
    (add-element
     '((h2) "Learner found a discriminating concept:"))
    (add-element
     `((div)
       ,(s-dot->svg
         (concept->s-dot (find-data agent 'applied-concept))))))
   ;; failed
   (t
    (add-element
     `((h2) ,(format nil "~@(~a~) did not find discriminating spatial relation"
                     (id agent)))))))


(define-event-handler (trace-interaction-in-web-interface production-finished)
  (if (utterance agent)
    (progn
      (add-element
       `((h2) ,(format nil "~@(~a~) producted an utterance:" (id agent))))
      (add-element
       `((h3) ((i) ,(format nil "\"~a\"" (utterance agent))))))
    (add-element
     `((h2) ,(format nil "~@(~a~) could not produce an utterance" (id agent))))))


(define-event-handler (trace-interaction-in-web-interface parsing-finished)
  (when (learnerp agent)
    (if (find-data agent 'applied-concept)
      (progn (add-element '((h2) "The learner parsed the utterance:"))
        (add-element
         `((div)
           ,(s-dot->svg
             (concept->s-dot (find-data agent 'applied-concept))))))
      (add-element
       '((h2) "The learner could not parse the utterance.")))))


(define-event-handler (trace-interaction-in-web-interface interpretation-finished)
  (if (find-data agent 'interpreted-topic)
    (progn
      (add-element
       `((h2) ,(format nil "The ~a interpreted the utterance:"
                       (downcase (mkstr (id agent))))))
      (add-element (make-html (find-data agent 'interpreted-topic) :expand-initially t)))
    (add-element
     `((h2) ,(format nil "The ~a could not interpret the utterance."
                     (downcase (mkstr (id agent))))))))


(define-event-handler (trace-interaction-in-web-interface adopt-concept-started)
  (add-element
   `((h2) ,(format nil "Learner will adopt a new concept for the word \"~a\"" word))))


(define-event-handler (trace-interaction-in-web-interface align-concept-started)
  (add-element
   `((h2) ,(format nil "Learner will align the concept for the word \"~a\"" word))))


(define-event-handler (trace-interaction-in-web-interface new-concept-added)
  (add-element
   '((h2) "A new concept was created:"))
  (add-element
   `((div) ,(s-dot->svg
             (concept->s-dot concept)))))


(define-event-handler (trace-interaction-in-web-interface scores-updated)
  (add-element
   `((h2) ,(format nil "Attributes rewarded and punished for \"~a\""
                   (form concept))))
  (add-element
   `((div) ,(s-dot->svg
             (concept->s-dot concept
                             :highlight-green rewarded-attrs
                             :highlight-red punished-attrs)))))


(define-event-handler (trace-interaction-in-web-interface attribute-removed)
  (add-element
   `((h2) ,(format nil "Removed attribute ~a from word \"~a\""
                   attribute (form concept)))))


(define-event-handler (trace-interaction-in-web-interface concept-removed)
  (add-element
   `((h2) ,(format nil "Removed concept \"~a\""
                   (form concept)))))


(define-event-handler (trace-interaction-in-web-interface found-discriminating-attributes)
  (add-element
   `((h3) ,(format nil "The following attributes are discriminating: ~{\"~a\"~^, ~}"
                   (reverse attributes)))))

(define-event-handler (trace-interaction-in-web-interface found-subset-to-reward)
  (add-element
   `((h3) ,(format nil "The agent will reward the following subset: ~{\"~a\"~^, ~}"
                   (reverse (mapcar #'attribute subset))))))
