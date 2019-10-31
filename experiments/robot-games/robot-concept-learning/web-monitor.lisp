(in-package :robot-concept-learning)

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
  (add-element '((hr))))

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element '((hr)))
  (add-element `((h2) "Interaction "
                 ,(if (communicated-successfully interaction)
                    `((b :style "color:green") "succeeded")
                    `((b :style "color:red") "failed")))))

(define-event-handler (trace-interaction-in-web-interface context-determined)
  (let ((source-path (babel-pathname :directory '(".tmp" "nao-img")
                                     :name image
                                     :type "jpg"))
        (dest-path (pathname "~/Sites/")))
    (copy-file source-path dest-path)
    (if (null cl-user::*localhost-user-dir*)
      (warn "Set your *localhost-user-dir* in init-babel")
      (progn
        (add-element
         `((table)
           ((tr)
            ((th :align "center") "Observed scene:"))
           ((tr)
            ((td :align "center") ((img :src ,(string-append cl-user::*localhost-user-dir* (format nil "~a.jpg" image)) :width "90%"))))))
        (add-element
         `((div) ,(make-html (context (first (population experiment))) :expand-initially t)))))))

(define-event-handler (trace-interaction-in-web-interface utterance-received)
  (add-element `((h2) "The agent received the utterance "
                 ((i) ,(format nil "\"~a\"" utterance)))))

(define-event-handler (trace-interaction-in-web-interface parsing-finished)
  (if (parsed-meaning agent)
    (progn (add-element '((h2) "The agent parsed the utterance:"))
      (add-element `((div) ,(s-dot->svg
                             (meaning->s-dot (parsed-meaning agent))))))
    (add-element '((h2) "The agent could not parse the utterance."))))

(define-event-handler (trace-interaction-in-web-interface interpretation-finished)
  (when (topic agent)
    (add-element '((h2) "The agent interpreted the utterance:"))
    (add-element (make-html (topic agent) :expand-initially t))))

(define-event-handler (trace-interaction-in-web-interface alignment-started)
  (add-element '((hr)))
  (add-element '((h2) "Alignment started")))

(define-event-handler (trace-interaction-in-web-interface adopt-unknown-word)
  (add-element `((h2) "The agent will adopt a new word: "
                 ((i) ,(format nil "\"~a\"" (utterance agent))))))

(define-event-handler (trace-interaction-in-web-interface update-known-word)
  (add-element `((h2) "The agent will align a known word: "
                 ((i) ,(format nil "\"~a\"" (utterance agent))))))

(define-event-handler (trace-interaction-in-web-interface new-cxn-added)
  (add-element '((h2) "A new construction was added:"))
  (add-element `((div) ,(s-dot->svg
                         (cxn->s-dot cxn)))))

(define-event-handler (trace-interaction-in-web-interface scores-updated)
  (add-element `((h2) ,(format nil "Attributes rewarded and punished for \"~a\"" (attr-val cxn :form))))
  (add-element `((div) ,(s-dot->svg
                         (cxn->s-dot cxn
                                     rewarded-attrs
                                     punished-attrs)))))