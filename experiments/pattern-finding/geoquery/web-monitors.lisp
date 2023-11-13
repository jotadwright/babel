(in-package :pf-for-sql)

(define-monitor trace-interactions-in-wi)

;; -------------------------------------
;; + trace-interactions-in-wi handlers +
;; -------------------------------------

(define-event-handler (trace-interactions-in-wi interaction-started)
  (add-element `((h1) ,(format nil "Observation ~a" (interaction-number interaction)))))

(define-event-handler (trace-interactions-in-wi interaction-before-finished)
    (add-element `((h3) ,(format nil "Utterance: ") ,(format nil "\"~a\"" utterance)))
    (add-element `((h3) ,(format nil "Meaning: ")))
    (add-element (predicate-network->svg gold-standard-meaning :only-variables nil :extensional-meanings nil)))

#|(define-event-handler (trace-interactions-in-wi cipn-statuses)
  (add-element '((h3) "CIPN statuses:"))
  (add-element (html-pprint cipn-statuses)))

(define-event-handler (trace-interactions-in-wi constructions-chosen)
  (add-element '((h3) "Applied cxns:"))
  (add-cxns-to-wi constructions))

(define-event-handler (trace-interactions-in-wi interaction-finished)
  (let* ((windowed-success (get-windowed-success experiment))
         (overall-success (get-overall-success experiment))
         (grammar (grammar (first (interacting-agents experiment))))
         (grammar-size (count-if #'non-zero-cxn-p (constructions grammar)))
         (num-th-nodes (nr-of-categories grammar))
         (num-th-edges (nr-of-links grammar)))
    (add-element `((h3) ,(format nil  "Windowed accuracy: ~,2f%" windowed-success)))
    (add-element `((h3) ,(format nil  "Overall accuracy: ~,2f" overall-success)))
    (add-element `((h3) ,(format nil  "Grammar size: ~a" grammar-size)))
    (add-element `((h3) ,(format nil  "Categories: ~a" num-th-nodes)))
    (add-element `((h3) ,(format nil  "Categorial links: ~a" num-th-edges)))
    (add-element `((h3) "Communicative success: "
                   ,(if (communicated-successfully interaction)
                      `((b :style "color:green") "yes")
                      `((b :style "color:red") "no"))))
    (add-element '((hr)))))

(define-event-handler (trace-interactions-in-wi cxns-learned)
  (add-element `((h3) ,(format nil "The following cxns were learned (~a):" (length cxns))))
  (add-cxns-to-wi cxns))

(define-event-handler (trace-interactions-in-wi cxns-rewarded)
  (when cxns
    (add-element '((h3) "The following cxns are rewarded:"))
    (add-cxns-to-wi cxns)))

(define-event-handler (trace-interactions-in-wi cxns-punished)
  (when cxns
    (add-element '((h3) "The following cxns are punished:"))
    (add-cxns-to-wi cxns)))

(define-event-handler (trace-interactions-in-wi links-added)
  (when links
    (add-element '((h3) "The following links were added to the categorial network:"))
    (add-element (html-pprint links))
    (add-element
     `((div)
       ,(s-dot->svg
         (categorial-links->s-dot links))))))
|#