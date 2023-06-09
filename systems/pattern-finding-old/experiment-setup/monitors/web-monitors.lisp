;;;; web-monitors.lisp

(in-package :pattern-finding-old)

;; ------------
;; + Monitors +
;; ------------

(define-monitor trace-interactions-in-wi)
(define-monitor summarize-results-after-n-interactions)
(define-monitor evaluation-after-n-interactions)
(define-monitor show-type-hierarchy-after-n-interactions)
(define-monitor trace-grammar-learning-repairs-in-wi)

;; -------------------------------------------------
;; + trace-grammar-learning-repairs-in-wi handlers +
;; -------------------------------------------------

(define-event-handler (trace-grammar-learning-repairs-in-wi fix-applied)
  (add-element
   `((h4) ,(format nil "Applied repair: ~a with form: \"~{~a~^ ~}\" and learned: ~{~a~^; ~}"
                   repair-name (render form :render-string-meets)
                   (mapcar #'(lambda (cxn) (attr-val cxn :bare-cxn-name)) learned-cxns)))))


;; -------------------------------------
;; + trace-interactions-in-wi handlers +
;; -------------------------------------
  
(define-event-handler (trace-interactions-in-wi loading-corpus-started)
  (add-element `((h1) ,(format nil "Loading corpus..."))))

(define-event-handler (trace-interactions-in-wi loading-corpus-finished)
  (add-element `((h1) ,(format nil "Corpus loaded!")))
  (add-element '((hr))))

(define-event-handler (trace-interactions-in-wi interaction-started)
  (add-element `((h1) ,(format nil "Observation ~a" (interaction-number interaction)))))

(define-event-handler (trace-interactions-in-wi interaction-before-finished)
    (add-element `((h3) ,(format nil "Utterance: ") ,(format nil "\"~a\"" utterance)))
    (add-element `((h3) ,(format nil "Meaning: ")))
    (add-element (predicate-network->svg gold-standard-meaning)))

(define-event-handler (trace-interactions-in-wi cipn-statuses)
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

(define-event-handler (trace-interactions-in-wi alignment-started)
  (add-element '((h2) "Alignment started")))

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


;; --------------------------------------------
;; + evaluation-after-n-interactions handlers +
;; --------------------------------------------

(define-event-handler (evaluation-after-n-interactions interaction-finished)
  (when (or (= (mod (interaction-number interaction)
                    (get-configuration experiment :result-display-interval)) 0)
            (= (interaction-number interaction) (length (corpus experiment))))
    (let ((overall-success (get-overall-success experiment)))
      (add-element `((h1) ,(format nil  "Interaction: ~a" (interaction-number interaction))))
      (add-element `((h3) ,(format nil  "Overall accuracy: ~,2f%" overall-success)))
      (add-element '((hr))))))


;; ---------------------------------------------------
;; + summarize-results-after-n-interactions handlers +
;; ---------------------------------------------------

(define-event-handler (summarize-results-after-n-interactions interaction-finished)
  (when (or (= (mod (interaction-number interaction)
                    (get-configuration experiment :result-display-interval)) 0)
            (= (interaction-number interaction) (length (corpus experiment))))
    
    (let* ((windowed-success (get-windowed-success experiment))
           (overall-success (get-overall-success experiment))
           (grammar (grammar (first (interacting-agents experiment))))
           (num-th-nodes (nr-of-categories grammar))
           (num-th-edges (nr-of-links grammar))
           (num-hol (count-non-zero-holophrases grammar))
           (grammar-size (count-if #'non-zero-cxn-p (constructions grammar))))
      (add-element `((h1) ,(format nil  "Interaction: ~a" (interaction-number interaction))))
      (add-element `((h3) ,(format nil  "Windowed accuracy: ~,2f%" windowed-success)))
      (add-element `((h3) ,(format nil  "Overall accuracy: ~,2f%" overall-success)))
      (add-element `((h3) ,(format nil  "Grammar size: ~a" grammar-size)))
      (add-element `((h3) ,(format nil  "Holophrases: ~a" num-hol)))
      (add-element `((h3) ,(format nil  "Categories: ~a" num-th-nodes)))
      (add-element `((h3) ,(format nil  "Categorial links: ~a" num-th-edges)))
      (add-element (make-html (grammar (first (interacting-agents experiment)))
                              :sort-by-type-and-score t
                              :hide-zero-scored-cxns nil
                              :routine-only t))
      (add-element '((hr))))))

;; -----------------------------------------------------
;; + show-type-hierarchy-after-n-interactions handlers +
;; -----------------------------------------------------

(define-event-handler (show-type-hierarchy-after-n-interactions interaction-finished)
  (when (= (mod (interaction-number interaction)
                (get-configuration experiment :result-display-interval)) 0)
    (add-element (make-html (categorial-network (grammar (first (interacting-agents experiment)))) :weights? t :render-program "fdp"))
    (add-element '((hr)))))



