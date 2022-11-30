;;;; web-monitors.lisp

(in-package :grammar-learning)


;;;;;; EVENTS ;;;;;;


(define-event log-parsing-finished
  (agent clevr-learning-agent)
  (process-result-data list))

(define-monitor trace-interactions-in-wi)
(define-monitor summarize-results-after-n-interactions)
(define-monitor evaluation-after-n-interactions)
(define-monitor show-type-hierarchy-after-n-interactions)

(define-event-handler (trace-interactions-in-wi corpus-utterances-loaded)
  (add-element `((h1) ,(format nil "Corpus utterances loaded"))))

(define-event-handler (trace-interactions-in-wi interaction-started)
  (add-element `((h1) ,(format nil "Observation ~a"
                               (interaction-number interaction)))))

(define-event-handler (trace-interactions-in-wi interaction-before-finished)
    (add-element `((h3) ,(format nil "Utterance: ") ,(format nil "\"~a\"" utterance)))
    (add-element `((h3) ,(format nil "Meaning: ")))
    (add-element (predicate-network->svg gold-standard-meaning)))

(define-event-handler (trace-interactions-in-wi cipn-statuses)
  (add-element '((h3) "CIPN statuses:"))
  (add-element (make-html cipn-statuses)))

(define-event-handler (trace-interactions-in-wi constructions-chosen)
  (add-element '((h3) "Applied cxns:"))
  (loop for cxn in constructions
        do (add-element (make-html cxn))))

(define-event-handler (trace-interactions-in-wi interaction-finished)
  (let* ((windowed-success (* 100 (float (average (subseq (success-buffer experiment)
                                                          (if (> (- (length (success-buffer experiment)) 100) -1) (- (length (success-buffer experiment)) 100) 0)
                                                          (length (success-buffer experiment)))))))
         (overall-success (count 1 (success-buffer experiment)))
         (accuracy (* 100 (float ( / overall-success (interaction-number interaction)))))
         (grammar (grammar (first (interacting-agents experiment))))
         (grammar-size (count-if #'non-zero-cxn-p (constructions grammar)))
         (num-th-nodes (nr-of-categories grammar))
         (num-th-edges (nr-of-links grammar)))
    (add-element `((h3) ,(format nil  "Windowed accuracy: ~a%" windowed-success)))
    (add-element `((h3) ,(format nil  "Overall accuracy: ~a" accuracy)))
    (add-element `((h3) ,(format nil  "Grammar size: ~a" grammar-size)))
    (add-element `((h3) ,(format nil  "Categories: ~a" num-th-nodes)))
    (add-element `((h3) ,(format nil  "Categorial links: ~a" num-th-edges)))
    (add-element `((h3) "Communicative success: "
                   ,(if (communicated-successfully interaction)
                      `((b :style "color:green") "yes")
                      `((b :style "color:red") "no"))))
    (add-element '((hr)))))

(define-event-handler (evaluation-after-n-interactions interaction-finished)
  (when (or (= (mod (interaction-number interaction)
                (get-configuration experiment :result-display-interval)) 0)
            (= (interaction-number interaction) (length (question-data experiment))))
    (let* ((overall-success (count 1 (success-buffer experiment)))
           (accuracy (* 100 (float ( / overall-success (interaction-number interaction))))))
      (add-element `((h1) ,(format nil  "Interaction: ~a" (interaction-number interaction))))
      (add-element `((h3) ,(format nil  "Overall accuracy: ~$%" accuracy)))
      (add-element '((hr))))))


(define-event-handler (summarize-results-after-n-interactions interaction-finished)
  (when (or (= (mod (interaction-number interaction)
                (get-configuration experiment :result-display-interval)) 0)
            (= (interaction-number interaction) (length (question-data experiment))))
    
    (let* ((windowed-success (* 100 (float (average (subseq (success-buffer experiment)
                                                            (if (> (- (length (success-buffer experiment)) 100) -1) (- (length (success-buffer experiment)) 100) 0)
                                                            (length (success-buffer experiment)))))))
           (overall-success (count 1 (success-buffer experiment)))
           (accuracy (* 100 (float ( / overall-success (interaction-number interaction)))))
           (grammar (grammar (first (interacting-agents experiment))))
           (num-th-nodes (nr-of-categories grammar))
           (num-th-edges (nr-of-links grammar))
           (num-hol (count-holophrases grammar))
           (grammar-size (count-if #'non-zero-cxn-p (constructions grammar))))
      (add-element `((h1) ,(format nil  "Interaction: ~a" (interaction-number interaction))))
      (add-element `((h3) ,(format nil  "Windowed accuracy: ~a%" windowed-success)))
      (add-element `((h3) ,(format nil  "Overall accuracy: ~$%" accuracy)))
      (add-element `((h3) ,(format nil  "Grammar size: ~a" grammar-size)))
      (add-element `((h3) ,(format nil  "Holophrases ~a" num-hol)))
      (add-element `((h3) ,(format nil  "Categories: ~a" num-th-nodes)))
      (add-element `((h3) ,(format nil  "Categorial links: ~a" num-th-edges)))
      (add-element (make-html (grammar (first (interacting-agents experiment))) :sort-by-type-and-score t :hide-zero-scored-cxns nil :routine-only t))
      (add-element '((hr))))))

(define-event-handler (show-type-hierarchy-after-n-interactions interaction-finished)
  (when (= (mod (interaction-number interaction)
                (get-configuration experiment :result-display-interval)) 0)
    (add-element (make-html (categorial-network (grammar (first (interacting-agents experiment)))) :weights? t :render-program "fdp"))
    (add-element '((hr)))))

(define-event-handler (trace-interactions-in-wi alignment-started)
  (add-element '((h2) "Alignment started")))


(define-event-handler (trace-interactions-in-wi cxns-learned)
  (add-element `((h3) ,(format nil "The following cxns were learned (~a):" (length cxns))))
  (mapcar #'(lambda (cxn)
              (add-element (make-html cxn)))
          cxns))

(define-event-handler (trace-interactions-in-wi cxns-rewarded)
  (add-element '((h3) "The following cxns are rewarded:"))
  (mapcar #'(lambda (cxn)
              (add-element (make-html cxn)))
          cxns))

(define-event-handler (trace-interactions-in-wi cxns-punished)
  (unless (null cxns)
    (add-element '((h3) "The following cxns are punished:"))
    (mapcar #'(lambda (cxn)
                (add-element (make-html cxn)))
            cxns)))



