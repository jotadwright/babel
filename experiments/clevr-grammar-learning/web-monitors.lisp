;;;; web-monitors.lisp

(in-package :clevr-grammar-learning)


;;;;;; EVENTS ;;;;;;


(define-event log-parsing-finished
  (agent clevr-learning-agent)
  (process-result-data list))


(defun new-th-links->s-dot (type-hierarchy new-links)
  (let* ((g (type-hierarchies::graph type-hierarchy))
         (graph-properties '((s-dot::fontcolor "#000000")
                             (s-dot::fontsize "10.0")
                             (s-dot::fontname "Helvetica")
                             (s-dot::rankdir "LR")))
         (all-node-names
          (remove-duplicates
           (loop for (from . to) in new-links
                 append (list from to))))
         (all-node-ids
          (loop for node-name in all-node-names
                for id = (gethash node-name (graph-utils::nodes g))
                collect id))
         (all-edges
          (loop for (from . to) in new-links
                collect (cons (gethash from (graph-utils::nodes g))
                              (gethash to (graph-utils::nodes g)))))
         (s-dot-nodes
          (loop for node-name in all-node-names
                for node-id in all-node-ids
                collect (graph-utils::type-hierarchy-node->s-dot
                         node-name node-id)))
         (s-dot-edges
          (loop for (from-id . to-id) in all-edges
                for edge-weight = (graph-utils::edge-weight g from-id to-id)
                collect (graph-utils::type-hierarchy-edge->s-dot
                         from-id to-id
                         :weight edge-weight :directedp nil
                         :colored-edges-0-1 nil))))
    `(s-dot::graph ,graph-properties
                   ,@s-dot-nodes
                   ,@s-dot-edges)))





(define-monitor trace-interactions-in-wi)
(define-monitor summarize-results-after-n-interactions)
(define-monitor show-type-hierarchy-after-n-interactions)


(define-event-handler (trace-interactions-in-wi challenge-level-questions-loaded)
  (add-element `((h1) ,(format nil "Level ~a questions loaded"
                               level))))

(define-event-handler (trace-interactions-in-wi interaction-started)
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction)))))

(define-event-handler (trace-interactions-in-wi interaction-before-finished)
    (add-element `((h3) ,(format nil "Utterance: ") ,(format nil "\"~a\"" utterance)))
    (add-element `((h3) ,(format nil "Meaning: ")))
    (add-element (make-html gold-standard-meaning)))

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
         (grammar-size (hash-table-count (cxn-pathnames (grammar (first (interacting-agents experiment))))))
         (consistency-checksum (- (interaction-number interaction) overall-success grammar-size)))

    (add-element `((h3) ,(format nil  "Windowed success: ~a%" windowed-success)))
    (add-element `((h3) ,(format nil  "Overall success: ~a" overall-success)))
    (add-element `((h3) ,(format nil  "Grammar size: ~a" grammar-size)))
    (add-element `((h3) ,(format nil  "Consistency checksum: ~a" consistency-checksum)))

    (add-element `((h3) "Communicative success: "
                   ,(if (communicated-successfully interaction)
                      `((b :style "color:green") "yes")
                      `((b :style "color:red") "no"))))
    (add-element '((hr)))))

(define-event-handler (summarize-results-after-n-interactions interaction-finished)
  (when (= (mod (interaction-number interaction)
                (get-configuration experiment :result-display-interval)) 0)
    
    (let* ((windowed-success (* 100 (float (average (subseq (success-buffer experiment)
                                                            (if (> (- (length (success-buffer experiment)) 100) -1) (- (length (success-buffer experiment)) 100) 0)
                                                            (length (success-buffer experiment)))))))
           (overall-success (count 1 (success-buffer experiment)))
           (grammar-size (hash-table-count (cxn-pathnames (grammar (first (interacting-agents experiment)))))))
      (add-element `((h1) ,(format nil  "Interaction: ~a" (interaction-number interaction))))
      (add-element `((h3) ,(format nil  "Windowed success: ~a%" windowed-success)))
      (add-element `((h3) ,(format nil  "Overall success: ~a" overall-success)))
      (add-element `((h3) ,(format nil  "Grammar size: ~a" grammar-size)))
      (add-element (make-html (grammar (first (interacting-agents experiment)))))
      (add-element '((hr))))))

(define-event-handler (show-type-hierarchy-after-n-interactions interaction-finished)
  (when (= (mod (interaction-number interaction)
                (get-configuration experiment :result-display-interval)) 0)
    (add-element (make-html (get-type-hierarchy (grammar (first (interacting-agents experiment)))) :weights? t :render-program "circo"))
    (add-element '((hr)))))

(define-event-handler (trace-interactions-in-wi alignment-started)
  (add-element '((h2) "Alignment started")))

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

#|
(define-event-handler (trace-interactions-in-wi add-holophrase-new-cxn)
  (add-element '((h3) "New holophrase construction:"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interactions-in-wi item-based->lexical-repair-started)
  (add-element '((h2) "Parsing failed. Composing a new program using the partial program")))

(define-event-handler (trace-interactions-in-wi item-based->lexical-new-cxn-and-th-links)
  (add-element '((h3) "New lexical construction:"))
  (add-element (make-html cxn))
  (add-element '((h3) "New links are added to the type hierarchy:"))
  (add-element
   `((div) ,(s-dot->svg
             (new-th-links->s-dot th new-links)))))

(define-event-handler (trace-interactions-in-wi lexical->item-based-repair-started)
  (add-element '((h2) "Parsing failed. Composing a new program using the partial program")))

(define-event-handler (trace-interactions-in-wi lexical->item-based-new-cxn-and-links)
  (add-element '((h3) "New item-based construction:"))
  (add-element (make-html cxn))
  (add-element '((h3) "New links are added to the type hierarchy:"))
  (add-element
   `((div) ,(s-dot->svg
             (new-th-links->s-dot th new-links)))))

(define-event-handler (trace-interactions-in-wi add-th-links-repair-started)
  (add-element '((h2) "Parsing failed. Adding type hierarchy links")))

(define-event-handler (trace-interactions-in-wi add-th-links-new-th-links)
  (add-element '((h3) "New links are added to the type hierarchy:"))
  (add-element
   `((div) ,(s-dot->svg
             (new-th-links->s-dot th new-links)))))

(define-event-handler (trace-interactions-in-wi interpretation-succeeded)
  (add-element '((h2) "Interpretation succeeded"))
  (add-element '((h3) "Computed answer:"))
  (if (subtypep (type-of answer) 'entity)
    (add-element (make-html answer))
    (add-element `((p) ,(format nil "\"~a\"" answer)))))



(define-event-handler (trace-interactions-in-wi agent-confidence-level)
  (add-element `((h2) ,(format nil "The agent is ~,2f% confident"
                               (* 100.0 level)))))



(define-event-handler (trace-interactions-in-wi holophrase->item-based-substitution-repair-started)
  (add-element '((h2) "Generalising over the grammar (substitution)")))

(define-event-handler (trace-interactions-in-wi holophrase->item-based-subsititution-new-cxn-and-th-links)
  (add-element '((h3) "New constructions are created:"))
  (loop for cxn in new-cxns
        do (add-element (make-html cxn)))
  (add-element '((h3) "New links are added to the type hierarchy:"))
  (add-element
   `((div) ,(s-dot->svg
             (new-th-links->s-dot th new-links)))))

(define-event-handler (trace-interactions-in-wi holophrase->item-based-addition-repair-started)
  (add-element '((h2) "Generalising over the grammar (addition)")))

(define-event-handler (trace-interactions-in-wi holophrase->item-based-addition-new-cxn-and-th-links)
  (add-element '((h3) "New constructions are created:"))
  (loop for cxn in new-cxns
        do (add-element (make-html cxn)))
  (add-element '((h3) "New links are added to the type hierarchy:"))
  (add-element
   `((div) ,(s-dot->svg
             (new-th-links->s-dot th new-links)))))

(define-event-handler (trace-interactions-in-wi holophrase->item-based-deletion-repair-started)
  (add-element '((h2) "Generalising over the grammar (deletion)")))

(define-event-handler (trace-interactions-in-wi holophrase->item-based-deletion-new-cxn-and-th-links)
  (add-element '((h3) "New constructions are created:"))
  (loop for cxn in new-cxns
        do (add-element (make-html cxn)))
  (add-element '((h3) "New links are added to the type hierarchy:"))
  (add-element
   `((div) ,(s-dot->svg
             (new-th-links->s-dot th new-links)))))

(define-event-handler (trace-interactions-in-wi make-hypotheses-repair-started)
  (add-element '((h2) "Making hypotheses")))

(define-event-handler (trace-interactions-in-wi make-hypotheses-new-cxns-and-th-links)
  (add-element '((h3) "New constructions are created:"))
  (loop for cxn in new-cxns
        do (add-element (make-html cxn)))
  (add-element '((h3) "New links are added to the type hierarchy:"))
  (add-element
   `((div) ,(s-dot->svg
             (new-th-links->s-dot th new-links)))))

(define-event-handler (trace-interactions-in-wi item-based+lexical->item-based-repair-started)
  (add-element '((h2) "Generalising over the grammar")))

(define-event-handler (trace-interactions-in-wi item-based+lexical->item-based-new-cxns-and-th-links)
  (add-element '((h3) "New constructions are created:"))
  (loop for cxn in new-cxns
        do (add-element (make-html cxn)))
  (add-element '((h3) "New links are added to the type hierarchy:"))
  (add-element
   `((div) ,(s-dot->svg
             (new-th-links->s-dot th new-links)))))

|#


