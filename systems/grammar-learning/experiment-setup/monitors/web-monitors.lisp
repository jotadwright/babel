;;;; web-monitors.lisp

(in-package :grammar-learning)


;;;;;; EVENTS ;;;;;;


(define-event log-parsing-finished
  (agent clevr-learning-agent)
  (process-result-data list))


(defun new-th-links->s-dot (type-hierarchy new-links)
  (let* ((g (graph type-hierarchy))
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
(define-monitor evaluation-after-n-interactions)
(define-monitor show-type-hierarchy-after-n-interactions)


(define-event-handler (trace-interactions-in-wi challenge-level-questions-loaded)
  (add-element `((h1) ,(format nil "Level ~a questions loaded"
                               level))))

(define-event-handler (trace-interactions-in-wi interaction-started)
  (add-element `((h1) ,(format nil "Observation ~a"
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
         (accuracy (* 100 (float ( / overall-success (interaction-number interaction)))))
         (grammar (grammar (first (interacting-agents experiment))))
         (grammar-size (count-if #'non-zero-cxn-p (constructions grammar)))
         (num-th-nodes (categories (categorial-network grammar)))
         (num-th-edges (links (categorial-network grammar))))
    (add-element `((h3) ,(format nil  "Windowed accuracy: ~a%" windowed-success)))
    (add-element `((h3) ,(format nil  "Overall accuracy: ~a" accuracy)))
    (add-element `((h3) ,(format nil  "Grammar size: ~a" grammar-size)))
    (add-element `((h3) ,(format nil  "Type hierarchy nodes: ~a" num-th-nodes)))
    (add-element `((h3) ,(format nil  "Type hierarchy edges: ~a" num-th-edges)))
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
           (grammar-size (count-if #'non-zero-cxn-p (constructions grammar))))
      (add-element `((h1) ,(format nil  "Interaction: ~a" (interaction-number interaction))))
      (add-element `((h3) ,(format nil  "Windowed accuracy: ~a%" windowed-success)))
      (add-element `((h3) ,(format nil  "Overall accuracy: ~$%" accuracy)))
      (add-element `((h3) ,(format nil  "Grammar size: ~a" grammar-size)))
      (add-element `((h3) ,(format nil  "Type hierarchy nodes: ~a" num-th-nodes)))
      (add-element `((h3) ,(format nil  "Type hierarchy edges: ~a" num-th-edges)))
      (add-element (make-html (grammar (first (interacting-agents experiment))) :sort-by-type-and-score t :hide-zero-scored-cxns t))
      (add-element '((hr))))))

(define-event-handler (show-type-hierarchy-after-n-interactions interaction-finished)
  (when (= (mod (interaction-number interaction)
                (get-configuration experiment :result-display-interval)) 0)
    (add-element (make-html (categorial-network (grammar (first (interacting-agents experiment)))) :weights? t :render-program "fdp"))
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



