(ql:quickload :grammar-learning)
(in-package :grammar-learning)



(progn
  (deactivate-all-monitors)
  ;(activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))

(progn
  (wi::reset)
  (notify reset-monitors)
  (defparameter *experiment*
    (make-instance 'grammar-learning-experiment
                   :entries '((:observation-sample-mode . :sort-length-ascending)
                              (:meaning-representation . :amr)))))

;(run-interaction *experiment*)
;(comprehend "Ah !" :cxn-inventory (grammar (first (agents *experiment*))))
;(run-series *experiment* 118)

;(add-element (make-html (grammar (first (agents *experiment*)))))


(loop for el in (constructions-list (grammar (first (agents *experiment*))))
      unless (eql (attr-val el :cxn-type) 'holophrase)
      do (format t "~a~%" (name el))
      unless (eql (attr-val el :cxn-type) 'holophrase)
      count el)

(defun summarise-metrics (experiment interaction)  
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
    (add-element '((hr)))))

;(summarise-metrics *experiment* (first (interactions *experiment*)))