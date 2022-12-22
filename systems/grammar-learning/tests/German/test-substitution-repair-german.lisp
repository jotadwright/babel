(ql:quickload :grammar-learning) 
(in-package :grammar-learning)

;; full logging
(progn
  (deactivate-all-monitors)
  (activate-monitor display-metrics)
  (activate-monitor trace-fcg)
  (activate-monitor print-a-dot-for-each-interaction)
  (activate-monitor summarize-results-after-n-interactions)
  (activate-monitor show-type-hierarchy-after-n-interactions)
  (activate-monitor trace-interactions-in-wi))


(defun test-substitution-repair-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Die junge Frau ist beim Arzt"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '(((sein-01 ?s)
                                              (young ?y)
                                              (woman ?w)
                                              (mod ?w ?y)
                                              (doctor ?d)
                                              (arg1 ?s ?w)
                                              (arg2 ?s ?d)
                                              (topicalized ?w))
                                             ))
    
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "Die junge Frau ist beim Baecker"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '(((sein-01 ?s)
                                              (young ?y)
                                              (woman ?w)
                                              (mod ?w ?y)
                                              (baker ?b)
                                              (arg1 ?s ?w)
                                              (arg2 ?s ?b)
                                              (topicalized ?w))
                                             )))))))


(test-substitution-repair-comprehension-german)