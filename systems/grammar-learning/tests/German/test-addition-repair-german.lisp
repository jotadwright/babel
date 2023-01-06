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


(defun test-addition-repair-short-first-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))  ;geo
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Die junge Frau gibt dem Mann den Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w)))
    (test-repair-status 'holistic->item-based--addition
                        (second (multiple-value-list
                                 (comprehend "Die junge Frau gibt dem Mann den gruenen Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (green ?gr)
                                             (mod ?a ?gr)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w)))
                                 )))))

;(test-addition-repair-short-first-comprehension-german)

(defun test-addition-repair-long-first-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))  ;geo
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Die junge Frau gibt dem Mann den gruenen Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (green ?gr)
                                             (mod ?a ?gr)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w)))
    
    (test-repair-status 'holistic->item-based--addition
                        (second (multiple-value-list
                                 (comprehend "Die junge Frau gibt dem Mann den Apfel"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((geben-01 ?g)
                                             (young ?y)
                                             (woman ?w)
                                             (mod ?w ?y)
                                             (man ?m)
                                             (apple ?a)
                                             (arg0 ?g ?w)
                                             (arg1 ?g ?a)
                                             (arg2 ?g ?m)
                                             (topicalized ?w)))
                                 )))))


;(test-addition-repair-long-first-comprehension-german)   in this case its deletion from the cxn 