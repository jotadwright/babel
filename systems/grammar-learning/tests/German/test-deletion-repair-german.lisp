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


(defun test-deletion-repair-comprehension-clevr ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (test-repair-status 'holistic->item-based--deletion
                        (second (multiple-value-list
                                 (comprehend "The gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (unique ?source-9 ?target-2)
                                             (bind shape-category ?shape-8 thing)
                                             (bind attribute-category ?attribute-2 shape)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind color-category ?color-2 gray)
                                             (query ?target-7 ?source-9 ?attribute-2))))))
    (comprehend "The large gray object is what shape?"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '((get-context ?source-1)
                                             (filter ?target-39552 ?target-2 ?size-4)
                                             (unique ?source-10 ?target-39552)
                                             (bind color-category ?color-2 gray)
                                             (filter ?target-1 ?source-1 ?shape-8)
                                             (bind attribute-category ?attribute-2 shape)
                                             (bind shape-category ?shape-8 thing)
                                             (filter ?target-2 ?target-1 ?color-2)
                                             (bind size-category ?size-4 large)
                                             (query ?target-8 ?source-10 ?attribute-2)))
    (add-element (make-html cxn-inventory))))


(defun test-deletion-repair-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
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
    
    (test-repair-status 'holistic->item-based--deletion
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
                                             (topicalized ?w))))))))


(test-deletion-repair-comprehension-german)