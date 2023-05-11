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



(defun test-substitution-repair-comprehension-german ()
  (let* ((experiment (set-up-cxn-inventory-and-repairs-german))
         (cxn-inventory (grammar (first (agents experiment)))))
    (comprehend "Der Doktor verkauft der Frau das Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '(((verkaufen-01 ?v)
                                              (doctor ?d)
                                              (woman ?w)
                                              (book ?b)
                                              (arg0 ?v ?d)
                                              (arg1 ?v ?b)
                                              (arg2 ?v ?w)
                                              (topicalized ?d))
                                             ))
    
    (test-repair-status 'holistic->item-based--substitution
                        (second (multiple-value-list
                                 (comprehend "Der Doktor verkauft dem Clown das Buch"
                    :cxn-inventory cxn-inventory
                    :gold-standard-meaning '(((verkaufen-01 ?v)
                                              (doctor ?d)
                                              (clown ?c)
                                              (book ?b)
                                              (arg0 ?v ?d)
                                              (arg1 ?v ?b)
                                              (arg2 ?v ?c)
                                              (topicalized ?d))
                                             )))))))

(test-substitution-repair-comprehension-german)